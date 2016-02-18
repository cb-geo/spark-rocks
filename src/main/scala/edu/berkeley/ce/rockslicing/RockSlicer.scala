package edu.berkeley.ce.rockslicing

import java.io._
import org.apache.spark.{SparkConf, SparkContext}
import scala.io.Source
import scala.annotation.tailrec

object RockSlicer {

  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("SparkRocks")
    val sc = new SparkContext(conf)
    val parsedArgs = CommandReader.parseArguments(args)
    if (parsedArgs.isEmpty) {
      // Error message will already be printed out by CommandReader
      System.exit(-1)
    }
    val arguments = CommandReader.parseArguments(args).get

    // Open and read input file specifying rock volume and joints
    val inputSource = Source.fromFile(arguments.inputFile)
    val inputOpt = InputProcessor.readInput(inputSource)
    inputSource.close()
    if (inputOpt.isEmpty) {
      // Error message will already be printed out by InputProcessor
      System.exit(-1)
    }
    val (globalOrigin, rockVolume, joints) = inputOpt.get
    var blocks = Seq(Block(globalOrigin, rockVolume))
    val processorJoints = if (arguments.numProcessors > 1) {
      LoadBalancer.generateProcessorJoints(blocks.head, arguments.numProcessors)
    } else {
      Seq.empty[Joint]
    }

    // Generate a list of initial blocks before RDD-ifying it
    if (arguments.numProcessors > 1) {
      processorJoints foreach { joint =>
        blocks = blocks.flatMap(_.cut(joint))
      }
    }

    val blockRdd = sc.parallelize(blocks)
    val broadcastJoints = sc.broadcast(joints)

    // Iterate through the discontinuities, cutting blocks where appropriate
    var cutBlocks = blockRdd
    for (joint <- broadcastJoints.value) {
      cutBlocks = cutBlocks.flatMap(_.cut(joint, arguments.minRadius, arguments.maxAspectRatio))
    }

    // Remove geometrically redundant joints
    val nonRedundantBlocks = cutBlocks.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }

    // Find all blocks that contain processor joints
    val processorBlocks = nonRedundantBlocks.filter { block => 
      block.faces.exists(_.processorJoint)
    }

    // Find blocks that do not contain processor joints
    val realBlocks = nonRedundantBlocks.filter { block =>
      !block.faces.exists(_.processorJoint)
    }

    // Process processor blocks before continuing with remaining blocks
    // Search blocks for matching processor joints
    if (processorBlocks.count() > 0) {
      val treeReduceBlockPairsRDD = processorBlocks.map{ blocks => (Seq(blocks), Seq.empty[Block])}
      val (allOrphanBlocks, allReconstructedBlocks) = treeReduceBlockPairsRDD.treeReduce({
        case ((toMerge1, merged1), (toMerge2, merged2)) =>
          val (treeReconBlocks, treeOrphanBlocks) = mergeBlocks(toMerge1 ++ toMerge2, Seq.empty[Block],
                                                                Seq.empty[Block])

          val (treeProcessorBlocks, treeRealBlocks) = treeReconBlocks.partition { block =>
            block.faces.exists(_.processorJoint)
          }

          (treeProcessorBlocks ++ treeOrphanBlocks, treeRealBlocks ++ merged1 ++ merged2)
      }, math.ceil(math.log(arguments.numProcessors)/math.log(2)).toInt)

      assert(allOrphanBlocks.isEmpty)

      // Update centroids of reconstructed processor blocks and remove duplicates
      val reconCentroidBlocks = allReconstructedBlocks.map {block =>
        val centroid = block.centroid
        Block(centroid, block.updateFaces(centroid))
      }

      // Clean up faces of reconstructed blocks with values that should be zero, but have
      // arbitrarily small floating point values
      val squeakyCleanRecon = reconCentroidBlocks.map { case Block(center, faces) =>
        Block(center, faces.map(_.applyTolerance))
      }

      // Convert reconstructed blocks to requested output
      if (arguments.toInequalities) {
        val jsonReconBlocks = squeakyCleanRecon.map(Json.blockToMinimalJson)
        val pw = new PrintWriter(new File("reconstructedBlocks.json"))
        jsonReconBlocks.foreach { block =>
          pw.write(block+"\n")
        }
        pw.close()
      }

      if (arguments.toVTK) {
        val vtkBlocksRecon = squeakyCleanRecon.map(BlockVTK(_))
        val jsonVtkBlocksRecon = vtkBlocksRecon.map(JsonToVtk.blockVtkToMinimalJson)
        val pw = new PrintWriter(new File("vtkReconstructedBlocks.json"))
        jsonVtkBlocksRecon.foreach { block =>
          pw.write(block+"\n")
        }
        pw.close()
      }
    }

    // Process remaining blocks that do not contain processor blocks
    // Calculate centroid of each real block
    val centroidBlocks = realBlocks.map { block =>
      val centroid = block.centroid
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, updatedFaces)
    }

    // Clean up faces of real blocks with values that should be zero, but have arbitrarily
    // small floating point values
    val squeakyClean = centroidBlocks.map { case Block(center, faces) =>
      Block(center, faces.map(_.applyTolerance))
    }

    // Convert list of rock blocks to requested output
    if (arguments.toInequalities) {
      // Convert the list of rock blocks to JSON and save this to a file
      val jsonBlocks = squeakyClean.map(Json.blockToMinimalJson)
      jsonBlocks.saveAsTextFile("blocks.json")
    }

    if (arguments.toVTK) {
      // Convert the list of rock blocks to JSON with vertices, normals and connectivity in
      // format easily converted to vtk by rockProcessor module
      val vtkBlocks = squeakyClean.map(BlockVTK(_))
      val jsonVtkBlocks = vtkBlocks.map(JsonToVtk.blockVtkToMinimalJson)
      jsonVtkBlocks.saveAsTextFile("vtkBlocks.json")
    }
    sc.stop()
  }

  /**
    * Recursive function that merges sequence of blocks containing processor joints
    * along matching processor joints, eliminating false blocks caused by division of
    * input rock volume into equal volumes by load balancer
    * @param processorBlocks Sequence of blocks containing one or more processor joints each
    * @param mergedBlocks Sequence of blocks that have had processor joints removed. Initial
    *                     input is an empty sequence
    * @param orphanBlocks Sequence of blocks that cannot be matched with other blocks. Initial
    *                     input is an empty sequence
    * @return Tuple containing two sequences of blocks. The first sequence is all the merged blocks.
    *         The second is all blocks that could not be merged.
    */
  @tailrec
  def mergeBlocks(processorBlocks: Seq[Block], mergedBlocks: Seq[Block],
                  orphanBlocks: Seq[Block]):
                 (Seq[Block], Seq[Block]) = {
    val blockMatches = findMates(processorBlocks)
    val joinedBlocks = blockMatches.map{ case (paired, _) => paired }
    val matchIndices = blockMatches.flatMap { case (_, indices) => indices}.distinct
    val originalPairedBlocks = matchIndices.collect(processorBlocks)

    val uniqueBlocks = (processorBlocks ++ orphanBlocks).foldLeft(Seq.empty[Block]) { (unique, current) =>
      if (!unique.exists(current.approximateEquals(_))) {
        current +: unique
      } else {
        unique
      }
    }

    val currentOrphanBlocks = uniqueBlocks.diff(originalPairedBlocks)
    val newProcessorBlocks = if (originalPairedBlocks.isEmpty) {
      processorBlocks.tail
    } else {
      processorBlocks.diff(originalPairedBlocks)
    }

    // Check if more than one match was found. If so, merge all matches into one block
    val mergedJoinedBlock = if (joinedBlocks.length > 1) {
      // Note: originalPairedBlocks.head will be equal to processorBlocks.head
      val temporaryOrigin = (originalPairedBlocks.head.centerX,
                             originalPairedBlocks.head.centerY,
                             originalPairedBlocks.head.centerZ)
      val temporaryBlocks = originalPairedBlocks.tail.map { block =>
        Block(temporaryOrigin, block.updateFaces(temporaryOrigin))
      }

      val sharedProcessorFaces = temporaryBlocks.flatMap { block =>
        compareProcessorBlocks(originalPairedBlocks.head, block)
      }
      val allFaces = temporaryBlocks.flatMap { block =>
        block.faces
      }
      // Remove any faces that are shared between blocks
      val nonSharedFaces = allFaces.diff(sharedProcessorFaces)
      val smushedBlock = Block(temporaryOrigin, nonSharedFaces)
      Seq(Block(temporaryOrigin, smushedBlock.nonRedundantFaces))
    } else {
      joinedBlocks
    }

    // Divide blocks into groups with and without processor joints remaining
    val (remainingBlocks, completedBlocks) = mergedJoinedBlock.partition { block =>
      block.faces.exists(_.processorJoint)
    }

    if (remainingBlocks.nonEmpty) {
      // Merged blocks still contain some processor joints
      mergeBlocks(remainingBlocks ++ newProcessorBlocks, completedBlocks ++ mergedBlocks,
        currentOrphanBlocks)
    } else if (newProcessorBlocks.isEmpty) {
      // All blocks are free of processor joints - check for duplicates then return
      (completedBlocks ++ mergedBlocks, currentOrphanBlocks)
    } else {
      // Proceed to next processor block
      mergeBlocks(newProcessorBlocks, completedBlocks ++ mergedBlocks, currentOrphanBlocks)
    }
  }

  /**
    * Compares two input blocks and determines whether they share a processor face
    * @param block1 First input block
    * @param block2 Second input block
    * @return List of processor faces that are shared by the two blocks. Will be 
    *         empty if they share no faces
    */
  def compareProcessorBlocks(block1: Block, block2: Block): Seq[Face] = {
    val processorFaces1 = block1.faces.filter(_.processorJoint)
    val processorFaces2 = block2.faces.filter(_.processorJoint)
    (processorFaces1 flatMap { face1 =>
      processorFaces2 flatMap { face2 =>
        if (face1.isSharedWith(face2)) {
          Seq[Face](face1, face2)
        } else {
          Seq.empty[Face]
        }
      }
    }).distinct
  }

  /**
    * Finds blocks that share a processor joint and appear on opposite sides of the processor joint.
    * @param processorBlocks Seq of blocks to search for mates
    * @return Seq of tuples. The first entry in the tuple is the block generated by merging two mated
    *         blocks. The second entry in the tuple is a Seq containing the indices of the two blocks that
    *         were merged to create the block in the first entry.
    */
  def findMates(processorBlocks: Seq[Block]): Seq[(Block, Seq[Int])] = {
    if (processorBlocks.nonEmpty) {
      processorBlocks.tail flatMap { block =>
        val currentBlock = processorBlocks.head
        val localCenter = (currentBlock.centerX, currentBlock.centerY, currentBlock.centerZ)
        val comparisonBlock = Block(localCenter, block.updateFaces(localCenter))
        val sharedProcFaces = compareProcessorBlocks(currentBlock, comparisonBlock)

        if (sharedProcFaces.nonEmpty) {
          val currentFaces = currentBlock.faces.diff(sharedProcFaces)
          val updatedFaces = comparisonBlock.faces.diff(sharedProcFaces)
          val allFaces = currentFaces ++ updatedFaces
          // Check for any actual shared faces between blocks - if these exists blocks
          // should not be merged since there is a real joint separating the blocks
          val nonSharedFaces =
            allFaces.foldLeft(Seq.empty[Face]) { (unique, current) =>
              if (!unique.exists(current.isSharedWith)) {
                current +: unique
              } else {
                unique
              }
            }

          if (allFaces.diff(nonSharedFaces).isEmpty) {
            val mergedBlock = Block(localCenter, nonSharedFaces)
            val nonRedundantNonSharedFaces = mergedBlock.nonRedundantFaces
            // Filter blocks that actually aren't adjacent at all,
            // but share a processor joint
            nonRedundantNonSharedFaces match {
              case Nil => None
              case x => Some(Block(localCenter, nonRedundantNonSharedFaces),
                             Seq[Int](0, processorBlocks.indexOf(block)))
            }
          } else {
            None
          }
        } else {
          None
        }
      }
    } else {
      Seq((Block((0.0, 0.0, 0.0), Seq.empty[Face]), Seq.empty[Int]))
    }
  }
}