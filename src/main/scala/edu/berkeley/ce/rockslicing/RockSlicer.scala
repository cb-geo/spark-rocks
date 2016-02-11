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
    val broadcastProcJoints = sc.broadcast(processorJoints)

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

    // Process processor blocks before continueing with remaining blocks
    // Search blocks for matching processor joints
    if (processorBlocks.count() > 0) {
      val updatedProcessorBlocks = processorBlocks.map { block =>
        Block(globalOrigin, block.updateFaces(globalOrigin))
      }

      val treeReduceBlockPairsRDD = updatedProcessorBlocks.map{ blocks => (Seq(blocks), Seq.empty[Block])}
      val (allOrphanBlocks, allReconstructedBlocks) = treeReduceBlockPairsRDD.treeReduce({
        case ((toMerge1, merged1), (toMerge2, merged2)) =>
          val (treeReconBlocks, treeOrphanBlocks) = mergeBlocks(toMerge1 ++ toMerge2, Seq.empty[Block],
                                                                Seq.empty[Block])

          val (treeProcessorBlocks, treeRealBlocks) = treeReconBlocks.partition { block =>
            block.faces.exists(_.processorJoint)
          }

          (treeProcessorBlocks ++ treeOrphanBlocks, treeRealBlocks ++ merged1 ++ merged2)
      }, math.ceil(math.log(arguments.numProcessors)/math.log(2)).toInt)
      // assert(allOrphanBlocks.isEmpty)


      // Update centroids of reconstructed processor blocks and remove duplicates
      val reconCentroidBlocks = allReconstructedBlocks.map {block =>
        val centroid = block.centroid
        Block(centroid, block.updateFaces(centroid))
      }

      val uniqueReconBlocks = reconCentroidBlocks.foldLeft(Seq.empty[Block]) { (unique, current) =>
        if (!unique.exists(current.approximateEquals(_))) {
          current +: unique
        } else {
          unique
        }
      }

      // Clean up faces of reconstructed blocks with values that should be zero, but have
      // arbitrarily small floating point values
      val squeakyCleanRecon = uniqueReconBlocks.map { case Block(center, faces) =>
        Block(center, faces.map(_.applyTolerance))
      }

      // Convert reconstructed blocks to requested output
      if (arguments.toInequalities) {
        val jsonReconBlocks = squeakyCleanRecon.map(Json.blockToMinimalJson)
        printToFile(new File("reconstructedBlocks.json")) { field =>
          jsonReconBlocks.foreach(field.println)
        }
      }

      if (arguments.toVTK) {
        val vtkBlocksRecon = squeakyCleanRecon.map(BlockVTK(_))
        val jsonVtkBlocksRecon = vtkBlocksRecon.map(JsonToVtk.blockVtkToMinimalJson)
        printToFile(new File("vtkReconstructedBlocks.json")) { field =>
          jsonVtkBlocksRecon.foreach(field.println)
        }
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
    * along matching processor joints, elliminating false blocks caused by division of 
    * input rock volume into equal volumes by load balancer
    * @param processorBlocks Sequence of blocks containing one or more processor joints each
    * @param mergedBlocks Sequence of blocks that have had processor joints removed. Initial
    *                     input is an empty sequence
    * @param origin Origin that all blocks and their faces will be referenced to for consistency
    * @param nonMergedBlocks Sequence of blocks that can not be matched with other blocks. Initial
                             input is an empty sequence
    * @return Sequence of blocks with all processor joints removed by merging input blocks along
    *         matching processor joints. Redundant faces and duplicate blocks are already removed
    *         in this list, so it is not necessary to perform these computations on the returned
    *         sequence.
    */
  @tailrec
  def mergeBlocks(processorBlocks: Seq[Block], mergedBlocks: Seq[Block],
                  orphanBlocks: Seq[Block]):
                 (Seq[Block], Seq[Block]) = {
    println("\nProcessor Blocks")
    processorBlocks.foreach(println)
    val blockMatches = findMates(processorBlocks)
    val pairedBlocks = blockMatches.map{ case (paired, _) => paired }
    val matchIndices = blockMatches.flatMap { case (_, indices) => indices}.distinct
    val originalPairedBlocks = matchIndices.collect(processorBlocks)
    println("These are the paired blocks")
    originalPairedBlocks.foreach(println)

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
    println("These are the orphan blocks")
    currentOrphanBlocks.foreach(println)

    // Remove redundant faces and remove duplicates
    val joinedBlocks = (pairedBlocks map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }).filter { block => block.faces.nonEmpty } // Filters blocks that actually aren't adjacent at all,
                                                // but shared a processor joint
    println("Merged Blocks")
    joinedBlocks.foreach(println)

    // Divide blocks into groups with and without processor joints remaining
    val (remainingBlocks, completedBlocks) = joinedBlocks.partition { block =>
      block.faces.exists(_.processorJoint)
    }
    println("Remaining Blocks")
    remainingBlocks.foreach(println)

    if (remainingBlocks.nonEmpty) {
      // Merged blocks still contain some processor joints
      println("DAAR IS NOG BAIE KAK OM TE DOEN")
      mergeBlocks(remainingBlocks ++ newProcessorBlocks, completedBlocks ++ mergedBlocks,
        currentOrphanBlocks)
//    } else if (processorBlocks.diff(currentOrphanBlocks).isEmpty) {
//      (completedBlocks ++ mergedBlocks, currentOrphanBlocks)
    } else if (newProcessorBlocks.isEmpty) {
      // All blocks are free of processor joints - check for duplicates then return
      println("FOK DIT, EK IS KLAAR, HIER IS DIE BLOKKE")
      (completedBlocks ++ mergedBlocks, currentOrphanBlocks)
    } else {
      // Proceed to next processor block
      println("REG, AAN NA DIE VOLGENDE EEN")
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
    (processorFaces1 flatMap { case face1 =>
      processorFaces2 flatMap { case face2 =>
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
//            val nonNegativeFaces = nonSharedFaces.map { face =>
//              if (face.d < 0.0) {
//                Face((-face.a, -face.b, -face.c), -face.d, face.phi, face.cohesion, face.processorJoint)
//              } else {
//                face
//              }
//            }
            Some(Block(localCenter, nonSharedFaces), Seq[Int](0, processorBlocks.indexOf(block)))
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

  // Function that writes JSON string to file for single node - taken from 
  // http://stackoverflow.com/questions/4604237/how-to-write-to-a-file-in-scala
  private def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
