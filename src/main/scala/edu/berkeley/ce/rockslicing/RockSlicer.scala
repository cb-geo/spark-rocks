package edu.berkeley.ce.rockslicing

import java.io._
import org.apache.spark.{SparkConf, SparkContext, HashPartitioner}
import org.apache.spark.storage.StorageLevel
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

    val blockRdd = sc.parallelize(blocks).persist(StorageLevel.MEMORY_ONLY)
    // val processorBlocksRDD = if (arguments.numProcessors > 1) {
    //   sc.parallelize(processorJoints.map(joint => (joint, Seq.empty[Block])))
    //     .partitionBy(new HashPartitioner(arguments.numProcessors))
    //     .persist(StorageLevel.MEMORY_ONLY)
    // } else {
    //   (None, None)
    // }

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
    // Collect all blocks that contain processor joints from all nodes
    val allProcessorBlocks = processorBlocks.collect()
    // Search blocks for matching processor joints
    if (allProcessorBlocks.length > 0) {
      // val sortedProcessorBlocks =
      //   generateProcJointKeyValues(broadcastProcJoints.value, processorBlocks.toLocalIterator.toSeq)
      // val sortedProcBlocksRDD = sc.parallelize(sortedProcessorBlocks)
      // // Shuffle processor blocks so all blocks containing same processor joints are local to each node
      // processorBlocksRDD.join(sortedProcessorBlocks)

      val (reconstructedBlocks, orphanBlocks) = mergeBlocks(allProcessorBlocks, Seq.empty[Block], globalOrigin, Seq.empty[Block])

      // Update centroids of reconstructed processor blocks and remove duplicates
      val reconCentroidBlocks = reconstructedBlocks.map {block =>
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
                  origin: (Double, Double, Double), nonMergedBlocks: Seq[Block]):
                 (Seq[Block], Seq[Block]) = {
    val blockMatches = processorBlocks.tail map { block =>
      val currentBlock = Block(origin, processorBlocks.head.updateFaces(origin))
      val updatedBlock = Block(origin, block.updateFaces(origin))
      val sharedProcFaces = compareProcessorBlocks(currentBlock, updatedBlock)
      if (sharedProcFaces.nonEmpty) {
        val currentFaces = currentBlock.faces.diff(sharedProcFaces)
        val updatedFaces = updatedBlock.faces.diff(sharedProcFaces)
        val allFaces = currentFaces ++ updatedFaces
        // Check for any actual shared faces between blocks - if these exists blocks
        // should not be merged since there is a real joint seperating the blocks
        val nonSharedFaces =
          allFaces.foldLeft(Seq.empty[Face]) { (unique, current) =>
            if (!unique.exists(current.isSharedWith(_))) {
              current +: unique
            } else {
              unique
            }
          }
        if (allFaces.diff(nonSharedFaces).isEmpty) {
          (Some(Block(origin, nonSharedFaces)), None)
        } else {
          (None, Some(currentBlock))
        }
      } else {
        (None, Some(currentBlock))
      }
    }

    val pairedBlocks = blockMatches.map{ case (paired, _) => paired }.flatten
    val orphanBlocks = blockMatches.map{ case (_, orphan) => orphan }.flatten
    // Remove redundant faces and remove duplicates
    val joinedBlocks = (pairedBlocks map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }).filter { block => block.faces.nonEmpty } // Filters blocks that actually aren't adjacent at all,
                                                // but shared a processor joint

    // Divide blocks into groups with and without processor joints remaining
    val (remainingBlocks, completedBlocks) = joinedBlocks.partition { block =>
      block.faces.exists(_.processorJoint)
    }

    if (remainingBlocks.nonEmpty) {
      // Merged blocks still contain some processor joints
      mergeBlocks(remainingBlocks ++ processorBlocks.tail, completedBlocks ++ mergedBlocks,
                  origin, orphanBlocks ++ nonMergedBlocks)
    } else if (processorBlocks.tail.isEmpty) {
      // All blocks are free of processor joints - check for duplicates then return
      val mergedBlocksDuplicates = completedBlocks ++ mergedBlocks
      val mergedBlocksUnique = mergedBlocksDuplicates.foldLeft(Seq.empty[Block]) { (unique, current) =>
        if (!unique.exists(current.approximateEquals(_))) {
          current +: unique
        } else {
          unique
        }
      }
      (mergedBlocksUnique, orphanBlocks ++ nonMergedBlocks)
    } else {
      // Proceed to next processor block
      mergeBlocks(processorBlocks.tail, completedBlocks ++ mergedBlocks, origin,
                  orphanBlocks ++ nonMergedBlocks)
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
    val faceMatches = 
      processorFaces1 map { case face1 =>
        processorFaces2 map { case face2 =>
          if (face1.isSharedWith(face2)) {
            Seq[Face](face1, face2)
          } else {
            Seq.empty[Face]
          }
        }
      }
    faceMatches.filter(_.nonEmpty).flatten.flatten.distinct
  }

  /**
    * Generates key-value pairs of processor joints and blocks that contain that processor joint
    * @param procJoints Seq of processor joints
    * @param procBlocks Seq of blocks containing processor joints
    */
  def generateProcJointKeyValues(procJoints: Seq[Joint], procBlocks: Seq[Block]): 
                                 Seq[(Joint, Seq[Block])] = {
    procJoints map { joint =>
      (joint, procBlocks.filter(block => joint.inBlock(block)))
    }
  }

  // Function that writes JSON string to file for single node - taken from 
  // http://stackoverflow.com/questions/4604237/how-to-write-to-a-file-in-scala
  private def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
