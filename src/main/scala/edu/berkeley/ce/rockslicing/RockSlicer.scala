package edu.berkeley.ce.rockslicing

import java.io._
import org.apache.spark.{SparkConf, SparkContext}
import scala.io.Source

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

    // Generate a list of initial blocks before RDD-ifying it
    if (arguments.numSeedJoints > 0) {
      val seedJoints = LoadBalancer.generateSeedJoints(blocks.head, arguments.numSeedJoints)
      seedJoints foreach { joint =>
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
    val processorBlocks = nonRedundantBlocks.filter { case block => 
      block.faces.exists { face => face.processorJoint}
    }

    // Find blocks that do not contain processor joints
    val realBlocks = nonRedundantBlocks.filter { block =>
      val faceFlags = block.faces map { case face =>
        face.processorJoint
      }
      !faceFlags.contains(true)
    }

    // PROCESS PROCESSOR BLOCKS BEFORE CONTINUEING WITH REMAINING BLOCKS
    // Collect all blocks that contain processor joints from all nodes
    val allProcessorBlocks = processorBlocks.collect()
    // Search blocks for matching processor joints
    if (allProcessorBlocks.length > 0) {
      val reconstructedBlocks = (allProcessorBlocks flatMap { block1 =>
        allProcessorBlocks map { block2 =>
          if (!Block.compareBlocks(block1, block2)) {
            val center1 = (block1.centerX, block1.centerY, block1.centerZ)
            val updatedBlock2 = Block(center1, block2.updateFaces(center1))
            val sharedProcFaces = compareProcessorBlocks(block1, updatedBlock2)
            if (sharedProcFaces.nonEmpty) {
              val block1Faces = block1.faces.diff(sharedProcFaces)
              val block2Faces = updatedBlock2.faces.diff(sharedProcFaces)
              val allFaces = block1Faces ++ block2Faces
              // Check for any real shared faces between blocks - if these exist blocks should NOT
              // be merged since there is an actual joint seperating blocks
              val nonSharedFaces =
                allFaces.foldLeft(Seq[Face]()) { (unique, current) =>
                  if (!unique.exists(compareSharedFaces(current, _)))
                    current +: unique
                  else unique
                }
              if (allFaces.diff(nonSharedFaces).isEmpty)
                Block(center1, nonSharedFaces)
            }
          }
        }
      }).collect{ case blockType: Block => blockType}

      // Update centroids of reconstructed processor blocks and remove duplicates
      val reconstructedBlocksRedundant = reconstructedBlocks.map {case block @ Block(center, _) =>
        Block(center, block.nonRedundantFaces)
      }
      val reconCentroidBlocks = reconstructedBlocksRedundant.map {block =>
        val centroid = block.centroid
        Block(centroid, block.updateFaces(centroid))
      }
      val reconCentroidBlocksDistinct =
        reconCentroidBlocks.foldLeft(Seq[Block]()) { (unique, current) =>
          if (!unique.exists(Block.compareBlocks(current, _)))
            current +: unique
          else unique
        }

      // Clean up faces of reconstructed blocks with values that should be zero, but have
      // arbitrarily small floating point values
      val squeakyCleanRecon = reconCentroidBlocksDistinct.map { case Block(center, faces) =>
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

    // PROCESS REMAINING BLOCKS THAT DO NOT CONTAIN PROCESSOR BLOCKS
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
      // Convert the list of rock blocks to JSON with vertices, normals and connectivity in format easily converted
      // to vtk by rockProcessor module
      val vtkBlocks = squeakyClean.map(BlockVTK(_))
      val jsonVtkBlocks = vtkBlocks.map(JsonToVtk.blockVtkToMinimalJson)
      jsonVtkBlocks.saveAsTextFile("vtkBlocks.json")
    }
    sc.stop()
  }

  /**
    * 
    */
  def mergeBlocks(processorBlocks: Seq[Block], mergedBlocks: Seq[Block]): Seq[Block] = {
    val reconstructedBlock = processorBlocks map { block =>
      val currentBlock = processorBlocks.head
      if (!Block.compareBlocks(currentBlock, block, ))
        // CONTINUE HERE, BUT STILL NEED TO FIX COMPAREPROCESSORBLOCKS
    }
  }

  /**
    * Compares two input blocks and determines whether they share a processor face
    * @param block1 First input block
    * @param block2 Second input block
    * @return List of processor faces that are shared by the two blocks. Will empty
    *         if they share no faces
    */
  def compareProcessorBlocks(block1: Block, block2: Block,
                             origin: (Double, Double, Double)): Seq[Face] = {
    // STILL NEED TO ADD UPDATE TO BLOCKS FOR ORIGIN
    val processorFaces1 = block1.faces.filter { case face => face.processorJoint }
    val processorFaces2 = block2.faces.filter { case face => face.processorJoint }
    val faceMatches = 
      processorFaces1 map { case face1 =>
        processorFaces2 map { case face2 =>
          if ((math.abs(face1.a + face2.a) < NumericUtils.EPSILON) &&
              (math.abs(face1.b + face2.b) < NumericUtils.EPSILON) &&
              (math.abs(face1.c + face2.c) < NumericUtils.EPSILON) &&
              (math.abs(face1.d + face2.d) < NumericUtils.EPSILON)) {
            Seq[Face](face1, face2)
          } else Seq[Face]()
        }
      }
    faceMatches.filter{ case faces => faces.nonEmpty }.flatten.flatten.distinct
  }

  /**
    * Compares two input faces and determines whether faces are shared. Shared
    * faces will have equal and opposite distances from local origin as well as
    * normal vectors in opposite directions
    * @param face1 First input face
    * @param face2 Second input face
    * @return True if faces are shared, false otherwise
    */
  def compareSharedFaces(face1: Face, face2: Face): Boolean = {
    (math.abs(face1.a + face2.a) < NumericUtils.EPSILON) &&
    (math.abs(face1.b + face2.b) < NumericUtils.EPSILON) &&
    (math.abs(face1.c + face2.c) < NumericUtils.EPSILON) &&
    (math.abs(face1.d + face2.d) < NumericUtils.EPSILON)
  }

  // Function that writes JSON string to file for single node - taken from 
  // http://stackoverflow.com/questions/4604237/how-to-write-to-a-file-in-scala
  private def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
