package edu.berkeley.ce.rockslicing

import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable
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
    val seedJoints = LoadBalancer.generateSeedJoints(blocks.head, arguments.numSeedJoints)

    seedJoints foreach { joint =>
      blocks = blocks.flatMap(_.cut(joint, arguments.minRadius, arguments.maxAspectRatio))
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
      block.faces.exists { face => face.processorJoint}
    }
    // Find blocks that do not contain processor joints
    val realBlocks = nonRedundantBlocks.diff(processorBlocks)

    // Collect all blocks that contain processor joints from all nodes
    val allProcessorBlocks = realBlocks.collect()
    // Search blocks for matching processor joints
    val reducedBlocks = allProcessorBlocks map { case block1 @ Block(center1, faces1) =>
      allProcessorBlocks map { case block2 @ Block(center2, faces2) =>
        val updatedFaces2 = block2.updateFaces(center1).filter {case updatedFace =>
          updatedFace.processorJoint 
        }
        val processorFaces1 = faces1.filter { case f => f.processorJoint }
        processorFaces1.map { case face1 =>
          updatedFaces2.map { case face2 =>
            if ((math.abs(face1.a + face2.a) < NumericUtils.EPSILON) &&
                (math.abs(face1.b + face2.b) < NumericUtils.EPSILON) &&
                (math.abs(face1.c + face2.c) < NumericUtils.EPSILON) &&
                (math.abs(face1.d - face2.d) < NumericUtils.EPSILON)) {
              val realFaces = (faces1 +: faces2).filter { case anotherFace => anotherFace.processorJoint}
              Block(center1, realFaces)
            }
          }
        }
      }
    }
    // Update centroids of reconstructed processor blocks and remove duplicates
    val reducedCentroidBlocks = reducedBlocks.map {blocks =>
      val centroid = block.centroid
      Block(centroid, block.updateFaces(centroid))
    }
    val reconstructedBlocks = reducedCentroidBlocks.distinct

    // Calculate centroid of each real block
    val centroidBlocks = realBlocks.map { block =>
      val centroid = block.centroid
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, updatedFaces)
    }

    // Merge real blocks and reconstructed blocks
    val allBlocks = centroidBlocks +: reconstructedBlocks
    // Clean up faces with values that should be zero, but have arbitrarily small floating point values
    val squeakyClean = allBlocks.map { case Block(center, faces) =>
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
      // to vtk my rockProcessor module
      val vtkBlocks = squeakyClean.map(BlockVTK(_))
      val jsonVtkBlocks = vtkBlocks.map(JsonToVtk.blockVtkToMinimalJson)
      jsonVtkBlocks.saveAsTextFile("vtkBlocks.json")
    }
    sc.stop()
  }
}
