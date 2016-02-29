package edu.berkeley.ce.rockslicing

import org.scalatest._

import scala.io.Source

class EndToEndSpec extends FunSuite {
  val INPUT_FILE_NAME = "endToEndData.txt"
  val OUTPUT_FILE_NAME = "blocks.json"

  test("Simple end-to-end test using unit cube and simple planes") {
    // Read input file to generate list of joints and initial rock block
    val inputSource = Source.fromURL(getClass.getResource(s"/$INPUT_FILE_NAME"))
    val (globalOrigin, rockVolume, jointList) = InputProcessor.readInput(inputSource).get
    inputSource.close()

    // Create an initial block
    val blocks = Seq(Block(globalOrigin, rockVolume))

    // Generate processor joints
    val numProcessors = 6
    val processorJoints = LoadBalancer.generateProcessorJoints(blocks.head, numProcessors)
    val joints = processorJoints ++ jointList

    // Iterate through joints, cutting blocks where appropriate
    var cutBlocks = blocks
    for (joint <- joints) {
      cutBlocks = cutBlocks.flatMap(_.cut(joint))
    }

    // Remove geometrically redundant joints
    val nonRedundantBlocks = cutBlocks.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }

    // Note: The following two filter operations could be done using the partition method;
    //       however, when executing on Spark this will be an RDD and partition will not work.
    //       The two filter operations are left here to mimic what is done in the actual Spark code

    // Find all blocks that contain processor joints
    val processorBlocks = nonRedundantBlocks.filter { block => 
      block.faces.exists(_.processorJoint)
    }

    // Find blocks that do not contain processor joints
    val realBlocks = nonRedundantBlocks.filter { block =>
      val faceTests = block.faces.exists(_.processorJoint)
      !faceTests
    }

    // Search blocks for matching processor joints
    val updatedProcessorBlocks = processorBlocks.map { block =>
      Block(globalOrigin, block.updateFaces(globalOrigin))
    }
    val (reconBlocks, orphanBlocks) = RockSlicer.mergeBlocks(updatedProcessorBlocks, Seq.empty[Block],
                                                             Seq.empty[Block])

    // Update centroids of reconstructed processor blocks and remove duplicates
    val reconCentroidBlocks = reconBlocks.map { block =>
      val centroid = block.centroid
      Block(centroid, block.updateFaces(centroid))
    }

    // Calculate the centroid of each real block
    val centroidBlocks = realBlocks.map { block =>
      val centroid = block.centroid
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, updatedFaces)
    }

    // Merge real blocks and reconstructed blocks - this won't happen on Spark since collect will
    // be called and all reconstructed blocks will be on one node
    val allBlocks = centroidBlocks ++ reconCentroidBlocks

    // Clean up double values arbitrarily close to 0.0
    val cleanedBlocks = allBlocks.map { case Block(center, faces) =>
      Block(center, faces.map(_.applyTolerance))
    }

    val blockJson = Json.blockSeqToReadableJson(cleanedBlocks)
    val expectedJsonSource = Source.fromURL(getClass.getResource(s"/$OUTPUT_FILE_NAME"))

    assert(orphanBlocks.isEmpty)
    try {
      val expectedJson = expectedJsonSource.mkString
      assert(blockJson.trim == expectedJson.trim)
    } finally {
      expectedJsonSource.close()
    }
  }
}
