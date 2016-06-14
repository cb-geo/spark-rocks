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
    val initialBlocks = Seq(Block(globalOrigin, rockVolume))

    // Generate processor joints
    val numProcessors = 6
    val processorJoints = LoadBalancer.generateProcessorJoints(initialBlocks.head, numProcessors)
    val joints = processorJoints ++ jointList

    // Iterate through joints, cutting blocks where appropriate
    val cutBlocks = initialBlocks flatMap { block =>
      joints.foldLeft(Seq(block)) { (currentBlocks, joint) =>
        currentBlocks.flatMap(_.cut(joint))
      }
    }

    // Remove geometrically redundant joints
    val nonRedundantBlocks = cutBlocks.map { case block @ Block(center, _, _) =>
      Block(center, block.nonRedundantFaces)
    }

    // Find blocks that do not contain processor joints
    val (processorBlocks, realBlocks) = nonRedundantBlocks.partition { block => block.faces.exists(_.isProcessorFace) }
    val finalBlocks = if (processorBlocks.isEmpty) {
      realBlocks
    } else {
      realBlocks ++ LoadBalancer.mergeProcessorBlocks(processorBlocks)
    }

    // Calculate the centroid of each block
    val centroidBlocks = finalBlocks.map { block =>
      val centroid = block.centroid
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, updatedFaces)
    }

    // Clean up double values arbitrarily close to 0.0
    val cleanedBlocks = centroidBlocks.map { case Block(center, faces, _) =>
      Block(center, faces.map(_.applyTolerance))
    }

    val blockJson = Json.blockSeqToReadableJson(cleanedBlocks)
    val expectedJsonSource = Source.fromURL(getClass.getResource(s"/$OUTPUT_FILE_NAME"))

    try {
      val expectedJson = expectedJsonSource.mkString
      val expectedBlocks = Json.blockSeqFromJson(expectedJson)

      assert(cleanedBlocks.forall(actualBlock => expectedBlocks.exists{
        expectedBlock => actualBlock.approximateEquals(expectedBlock)
      }))
    } finally {
      expectedJsonSource.close()
    }
  }
}