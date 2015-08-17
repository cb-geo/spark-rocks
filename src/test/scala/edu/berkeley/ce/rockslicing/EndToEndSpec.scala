package edu.berkeley.ce.rockslicing

import org.scalatest._

import scala.io.Source

class EndToEndSpec extends FunSuite {
  val INPUT_FILE_NAME = "endToEndData.txt"
  val OUTPUT_FILE_NAME = "blocks.json"

  test("Simple end-to-end test using unit cube and simple planes") {
    // Read input file to generate list of joints and initial rock block
    val inputSource = Source.fromURL(getClass.getResource(s"/$INPUT_FILE_NAME"))
    val (rockVolume, jointList) = InputProcessor.readInput(inputSource)
    inputSource.close()

    // Create an initial block
    val blocks = Vector(Block((0.0, 0.0, 0.0), rockVolume))

    // Iterate through joints, cutting blocks where appropriate
    var cutBlocks = blocks
    for (joint <- jointList) {
      cutBlocks = cutBlocks.flatMap(_.cut(joint))
    }

    // Remove geometrically redundant joints
    val nonRedundantBlocks = cutBlocks.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }

    // Calculate the centroid of each block
    val centroidBlocks = nonRedundantBlocks.map { block =>
      val centroid = block.centroid
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, updatedFaces)
    }

    // Clean up double values arbitrarily close to 0.0
    val cleanedBlocks = centroidBlocks.map { case Block(center, faces) =>
      Block(center, faces.map(_.applyTolerance))
    }

    val blockJson = Json.blockSeqToReadableJson(cleanedBlocks)
    val expectedJsonSource = Source.fromURL(getClass.getResource(s"/$OUTPUT_FILE_NAME"))
    try {
      val expectedJson = expectedJsonSource.mkString
      assert(blockJson.trim == expectedJson.trim)
    } finally {
      expectedJsonSource.close()
    }
  }
}
