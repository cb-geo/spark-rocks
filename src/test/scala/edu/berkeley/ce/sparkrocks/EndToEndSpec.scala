package edu.berkeley.ce.sparkrocks

import org.scalatest._

import scala.io.Source

class EndToEndSpec extends FunSuite {
  val INPUT_FILE_NAME = "endToEndData.txt"
  val OUTPUT_FILE_NAME = "blocks.json"

  test("Simple end-to-end test using unit cube and simple planes") {
    // Read input file to generate list of joints and initial rock block
    val inputSource = Source.fromURL(getClass.getResource(s"/$INPUT_FILE_NAME"))
    val (globalOrigin, boundingBox, rockVolumeInputs, jointSetInput) = InputProcessor.readInput(inputSource).get
    inputSource.close()
    val generatedInputs = JointGenerator(globalOrigin, boundingBox, rockVolumeInputs, jointSetInput)

    // Create an initial block
    val initialBlocks = Seq(Block(globalOrigin, generatedInputs.rockVolume))

    // Generate seed joints
    val numPartitions = 2
    val (seedBlocks, nonSeedJoints) =
      SeedJointSelector.generateSeedBlocks(generatedInputs.jointSets, initialBlocks.head, numPartitions)

    // Iterate through joints, cutting blocks where appropriate
    val cutBlocks = seedBlocks flatMap { block =>
      nonSeedJoints.foldLeft(Seq(block)) { (currentBlocks, joint) =>
        currentBlocks.flatMap(_.cut(joint, minSize = 0.0, maxAspectRatio = Double.PositiveInfinity))
      }
    }

    // Remove geometrically redundant joints
    val nonRedundantBlocks = cutBlocks.map { case block @ Block(center, _, _) =>
      Block(center, block.nonRedundantFaces)
    }

    // Calculate the centroid of each block
    val centroidBlocks = nonRedundantBlocks.map { block =>
      val centroid = block.centroid
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, updatedFaces)
    }

    // Clean up double values arbitrarily close to 0.0
    val cleanedBlocks = centroidBlocks.map { case Block(center, faces, _) =>
      Block(center, faces.map(_.applyTolerance))
    }

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