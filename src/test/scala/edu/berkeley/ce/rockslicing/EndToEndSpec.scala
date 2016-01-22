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

    // Generate seed joint
    val numSeedJoints = 2
    val seedJoints = LoadBalancer.generateSeedJoints(blocks.head, numSeedJoints)
    val joints = seedJoints ++ jointList

    // Iterate through joints, cutting blocks where appropriate
    var cutBlocks = blocks
    for (joint <- joints) {
      cutBlocks = cutBlocks.flatMap(_.cut(joint))
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
      val faceTests = block.faces map { case face =>
        face.processorJoint
      }
      !faceTests.contains(true)
    }

    // Search blocks for matching processor joints
    val reconBlocks = RockSlicer.mergeBlocks(processorBlocks, Seq[Block](),
                                             globalOrigin)

    // Update centroids of reconstructed processor blocks and remove duplicates
    // val reconBlocksRedundant = reconBlocks.map {case block @ Block(center, _) =>
    //   Block(center, block.nonRedundantFaces)
    // }
    val reconCentroidBlocks = reconBlocks.map {block =>
      val centroid = block.centroid
      Block(centroid, block.updateFaces(centroid))
    }
    // val reconCentroidBlocksDistinct =
    //     reconCentroidBlocks.foldLeft(Seq[Block]()) { (unique, current) =>
    //       if (!unique.exists(Block.compareBlocks(current, _)))
    //         current +: unique
    //       else unique
    //     }

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
    try {
      val expectedJson = expectedJsonSource.mkString
      assert(blockJson.trim == expectedJson.trim)
    } finally {
      expectedJsonSource.close()
    }
  }
}
