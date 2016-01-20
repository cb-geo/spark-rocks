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
    val seedJoints = Seq(Joint((1.0, 0.0, 0.0), globalOrigin, (0.5, 0.0, 0.0), phi = 30.0, cohesion = 0.0,
                           shape = Nil, artificialJoint = Some(true)))
    val joints = seedJoints ++ jointList

    // Iterate through joints, cutting blocks where appropriate
    var cutBlocks = blocks
    for (joint <- joints) {
      cutBlocks = cutBlocks.flatMap(_.cut(joint))
    }

    // Remove geometrically redundant joints
    val nonRedundantBlocks = cutBlocks.map { case block@Block(center, _) =>
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
    val reducedBlocks = (processorBlocks flatMap { block1 =>
      processorBlocks map { block2 =>
        val center1 = (block1.centerX, block1.centerY, block1.centerZ)
        val updatedBlock2 = Block(center1, block2.updateFaces(center1))
        val sharedFaces = RockSlicer.compareProcessorBlocks(block1, updatedBlock2)
        if (sharedFaces.nonEmpty) {
          val block1Faces = block1.faces.diff(sharedFaces)
          val block2Faces = updatedBlock2.faces.diff(sharedFaces)
          Block(center1, block1Faces ++ block2Faces)
        }
      }
    }).collect{ case blockType: Block => blockType}

    // Update centroids of reconstructed processor blocks and remove duplicates
    val reducedBlocksRedundant = reducedBlocks.map {case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }
    val reducedCentroidBlocks = reducedBlocksRedundant.map {block =>
      val centroid = block.centroid
      Block(centroid, block.updateFaces(centroid))
    }
    val reducedCentroidBlocksDistinct = reducedCentroidBlocks.distinct

    // Calculate the centroid of each real block
    val centroidBlocks = nonRedundantBlocks.map { block =>
      val centroid = block.centroid
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, updatedFaces)
    }

    println("Real Blocks: ")
    centroidBlocks.foreach(println)
    println()
    println("Proc. Blocks: ")
    reducedCentroidBlocksDistinct.foreach(println)

    // Merge real blocks and reconstructed blocks - this won't happen on Spark since collect will
    // be called and all reconstructed blocks will be on one node
    val allBlocks = centroidBlocks ++ reducedCentroidBlocksDistinct

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
