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
    val cutBlocks = blocks flatMap { block =>
      joints.foldLeft(Seq(block)) { (currentBlocks, joint) =>
        currentBlocks.flatMap(_.cut(joint))
      }
    }

    // Remove geometrically redundant joints
    val nonRedundantBlocks = cutBlocks.map { case block @ Block(center, _, _) =>
      Block(center, block.nonRedundantFaces)
    }

    // Note: The following two filter operations could be done using the partition method;
    //       however, when executing on Spark this will be an RDD and partition will not work.
    //       The two filter operations are left here to mimic what is done in the actual Spark code

    // Find all blocks that contain processor joints
    val processorBlocks = nonRedundantBlocks.filter { block =>
      block.faces.exists(_.isProcessorFace)
    }

    // Find blocks that do not contain processor joints
    val realBlocks = nonRedundantBlocks.filter { block =>
      val faceTests = block.faces.exists(_.isProcessorFace)
      !faceTests
    }

    val finalBlocks = if (processorBlocks.isEmpty) {
      realBlocks
    } else {
      val globalOriginBlocks = processorBlocks.map { block =>
          val globalOrigin = Array[Double](0.0, 0.0, 0.0)
          Block(globalOrigin, block.updateFaces(globalOrigin))
      }

      /*
       * Don't forget that normal vectors for same joint could be equal and opposite
       * TODO: Okay to Assume that each block only has one processor face?
       * TODO: This could be very expensive...
       */
      val normVecBlocks = globalOriginBlocks.groupBy { block =>
        val processorFace = block.faces.filter(_.isProcessorFace).head
        ((math.abs(processorFace.a), math.abs(processorFace.b), math.abs(processorFace.c)),
          math.abs(processorFace.d))
      }

      val mergedBlocks = normVecBlocks.flatMap { case (_, blks) => removeProcessorJoints(blks) }

      realBlocks ++ mergedBlocks
    }

    // Calculate the centroid of each real block
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
      assert(blockJson.trim == expectedJson.trim)
    } finally {
      expectedJsonSource.close()
    }
  }

   private def removeProcessorJoints(blocks: Seq[Block]): Seq[Block] = {
    val (left, right) = blocks.partition { block =>
      val processorFace = block.faces.filter(_.isProcessorFace).head
      processorFace.a >= 0
    }

    left.map { leftBlock =>
      // Use 'view' for lazy evaluation, avoids unnecessary calculations
      val mergedFaces = right.view.map { rightBlock =>
          val mergedBlock = Block(leftBlock.center, (leftBlock.faces ++ rightBlock.faces).filter(!_.isProcessorFace))
          mergedBlock.nonRedundantFaces
      }

      // TODO: Okay to assume that exactly one mate will be found?
      val newFaces = mergedFaces.find(_.nonEmpty)
      assert(newFaces.isDefined)
      Block(leftBlock.center, newFaces.get)
    }
  }
}