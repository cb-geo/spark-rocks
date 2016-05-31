package edu.berkeley.ce.rockslicing

import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.io.Source

object RockSlicer {
  val REDUNDANT_ELIM_PERIOD = 200

  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("SparkRocks")
      // Use Kryo serialization
      .set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
      // Require all classes to be registered
      .set("spark.kryo.registrationRequired", "true")
    // Register classes with Kryo using ClassRegistrator
    conf.set("spark.kryo.registrator", "edu.berkeley.ce.rockslicing.ClassRegistrator")

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
    val starterBlocks = Seq(Block(globalOrigin, rockVolume))

    // Generate a list of initial blocks before RDD-ifying it
    val seedBlocks = if (arguments.numProcessors > 1) {
      val processorJoints = LoadBalancer.generateProcessorJoints(starterBlocks.head, arguments.numProcessors)
      processorJoints.foldLeft(starterBlocks) { (currentBlocks, joint) =>
        currentBlocks.flatMap(_.cut(joint))
      }
    } else {
      starterBlocks
    }

    val seedBlockRdd = sc.parallelize(seedBlocks)
    val broadcastJoints = sc.broadcast(joints)

    // Iterate through the discontinuities for each seed block, cutting where appropriate
    val cutBlocks = seedBlockRdd flatMap { seedBlock =>
      broadcastJoints.value.zipWithIndex.foldLeft(Seq(seedBlock)) { case (currentBlocks, (joint, idx)) =>
        if (idx % REDUNDANT_ELIM_PERIOD == 0) {
          // When idx is a multiple of REDUNDANT_ELIM_PERIOD, check for geometrically redundant faces
          currentBlocks.flatMap(_.cut(joint, generation=idx)).map { case block @ Block(center, _, generation) =>
            if (generation > idx - REDUNDANT_ELIM_PERIOD) {
              // We only perform the check if the block has been newly added since the last round of checks
              Block(center, block.nonRedundantFaces, generation)
            } else {
              block
            }
          }
        } else {
          // Otherwise, just cut new blocks without checking for redundant faces
          currentBlocks.flatMap(_.cut(joint, generation=idx))
        }
      }
    }

    // We need to do one last round of checking for redundant faces at the end
    val nonRedundantBlocks = cutBlocks.map { case block @ Block(center, _, generation) =>
      if (generation > broadcastJoints.value.length - REDUNDANT_ELIM_PERIOD) {
        // Again, we only perform the check on blocks created since the previous round of checks
        Block(center, block.nonRedundantFaces, generation)
      } else {
        block
      }
    }

    // Find all blocks that contain processor joints
    val processorBlocks = nonRedundantBlocks.filter { block =>
      block.faces.exists(_.isProcessorFace)
    }

    val mergedBlocks = if (processorBlocks.isEmpty) {
      nonRedundantBlocks
    } else {
      /*
       * Since we're dealing with RDDs and not Seqs here, we have to adapt
       * the logic of LoadBalancer.mergeProcessorBlocks
       */
      val realBlocks = nonRedundantBlocks.filter { block =>
        !block.faces.exists(_.isProcessorFace)
      }

      var orphanBlocks = processorBlocks.map { block =>
        val globalOrigin = Array(0.0, 0.0, 0.0)
        // Using same number of decimal places as in LoadBalancer for now
        Block(globalOrigin, block.updateFaces(globalOrigin).map(_.roundToTolerance(decimalPlaces=4)))
      }
      var matchedBlocks: RDD[Block] = sc.emptyRDD[Block]

      /*
       * We should iterate more than once only in the unlikely event that some
       * blocks are adjacent to multiple processor joints
       */
      while (!orphanBlocks.isEmpty()) {
        val normVecBlocks = orphanBlocks.groupBy { block =>
          val processorFace = block.faces.find(_.isProcessorFace).get
          // Normal vectors for same joint could be equal and opposite, so use abs
          ((math.abs(processorFace.a), math.abs(processorFace.b), math.abs(processorFace.c)),
            math.abs(processorFace.d))
        }

        val mergeResults = normVecBlocks.map { case (commonJoint, blocks) =>
          LoadBalancer.removeCommonProcessorJoint(commonJoint, blocks.toSeq)
        }
        matchedBlocks = matchedBlocks ++ mergeResults.flatMap(_._1)
        orphanBlocks = mergeResults.flatMap(_._2)
      }

      realBlocks ++ matchedBlocks
    }

    val centroidBlocks = mergedBlocks.map { block =>
      val centroid = block.centroid
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, updatedFaces)
    }

    // Clean up faces of real blocks with values that should be zero, but have arbitrarily
    // small floating point values
    val squeakyClean = centroidBlocks.map { case Block(center, faces, _) =>
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
}
