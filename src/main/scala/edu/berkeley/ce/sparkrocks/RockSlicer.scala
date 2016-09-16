package edu.berkeley.ce.sparkrocks

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
      .set("spark.kryo.registrator", "edu.berkeley.ce.sparkrocks.ClassRegistrator")

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
    val (globalOrigin, boundingBox, rockVolumeInputs, jointSetInputs) = inputOpt.get
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolumeInputs, jointSetInputs)
    val starterBlocks = Seq(Block(globalOrigin, generatedInput.rockVolume))

    val startTime = System.nanoTime()
    // Generate a list of initial blocks before RDD-ifying it
    val (seedBlocks, remainingJoints) = if (arguments.numProcessors > 1) {
      // Check if at least one of the input joint sets is persistent
      if (jointSetInputs.forall(_.persistence != 100.0)) {
        println("ERROR: Input joint sets must contain at least one persistent joint set to run in parallel. Rerun" +
          "analysis in serial if all joint sets are non-persistent.")
        System.exit(-1)
      }

      SeedJointSelector.generateSeedBlocks(generatedInput.jointSets, starterBlocks.head, arguments.numProcessors)
    } else {
      (starterBlocks, generatedInput.jointSets.flatten)
    }

    val seedBlockRdd = sc.parallelize(seedBlocks)
    val broadcastJoints = sc.broadcast(remainingJoints)

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
    }.cache()

    val centroidBlocks = nonRedundantBlocks.map { block =>
      val centroid = block.centroid
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, updatedFaces)
    }

    // Clean up faces of real blocks with values that should be zero, but have arbitrarily
    // small floating point values
    val squeakyClean = centroidBlocks.map { case Block(center, faces, _) =>
      Block(center, faces.map(_.applyTolerance))
    }

    val totalNumBlocks = squeakyClean.count()
    val endTime = System.nanoTime()
    println(s"Cut $totalNumBlocks in ${(endTime - startTime) / 1.0e6} msec.")

    // Convert list of rock blocks to requested output
    if (arguments.jsonOut != "") {
      // Convert the list of rock blocks to JSON and save this to a file
      val jsonBlocks = squeakyClean.map(Json.blockToMinimalJson)
      jsonBlocks.saveAsTextFile(arguments.jsonOut)
    }

    if (arguments.vtkOut != "") {
      // Convert the list of rock blocks to JSON with vertices, normals and connectivity in
      // format easily converted to vtk by rockProcessor module
      val vtkBlocks = squeakyClean.map(BlockVTK(_))
      val jsonVtkBlocks = vtkBlocks.map(JsonToVtk.blockVtkToMinimalJson)
      jsonVtkBlocks.saveAsTextFile(arguments.vtkOut)
    }

    sc.stop()
  }
}
