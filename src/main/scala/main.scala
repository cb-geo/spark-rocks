package edu.berkeley.ce.rockslicing

import org.apache.spark.{SparkConf, SparkContext}

import scala.io.Source

object RockSlicer {

  val NUM_SEED_JOINTS = 25

  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("SparkRocks")
    val sc = new SparkContext(conf)
    val inputFile = args(0)
    val numberPartitions = args(1).toInt

    // Open and read input file specifying rock volume and joints
    val inputSource = Source.fromFile(inputFile)
    val (rockVolume, joints) = inputProcessor.readInput(inputSource)
    inputSource.close()
    var blocks = Vector(Block((0.0, 0.0, 0.0), rockVolume))

    // Generate a list of initial blocks before RDD-ifying it
    val (seedJoints, remainingJoints) = generateSeedJoints(joints, NUM_SEED_JOINTS)
    seedJoints foreach { joint => blocks = blocks.flatMap(_.cut(joint)) }
    val blockRdd = sc.parallelize(blocks)
    val broadcastJoints = sc.broadcast(remainingJoints)

    // Iterate through the discontinuities, cutting blocks where appropriate
    var cutBlocks = blockRdd
    for (joint <- broadcastJoints.value) {
      cutBlocks = cutBlocks.flatMap(_.cut(joint))
    }

    // Remove geometrically redundant joints
    val nonRedundantBlocks = cutBlocks.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }

    // Calculate centroid of each block
    val centroidBlocks = nonRedundantBlocks.map { block =>
      val centroid = block.centroid
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, updatedFaces)
    }

    // Clean up faces with values that should be zero, but have arbitrarily small floating point values
    val squeakyClean = centroidBlocks.map { case Block(center, faces) =>
      Block(center, faces.map(_.applyTolerance))
    }

    // Convert the list of rock blocks to JSON and save this to a file
    val jsonBlocks = squeakyClean.map(json.blockToMinimalJson)
    jsonBlocks.saveAsTextFile("blocks.json")
    sc.stop()
  }

  // Produce joints that achieve some lead balancing when generating the initial Block RDD
  private def generateSeedJoints(joints: Seq[Joint], numSeedJoints: Integer): (Seq[Joint], Seq[Joint]) = {
    val (persistentJoints, nonPersistentJoints) = joints.partition(joint => joint.shape.isEmpty)
    persistentJoints.length match {
      // All of the seed joints will be persistent
      case l if l >= numSeedJoints => (persistentJoints.take(numSeedJoints),
          persistentJoints.takeRight(l - numSeedJoints) ++ nonPersistentJoints)
      // All persistent joints are used as seed joints, along with some non-persistent joints
      case l =>
          // Sort non-persistent joints by relative position along x axis
          val sortedNonPersistent = nonPersistentJoints.sortWith(_.localOrigin._1 < _.localOrigin._1)
          val numNonPersistent = numSeedJoints - l
          val step = (sortedNonPersistent.length / numNonPersistent.toFloat).toInt + 1
          val seedNonpersistent = (0 to sortedNonPersistent.length by step) collect sortedNonPersistent
          val remainingNonpersistent = sortedNonPersistent.diff(seedNonpersistent)
          (persistentJoints ++ seedNonpersistent, remainingNonpersistent)
    }
  }
}
