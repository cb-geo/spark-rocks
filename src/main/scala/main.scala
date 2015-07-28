package edu.berkeley.ce.rockslicing

import org.apache.spark.{SparkConf, SparkContext}

import scala.io.Source

object RockSlicer {

  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("SparkRocks")
    val sc = new SparkContext(conf)
    val inputFile = args(0)
    val numberSeedJoints = args(1).toInt

    // Open and read input file specifying rock volume and joints
    val inputSource = Source.fromFile(inputFile)
    val (rockVolume, joints) = inputProcessor.readInput(inputSource)
    inputSource.close()
    var blocks = Vector(Block((0.0, 0.0, 0.0), rockVolume))

    // Generate a list of initial blocks before RDD-ifying it
    val (seedJoints, remainingJoints) = generateSeedJoints(joints, numberSeedJoints)
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
    val (persistentJoints, nonPersistentJoints) = joints.partition(_.shape.isEmpty)
    persistentJoints.length match {
      // All of the seed joints will be persistent
      case len if len >= numSeedJoints => (persistentJoints.take(numSeedJoints),
          persistentJoints.drop(numSeedJoints) ++ nonPersistentJoints)
      // All persistent joints are used as seed joints, along with some non-persistent joints
      case len =>
          // Sort non-persistent joints by relative position along x axis
          val sortedNonPersistent = nonPersistentJoints.sortWith(_.centerX < _.centerX)
          val numNonPersistent = numSeedJoints - len
          val step = sortedNonPersistent.length / numNonPersistent
          val seedNonPersistentCandidates = (sortedNonPersistent.indices by step) collect sortedNonPersistent
          // Still could have too many candidate joints, take middle joints
          val numExtraJoints = seedNonPersistentCandidates.length - numNonPersistent
          val seedNonPersistent = seedNonPersistentCandidates.slice(math.ceil(numExtraJoints/2.0).toInt,
                                                                    len - math.floor(numExtraJoints/2.0).toInt)
          val remainingNonPersistent = sortedNonPersistent.diff(seedNonPersistent)
          (persistentJoints ++ seedNonPersistent, remainingNonPersistent)
    }
  }
}
