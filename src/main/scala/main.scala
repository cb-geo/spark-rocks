package edu.berkeley.ce.rockslicing

import scala.collection.mutable.ListBuffer
import scala.io.Source
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf

object RockSlicer {
  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("CS 267 Final Project")
    val sc = new SparkContext(conf)
    val inputFile = args(0)
    val numberPartitions = args(1).toInt

    // Open and read input file specifying rock volume and joints
    var rockBuffer = new ListBuffer[Face]()
    var jointBuffer = new ListBuffer[Joint]()
    val inputSource = Source.fromFile(inputFile) // Input file name
    try {
      inputProcessor.readInput(inputSource, rockBuffer, jointBuffer)
    } finally {
      inputSource.close
    }
    val rockVolume = rockBuffer.toList
    val jointList = jointBuffer.toList
    var blocks = List(Block((0.0, 0.0, 0.0), rockVolume))

    // Generate a list of initial blocks before RDD-ifying it -- not very clean code
    var joints = jointList
    while (blocks.length < numberPartitions && !joints.isEmpty) {
      blocks = blocks.flatMap(_.cut(joints.head))
      joints = joints.tail
    }
    val blockRdd = sc.parallelize(blocks)
    val broadcastJoints = sc.broadcast(joints)

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
}
