package edu.berkeley.ce.rockslicing

import scala.collection.mutable.ListBuffer
import java.io._
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf

object RockSlicer {
  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("CS 267 Final Project")
    val sc = new SparkContext(conf)

    /* Open and read input file, producing list of linear inequalities specifying region and
     * list of linear equalities specifying discontinuities
     */
    var rockBuffer = new ListBuffer[Face]()
    var jointBuffer = new ListBuffer[Joint]()
    val inputFile = "tester" // Input file name
    inputProcessor.readInput(inputFile, rockBuffer, jointBuffer)
 
    val rockVolume = rockBuffer.toList 
    val jointList = jointBuffer.toList

    println(rockVolume mkString "\n") // Print input to verify during debugging
    println(jointBuffer mkString "\n")
    println("Input has been processed") 

    // Just placeholders so we can compile
    val joints: List[Joint] = Nil
    val blocks : List[Block] = Nil

    val broadcastJoints = sc.broadcast(joints)
    // Construct a bounding box for region and introduce articifial "processor joints"
    // Generate a list of initial blocks and change it into an RDD
    val blockRdd = sc.parallelize(blocks)
    // Eliminate any blocks that are contained in the bounding box but not in the actual rock mass

    /*
     * Iterate through the discontinuities, cutting blocks where appropriate and producing
     * a new list of blocks at each step
     */
    var cutBlocks = blockRdd
    for (joint <- broadcastJoints.value) {
      cutBlocks = cutBlocks.flatMap {
        case block @ Block(center, faces) =>
          if (block intersects joint) {
            List(Block(center, Face((joint.a, joint.b, joint.c), joint.d, joint.phi, joint.cohesion)::faces),
                 Block(center, Face((-joint.a, -joint.b, -joint.c), joint.d, joint.phi, joint.cohesion)::faces))
          } else {
            List(block)
          }
      }
    }

    // Merge together blocks that are separated only by a processor joint

    // Remove redundant joints as described in the original paper
    val nonRedundantBlocks = cutBlocks.map { case block @ Block(center, _) =>
                                             Block(center, block.nonRedundantFaces) }

    // Convert the list of rock blocks to JSON and save this to an output file
    val finalBlocks = nonRedundantBlocks.collect()
    val blockJson = json.rockBlocksToReadableJson(finalBlocks)
    val outputFile = "blocks.json"
    val writer = new BufferedWriter(new FileWriter(outputFile))
    try {
      writer.write(blockJson)
    } catch {
        case e: Exception => {
          println(s"Error writing to file: $e")
        }
    } finally {
      writer.close()
    }
  }
}
