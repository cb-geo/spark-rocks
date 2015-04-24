package edu.berkeley.ce.rockslicing

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
    val blocks : List[Block] = Nil // Just a placeholder so we can compile

    // Construct a bounding box for region and introduce articifial "processor joints"
    // Generate a list of initial blocks and change it into an RDD
    val blockRdd = sc.parallelize(blocks)
    // Eliminate any blocks that are contained in the bounding box but not in the actual rock mass

    /*
     * Iterate through the discontinuities, cutting blocks where appropriate and producing
     * a new list of blocks at each step
     */
    // Merge together blocks that are separated only by a processor joint
    // Remove redundant joints as described in the original paper

    // Convert the list of rock blocks to JSON and save this to an output file
    val finalBlocks = blockRdd.collect()
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
