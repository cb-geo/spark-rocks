package edu.berkeley.ce.rockslicing

import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable
import scala.io.Source

object RockSlicer {

  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("SparkRocks")
    val sc = new SparkContext(conf)
    val parsedArgs = CommandReader.parseArguments(args)
    if (parsedArgs.isEmpty) {
      System.exit(-1)
    }
    val arguments = CommandReader.parseArguments(args).get

    // Open and read input file specifying rock volume and joints
    val inputSource = Source.fromFile(arguments.inputFile)
    val (rockVolume, joints) = InputProcessor.readInput(inputSource)
    inputSource.close()
    var blocks = Vector(Block((0.0, 0.0, 0.0), rockVolume))

    // Generate a list of initial blocks before RDD-ifying it
    val (seedJoints, remainingJoints) = generateSeedJoints(joints, arguments.numSeedJoints)
    seedJoints foreach { joint =>
      blocks = blocks.flatMap(_.cut(joint, arguments.minRadius, arguments.maxAspectRatio))
    }
    val blockRdd = sc.parallelize(blocks)
    val broadcastJoints = sc.broadcast(remainingJoints)

    // Iterate through the discontinuities, cutting blocks where appropriate
    var cutBlocks = blockRdd
    for (joint <- broadcastJoints.value) {
      cutBlocks = cutBlocks.flatMap(_.cut(joint, arguments.minRadius, arguments.maxAspectRatio))
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

    // Convert list of rock blocks to requested output
    if (arguments.toInequalities) {
      // Convert the list of rock blocks to JSON and save this to a file
      val jsonBlocks = squeakyClean.map(Json.blockToMinimalJson)
      jsonBlocks.saveAsTextFile("blocks.json")
    }
    if (arguments.toVTK) {
      // Convert the list of rock blocks to JSON with vertices, normals and connectivity in format easily converted
      // to vtk my rockProcessor module
      val vtkBlocks = squeakyClean.map(BlockVTK(_))
      val jsonVtkBlocks = vtkBlocks.map(JsonToVtk.blockVtkToMinimalJson)
      jsonVtkBlocks.saveAsTextFile("vtkBlocks.json")
    }
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
        val numNonPersistent = numSeedJoints - len
        // Arrange joints so we prefer elements near middle of rock mass
        val sortedNonPersistent = nonPersistentJoints.sortWith(_.centerX < _.centerX)
        val indices = loadBalanceIndices(sortedNonPersistent.indices).take(numNonPersistent)
        val seedNonPersistent = indices.collect(sortedNonPersistent)
        val remainingNonPersistent = sortedNonPersistent.diff(seedNonPersistent)
        (persistentJoints ++ seedNonPersistent, remainingNonPersistent)
    }
  }

  private def loadBalanceIndices(indices: Seq[Int]): Seq[Int] = {
    def loadBalanceIndicesImpl(queue: mutable.PriorityQueue[Seq[Int]]): Seq[Int] = {
      if (queue.isEmpty) {
        Nil
      } else {
        val s = queue.dequeue()
        val (median, left, right) = removeMedian(s)
        if (left.nonEmpty) {
          queue.enqueue(left)
        }
        if (right.nonEmpty) {
          queue.enqueue(right)
        }
        median +: loadBalanceIndicesImpl(queue)
      }
    }

    def removeMedian[A](s: Seq[A]): (A, Seq[A], Seq[A]) = {
      val medianIndex = s.length / 2
      (s(medianIndex), s.take(medianIndex), s.drop(medianIndex + 1))
    }

    val queue = new mutable.PriorityQueue[Seq[Int]]()(Ordering.by(_.length))
    queue.enqueue(indices)
    loadBalanceIndicesImpl(queue)
  }
}
