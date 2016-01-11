package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.{DenseVector, DenseMatrix}

/** 
  * Manages initial partitioning of rock volume to maintain load balance among partitions
  */
object LoadBalancer {

  /**
    * Produce processor joints that cut initial rock volume into approximately equal volumes.
    * These joints will be used to seed the initial Block RDD such that the volume or rock
    * provided to each process is approximately equal.
    * @param rockVolume Initial block defining the entire rock volume of interest
    * @param numSeedJoints Number of joints to process before initiating parallel block cutting
    * @param boundingBox Tuple that contains the coordinates of the lower left and upper right
    *                    corners of a box bounding the rock volume of interest
    * @param forceBalancer Force the load balancer to provide seed joints regardless of load imbalance
    * @return Set of processor joints that will be used to seed the initial RDD
    */
  def generateSeedJoints(rockVolume: Block, numSeedJoints: Integer,
                         boundingBox: (Double, Double, Double, Double, Double, Double),
                         forceBalancer: Boolean):
                        Seq[Joint] = {

    // Diagonal vector from lower left to upper right corner of bounding box
    val diagonalVector = breeze.linalg.normalize(DenseVector[Double](boundingBox._4 - boundingBox._1,
                                                                     boundingBox._5 - boundingBox._2,
                                                                     boundingBox._6 - boundingBox._3))
    val x_diff = boundingBox._4 - boundingBox._1
    val y_diff = boundingBox._5 - boundingBox._2
    val z_diff = boundingBox._6 - boundingBox._3
    val diagonalLength = math.sqrt(x_diff*x_diff + y_diff*y_diff + z_diff*z_diff)
    println("This is the diagonal length: "+diagonalLength)
    println("This is the diangonal vector: "+diagonalVector)
    val centerVec = DenseVector[Double](boundingBox._1, boundingBox._2, boundingBox._3)
    println("This is the center: "+centerVec)
    val increment = centerVec + 0.05*diagonalLength*diagonalVector
    println("This is the incremented value: "+increment)
    println("This is the bounding box: "+boundingBox)
    val inc2 = increment + 0.05*diagonalLength*diagonalVector
    println("This is the second increment: "+inc2)

    val volumePerPart = rockVolume.volume/(numSeedJoints + 1)
    val seedJoints =
      findProcessorJoints(Seq[Joint](), diagonalVector, (rockVolume.centerX, rockVolume.centerY, rockVolume.centerZ),
                          (boundingBox._1, boundingBox._2, boundingBox._3), rockVolume, volumePerPart,
                          diagonalLength, (boundingBox._4, boundingBox._5, boundingBox._6))

    if (seedJoints.length != numSeedJoints) {
      throw new IllegalStateException("ERROR: LoadBalancer.generateSeedJoints unable to find optimal "+
                                      "seed joints")
    }
    seedJoints
  }

  private def findProcessorJoints(joints: Seq[Joint], normal: DenseVector[Double],
                                 origin: (Double, Double, Double),
                                 center: (Double, Double, Double), initialVolume: Block,
                                 desiredVolume: Double, diagonalLength: Double,
                                 cornerBound: (Double, Double, Double)): Seq[Joint] = {
    val joint = Joint((normal(0), normal(1), normal(2)), origin, center, phi = 0.0, cohesion = 0.0,
                      shape = Nil, artificialJoint = Some(true))
    val blocks = initialVolume.cut(joint)

    val nonRedundantBlocks = blocks.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }


    // If first block has satisfactory volume
    if ((nonRedundantBlocks.head.volume < 1.1*desiredVolume) && 
        (nonRedundantBlocks.head.volume > 0.9*desiredVolume) &&
        (nonRedundantBlocks.length == 2)) {
      if (nonRedundantBlocks.tail.head.volume <= 1.05*desiredVolume) {
        // If both blocks have satifactory volumes, prepend joint to joints
        joint +: joints
      } else {
        // If last block isn't small enough
        val remainingBlock = nonRedundantBlocks.tail.head
        findProcessorJoints(joints, normal, origin, center, remainingBlock,
                            desiredVolume, diagonalLength, cornerBound)
      }
    } else if ((nonRedundantBlocks.head.volume > 1.05*desiredVolume) &&
               (nonRedundantBlocks.length == 2)) {
      // If cut volume of first block is too big
      println("TOO BIG!!")
      val centerVec = DenseVector[Double](center._1, center._2, center._3)
      // val newCenterVec = centerVec - (nonRedundantBlocks.head.volume/desiredVolume - 1.0)*normal
      val newCenterVec = centerVec - (nonRedundantBlocks.head.volume/desiredVolume - 1.0)*normal
      val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
      findProcessorJoints(joints, normal, origin, newCenter, initialVolume,
                          desiredVolume, diagonalLength, cornerBound)
    } else if (nonRedundantBlocks.length == 2) {
      // If cut volume is first block is too small
      println("TOO SMALL!!")
      val centerVec = DenseVector[Double](center._1, center._2, center._3)
      val newCenterVec = centerVec + (desiredVolume/nonRedundantBlocks.head.volume - 1.0)*normal
      val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
      findProcessorJoints(joints, normal, origin, newCenter, initialVolume,
                          desiredVolume, diagonalLength, cornerBound)
    } else if ((center._1 < cornerBound._1) &&
               (center._2 < cornerBound._2) &&
               (center._3 < cornerBound._3)) {
      // Joint is outside block
      println("Lower left")
      val centerVec = DenseVector[Double](center._1, center._2, center._3)
      val newCenterVec = centerVec + 0.05*diagonalLength*normal
      val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
      findProcessorJoints(joints, normal, origin, newCenter, initialVolume,
                          desiredVolume, diagonalLength, cornerBound)
    } else {
      println("Upper right")
      val centerVec = DenseVector[Double](center._1, center._2, center._3)
      val newCenterVec = centerVec - 0.05*diagonalLength*normal
      val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
      println(newCenter)
      findProcessorJoints(joints, normal, origin, newCenter, initialVolume,
                          desiredVolume, diagonalLength, cornerBound)
    }
  }
}
