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

    val volumePerPart = rockVolume.volume/(numSeedJoints + 1)
    val seedJoints =
      findProcessorJoints(Seq[Joint](), diagonalVector, (rockVolume.centerX, rockVolume.centerY, rockVolume.centerZ),
                          (boundingBox._1, boundingBox._2, boundingBox._3), rockVolume, volumePerPart)

    if (seedJoints.length != numSeedJoints) {
      throw new IllegalStateException("ERROR: LoadBalancer.generateSeedJoints unable to find optimal "+
                                      "seed joints")
    }
    seedJoints
  }

  private def findProcessorJoints(joints: Seq[Joint], normal: DenseVector[Double],
                                 origin: (Double, Double, Double),
                                 center: (Double, Double, Double), initialVolume: Block,
                                 desiredVolume: Double): Seq[Joint] = {
    val joint = Joint((normal(0), normal(1), normal(2)), origin, center, phi = 0.0, cohesion = 0.0,
                      shape = Nil, artificialJoint = Some(true))
    val blocks = initialVolume.cut(joint)

    // If first block has satisfactory volume
    if ((blocks.head.volume < 1.05*desiredVolume) && 
        (blocks.head.volume > 0.95*desiredVolume) &&
        (blocks.length == 2)) {
      if (blocks.tail.head.volume <= 1.05*desiredVolume) {
        // If both blocks have satifactory volumes, prepend joint to joints
        joint +: joints
      } else {
        // If last block isn't small enough
        val remainingBlock = blocks.tail.head
        findProcessorJoints(joints, normal, origin, center, remainingBlock, desiredVolume)
      }
    } else if (blocks.head.volume > 1.05*desiredVolume) {
      // If cut volume of first block is too big
      val centerVec = DenseVector[Double](center._1, center._2, center._3)
      val newCenterVec = centerVec - (blocks.head.volume/desiredVolume - 1.0)*normal
      val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
      findProcessorJoints(joints, normal, origin, newCenter, initialVolume, desiredVolume)
    } else {
      // If cut volume is first block is too small
      val centerVec = DenseVector[Double](center._1, center._2, center._3)
      val newCenterVec = centerVec + (desiredVolume/blocks.head.volume - 1.0)*normal
      val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
      findProcessorJoints(joints, normal, origin, newCenter, initialVolume, desiredVolume)
    }
  }
}
