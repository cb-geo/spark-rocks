package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.{DenseVector, DenseMatrix}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import util.control.Breaks.{break, breakable}

/** 
  * Manages initial partitioning of rock volume to maintain load balance among partitions
  */
object LoadBalancer {

  /**
    * Produce two sets of joints from input joint sets. The first set will be used to seed the
    * initial Block RDD such that the volume or rock provided to each process is approximately
    * equal. The second set will be the remaining joints that need to be processed by each
    * process individually.
    * @param joints Input joints for rock volume of interest
    * @param rockVolume Initial block defining the entire rock volume of interest
    * @param numSeedJoints Number of joints to process before initiating parallel block cutting
    * @param boundingBox Tuple that contains the coordinates of the lower left and upper right
    *                    corners of a box bounding the rock volume of interest
    * @param minRadius The minimum radius of a sphere that can be inscribed in the child blocks.
    *                  If either child block falls below this minimum, no cut is performed.
    * @param maxAspectRatio The maximum ratio of a child block's bounding sphere to the radius
    *                       of the largest sphere that can be inscribed in the block. If either
    *                       child falls above this minimum, no cut is performed.
    * @return Two joint sets. The first set are the joints that should be processed before 
    *         initiating parallel context. The second set are the joints that should be processed
    *         by each process individually
    */
  def generateSeedJoints(joints: Seq[Joint], rockVolume: Block, numSeedJoints: Integer,
                         boundingBox: (Double, Double, Double, Double, Double, Double),
                         minRadius: Double, maxAspectRatio: Double):
                        (Seq[Joint], Seq[Joint]) = {

    if (joints.length < numSeedJoints) {
      throw new IllegalArgumentException("ERROR: Input to LoadBalancer.generateSeedJoints: "+
                                         "Number of seed joints exceeds total number of joints")
    }

    val (persistentJoints, nonPersistentJoints) = joints.partition(_.shape.isEmpty)  
    // Diagonal vector from lower left to upper right corner of bounding box
    val diagonalVector = breeze.linalg.normalize(DenseVector[Double](boundingBox._4 - boundingBox._1,
                                                                     boundingBox._5 - boundingBox._2,
                                                                     boundingBox._6 - boundingBox._3))
    // Sort according to distance of joint plane from origin. Assumes input joint origins are the same
    // as global origin
    val distSortedPersistent = persistentJoints.sortWith(_.d < _.d)
    // Sort such that normals of joint planes go from more to less parallel to diagonal of bounding box
    val sortedPersistent =
      distSortedPersistent.sortWith(jointDotProduct(_, diagonalVector) > jointDotProduct(_, diagonalVector))

    // Sort non-persistent joints similarly to how persistent joints were sorted above
    val distSortedNonPersistent = nonPersistentJoints.sortWith(_.d < _.d)
    val sortedNonPersistent = 
      distSortedNonPersistent.sortWith(jointDotProduct(_, diagonalVector) > jointDotProduct(_, diagonalVector))
    val allJoints = sortedPersistent ++ sortedNonPersistent

    val volumePerPart = rockVolume.volume/(numSeedJoints + 1)
    var remainingBlock = rockVolume
    val seedJoints = new ArrayBuffer[Joint]()
    val remainingJoints = new Queue[Joint]
    // First add persistent then non-persistent - this ensures that persistent joints are first checked
    // as seed joints before trying non-persistent
    remainingJoints ++= sortedPersistent
    remainingJoints ++= sortedNonPersistent
   
    while ((remainingBlock.volume > 1.1*volumePerPart) && (seedJoints.length != numSeedJoints)) {
      // Check if all joints have already been checked. If so, unable to find optimal load balancing
      // for selected number of seed joints
      if (remainingJoints.isEmpty) {
        throw new IllegalStateException("ERROR: LoadBalancer unable to find optimal seed joints to"+
                                        " maintain balanced load among processes.")
      }

      // If searching for last seed joints, save volume of remaining block
      if (seedJoints.length == numSeedJoints - 1) {
        val remainingVolume = remainingBlock.volume
      }

      breakable {
        while (!remainingJoints.isEmpty) {
          val joint = remainingJoints.dequeue
          val blocks = remainingBlock.cut(joint, minRadius, maxAspectRatio)
          // Remove redundant faces
          val nonRedundantBlocks = blocks.map { case block @ Block(center, _) =>
            Block(center, block.nonRedundantFaces)
          }

          val volumeBlocks = nonRedundantBlocks.sortWith(_.volume < _.volume)
          // Check if smallest block's volume falls within 0.9 to 1.1 of the ideal volume per part
          // Also, check that the largest block's volume does not fall below 0.9 of the ideal vol.
          if ((volumeBlocks(0).volume < 1.1*volumePerPart) &&
            (volumeBlocks(0).volume > 0.9*volumePerPart) &&
            (volumeBlocks(1).volume > 0.9*volumePerPart)) {
              seedJoints += joint
              remainingBlock = volumeBlocks(1)
              break()
          }
        }
      }
    }
   
    // Check that number of seed joints matches those requested by user
    // if (seedJoints.length != numSeedJoints) {
    //   throw new IllegalStateException("ERROR: Number of seed joints generated by LoadBalancer."+
    //                                   "generateSeedJoints did not match requested number: "+
    //                                   "May lead to possible load imbalance between processes")
    // }

    (seedJoints, allJoints.diff(seedJoints))
  }   

  /**
    * Calculates the absolute value of the dot product of a joint's normal with another unit vector
    * @param joint Joint whose normal is to be used in dot product
    * @param vector Vector to use in dot product
    * @return Dot product of joint's normal and input vector
    */
  private def jointDotProduct(joint: Joint, vector: DenseVector[Double]): Double = {
    val normal = DenseVector[Double](joint.a, joint.b, joint.c)
    math.abs(normal dot vector)
  }
}
