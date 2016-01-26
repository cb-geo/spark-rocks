package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.{DenseVector, DenseMatrix}
import scala.annotation.tailrec

/** 
  * Manages initial partitioning of rock volume to maintain load balance among partitions
  */
object LoadBalancer {
  val VOLUME_TOLERANCE = 0.05
  /**
    * Produce processor joints that cut initial rock volume into approximately equal volumes.
    * These joints will be used to seed the initial Block RDD such that the volume or rock
    * provided to each process is approximately equal. This function should only be called when
    * using more than 1 processor.
    * @param rockVolume Initial block defining the entire rock volume of interest
    * @param numProcessors Number of processors used in analysis.
    * @return Set of processor joints that will be used to seed the initial RDD
    */
  def generateProcessorJoints(rockVolume: Block, numProcessors: Integer):
                         Seq[Joint] = {
    // Calculate bounding box
    val vertices = rockVolume.findVertices.values.flatten
    val x_max = (vertices.map(_._1)).max
    val x_min = (vertices.map(_._1)).min
    val y_max = (vertices.map(_._2)).max
    val y_min = (vertices.map(_._2)).min
    val z_max = (vertices.map(_._3)).max
    val z_min = (vertices.map(_._3)).min
    val boundingBox = (x_min, y_min, z_min, x_max, y_max, z_max)

    // Diagonal vector from lower left to upper right corner of bounding box
    val diagonalVector = breeze.linalg.normalize(DenseVector[Double](boundingBox._4 - boundingBox._1,
                                                                     boundingBox._5 - boundingBox._2,
                                                                     boundingBox._6 - boundingBox._3))
    val volumePerPart = rockVolume.volume/numProcessors
    val centroid = rockVolume.centroid
    val centroidVolume = Block(centroid, rockVolume.updateFaces(centroid))
    val processorJoints =
      findProcessorJoints(Seq.empty[Joint], (centroidVolume.centerX, centroidVolume.centerY, centroidVolume.centerZ),
                          (boundingBox._1, boundingBox._2, boundingBox._3),
                          centroidVolume, volumePerPart)


    assert(processorJoints.length + 1 == numProcessors)
    processorJoints
  }

  /**
    * Finds the best set of processor joints to divide initial volume into approximately equal volumes
    * @param joints List to prepend processor joints to during each recursion
    * @param origin Origin which the joint is in reference to
    * @param center Center of the joint plane - start with lower left corner of bounding box
    * @param initialVolume Intial volume that needs to be sub-divided
    * @param desiredVolume Desired volume for each piece cut by processor joints
    * @return List of processor joints that will be used to seed initial RDD
    */
  @tailrec
  private def findProcessorJoints(joints: Seq[Joint],
                                  origin: (Double, Double, Double),
                                  center: (Double, Double, Double), initialVolume: Block,
                                  desiredVolume: Double):
                                  Seq[Joint] = {
    // Calculate bounding box
    val vertices = initialVolume.findVertices.values.flatten
    val x_max = (vertices.map(_._1)).max
    val x_min = (vertices.map(_._1)).min
    val y_max = (vertices.map(_._2)).max
    val y_min = (vertices.map(_._2)).min
    val z_max = (vertices.map(_._3)).max
    val z_min = (vertices.map(_._3)).min
    val boundingBox = (x_min, y_min, z_min, x_max, y_max, z_max)

    // Calculate diagonal length
    val x_diff = boundingBox._4 - boundingBox._1
    val y_diff = boundingBox._5 - boundingBox._2
    val z_diff = boundingBox._6 - boundingBox._3
    val diagonalLength = math.sqrt(x_diff*x_diff + y_diff*y_diff + z_diff*z_diff)
    val normal = linalg.normalize(DenseVector[Double](x_diff, y_diff, z_diff))

    val joint = Joint((normal(0), normal(1), normal(2)), origin, center, phi = 0.0, cohesion = 0.0,
                      shape = Nil, processorJoint = true)
    val blocks = initialVolume.cut(joint)
    val nonRedundantBlocks = blocks.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }
    val sortedBlocks = nonRedundantBlocks.sortWith{ (left, right) =>
      centroidCompare(left.centroid, right.centroid) }

    // If first block has satisfactory volume
    if ((sortedBlocks.head.volume < (1.0 + VOLUME_TOLERANCE)*desiredVolume) && 
        (sortedBlocks.head.volume > (1.0 - VOLUME_TOLERANCE)*desiredVolume) &&
        (sortedBlocks.length == 2)) {
      if (sortedBlocks.tail.head.volume <= (1.0 + VOLUME_TOLERANCE)*desiredVolume) {
        // If both blocks have satifactory volumes, prepend joint to joints
        joint +: joints
      } else {
        // If last block isn't small enough
        val remainingBlock = sortedBlocks.tail.head
        val newCenter = (joint.centerX, joint.centerY, joint.centerZ)
        val centroid = remainingBlock.centroid
        val centroidBlock = Block(centroid, remainingBlock.updateFaces(centroid))
        findProcessorJoints(joint +: joints, origin, newCenter, centroidBlock,
                            desiredVolume)
      }
    // Block volumes not satisfactory, need to iterate to find processor joint
    } else {
      val centerVec0 = DenseVector[Double](center._1, center._2, center._3)
      val centerVec1 = DenseVector[Double](boundingBox._4, boundingBox._5, boundingBox._6)
      // First distance guess
      val x_diff0 = centerVec0(0) - boundingBox._1
      val y_diff0 = centerVec0(1) - boundingBox._2
      val z_diff0 = centerVec0(2) - boundingBox._3
      val dist0 = math.sqrt(x_diff0*x_diff0 + y_diff0*y_diff0 + z_diff0*z_diff0)
      // Second distance guess
      val dist1 = diagonalLength
      val nextCenter = secantBisectionSolver(dist0, dist1, initialVolume, VOLUME_TOLERANCE, 
                                             boundingBox, origin, desiredVolume, 0)
      findProcessorJoints(joints, origin, nextCenter, initialVolume,
                          desiredVolume)
    }
  }

  /**
    * This function iterates along the diagonal of the bounding box of the input block
    * and finds a joint perpendicular to the diagonal that generates two blocks of which
    * the lower left block's volume is specified by the user. The volume is calculated to
    * within the input tolerance.
    * The function implements a combined bisection-secant solver that finds roots to
    * equation: V_current/V_desired - 1.0
    * V_current is the volume of the lower left block cut by the current joint and
    * V_desired is the desired volume of the lower left block. 
    * @param initialDist0 First guess for distance of joint from lower left corner of bounding
    *                     box along diagonal of bounding box
    * @param block Block that is to be subdivided
    * @param tolerance Tolerance for block volume calculations
    * @param boundingBox Bounding box for input block
    * @param origin Origin for input block
    * @param desiredVolume Desired volume of lower left block cut by generated joint
    * @param iterations Number of iterations that have been completed. Used to check that
    *                   maximum number of iterations have not been exceeded
    */
  @tailrec
  private def secantBisectionSolver(initialDist0: Double, initialDist1: Double, block: Block, 
                                    tolerance: Double,
                                    boundingBox: (Double, Double, Double, Double, Double, Double),
                                    origin: (Double, Double, Double), desiredVolume: Double,
                                    iterations: Int):
                                    (Double, Double, Double) = {
    val iterationLimit = 100
    // Calculate diagonal length
    val x_diff = boundingBox._4 - boundingBox._1
    val y_diff = boundingBox._5 - boundingBox._2
    val z_diff = boundingBox._6 - boundingBox._3
    val diagonalLength = math.sqrt(x_diff*x_diff + y_diff*y_diff + z_diff*z_diff)

    // Normal vector for processor joints
    val normal = breeze.linalg.normalize(DenseVector[Double](boundingBox._4 - boundingBox._1,
                                                             boundingBox._5 - boundingBox._2,
                                                             boundingBox._6 - boundingBox._3))
    val lowerLeft = DenseVector[Double](boundingBox._1, boundingBox._2, boundingBox._3)
    // Centers for initial joint guesses
    val center0Vec = lowerLeft + initialDist0*normal
    val center0 = (center0Vec(0), center0Vec(1), center0Vec(2))
    val center1Vec = lowerLeft + initialDist1*normal
    val center1 = (center1Vec(0), center1Vec(1), center1Vec(2))

    // Joint guesses and corresponding blocks
    val joint0 = Joint((normal(0), normal(1), normal(2)), origin, center0, phi = 0.0, cohesion = 0.0,
                       shape = Nil, processorJoint = true)
    val joint1 = Joint((normal(0), normal(1), normal(2)), origin, center1, phi = 0.0, cohesion = 0.0,
                       shape = Nil, processorJoint = true)
    val blocks_0 = block.cut(joint0)
    val blocks_1 = block.cut(joint1)
    val blocks0 = blocks_0.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }
    val blocks1 = blocks_1.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }

    val sortedBlocks0 = blocks0.sortWith{ (left, right) =>
      centroidCompare(left.centroid, right.centroid) }
    val sortedBlocks1 = blocks1.sortWith{ (left, right) =>
      centroidCompare(left.centroid, right.centroid) }

    // Secant step
    val (a, b) = if (initialDist0 < initialDist1) {
      (initialDist0, initialDist1)
    } else {
      (initialDist1, initialDist0)
    }
    val (f_a, f_b) = if (initialDist0 < initialDist1) {
      (sortedBlocks0.head.volume/desiredVolume - 1.0, sortedBlocks1.head.volume/desiredVolume - 1.0)
    } else {
      (sortedBlocks1.head.volume/desiredVolume - 1.0, sortedBlocks0.head.volume/desiredVolume - 1.0)
    }
    val f_prime = (f_b - f_a)/(initialDist1 - initialDist0)
    val secantDist = initialDist1 - f_b/f_prime
    // Bisection step
    val bisectionDist = (initialDist0 + initialDist1)/2.0
    // Select new step
    val newDist = if ((secantDist < a) || (secantDist > b) || (secantDist.isNaN)) {
      bisectionDist
    } else {
      secantDist
    }

    // Update based on new distance
    val newCenterVec = lowerLeft + newDist*normal
    val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
    val newJoint = Joint((normal(0), normal(1), normal(2)), origin, newCenter, phi = 0.0,
                         cohesion = 0.0, shape = Nil, processorJoint = true)
    val new_Blocks = block.cut(newJoint)
    val nonRedundantNew = new_Blocks.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }
    val newBlocks = nonRedundantNew.sortWith { (left, right) =>
      centroidCompare(left.centroid, right.centroid)
    }
    // Value of function at calculated new distance
    val f_p = newBlocks.head.volume/desiredVolume - 1.0

    // Check if initial guesses are within block, otherwise update and retry
    if (sortedBlocks0.length != 2) {
      if ((center0Vec(0) < block.centerX) &&
          (center0Vec(1) < block.centerY) &&
          (center0Vec(2) < block.centerZ)) {
        // Outside lower left portion of block
        val incrementedCenter = center0Vec + 0.01*diagonalLength*normal
        val x_incr = incrementedCenter(0) - boundingBox._1
        val y_incr = incrementedCenter(1) - boundingBox._2
        val z_incr = incrementedCenter(2) - boundingBox._3
        val incrementedDist = 
          if ((x_incr < 0.0) &&
              (y_incr < 0.0) &&
              (z_incr < 0.0)) {
            -math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
          } else {
            math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
          }
        secantBisectionSolver(incrementedDist, initialDist1, block, tolerance,
                              boundingBox, origin, desiredVolume, 0)
      } else {
        // Outside upper right portion of bounding box
        val incrementedCenter = center0Vec - 0.01*diagonalLength*normal
        val x_incr = incrementedCenter(0) - boundingBox._1
        val y_incr = incrementedCenter(1) - boundingBox._2
        val z_incr = incrementedCenter(2) - boundingBox._3
        val incrementedDist = 
          if ((x_incr < 0.0) &&
              (y_incr < 0.0) &&
              (z_incr < 0.0)) {
            -math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
          } else {
            math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
          }
        secantBisectionSolver(incrementedDist, initialDist1, block, tolerance,
                              boundingBox, origin, desiredVolume, 0)
      }

    } else if (sortedBlocks1.length != 2) {
      if ((center1Vec(0) < block.centerX) &&
          (center1Vec(1) < block.centerY) &&
          (center1Vec(2) < block.centerZ)) {
        // Outside lower left portion of block
        val incrementedCenter = center1Vec + 0.01*diagonalLength*normal
        val x_incr = incrementedCenter(0) - boundingBox._1
        val y_incr = incrementedCenter(1) - boundingBox._2
        val z_incr = incrementedCenter(2) - boundingBox._3
        val incrementedDist = 
          if ((x_incr < 0.0) &&
              (y_incr < 0.0) &&
              (z_incr < 0.0)) {
            -math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
          } else {
            math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
          }
        secantBisectionSolver(initialDist0, incrementedDist, block, tolerance,
                              boundingBox, origin, desiredVolume, 0)
      } else {
        // Outside upper right portion of bounding box
        val incrementedCenter = center1Vec - 0.01*diagonalLength*normal
        val x_incr = incrementedCenter(0) - boundingBox._1
        val y_incr = incrementedCenter(1) - boundingBox._2
        val z_incr = incrementedCenter(2) - boundingBox._3
        val incrementedDist = 
          if ((x_incr < 0.0) &&
              (y_incr < 0.0) &&
              (z_incr < 0.0)) {
            -math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
          }
          else {
            math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
          }
        secantBisectionSolver(initialDist0, incrementedDist, block, tolerance,
                              boundingBox, origin, desiredVolume, 0)
      }
    } else if ((math.abs(newBlocks.head.volume/desiredVolume - 1.0) < tolerance) ||
               (iterations > iterationLimit)) {
      // Either gives appropriate volume of iteration limit has been reached
      if (iterations > iterationLimit) println("Unable to converge within iteration limit, "+
                                               "returning result of last iteration")
      (newJoint.centerX, newJoint.centerY, newJoint.centerZ)
    } else if (f_p * f_b < 0.0) { // If f_p and f_b have opposite signs, there is a root between
                                  // the new distance and b, so do another iteration with lower
                                  // bound of interval updated
      secantBisectionSolver(newDist, b, block, tolerance, boundingBox, origin,
                            desiredVolume, iterations + 1)
    } else { // Root beteen a and new distancse, so do another iteration with upper bound of
             // interval updated
      secantBisectionSolver(a, newDist, block, tolerance, boundingBox, origin,
                            desiredVolume, iterations + 1)
    }
  }

  /**
    * Compares the relative location of two input centroids
    * @param centroid1 First centroid, input as a tuple
    * @param centroid2 Second centroid, input as a tuple
    * @return True if centroid1 is less positive than centroid2, false otherwise
    */
  private def centroidCompare(centroid1:(Double, Double, Double), centroid2:(Double, Double, Double)):
                             Boolean = {
    (centroid1._1 < centroid2._1) && (centroid1._2 < centroid2._2) && (centroid1._3 < centroid2._3)
  }
}
