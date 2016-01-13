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
                          (rockVolume.centerX, rockVolume.centerY, rockVolume.centerZ), rockVolume, volumePerPart,
                          boundingBox, numSeedJoints)

    if (seedJoints.length != numSeedJoints) {
      throw new IllegalStateException("ERROR: LoadBalancer.generateSeedJoints unable to find optimal "+
                                      "seed joints")
    }
    seedJoints
  }

  /**
    * Finds the best set of processor joints to divide initial volume into approximately equal volumes
    * @param joints List to prepend processor joints to during each recursion
    * @param normal Desired normal vector to the processor joint planes
    * @param origin Origin which the joint is in reference to
    * @param center Center of the joint plane - start with block origin
    * @param initialVolume Intial volume that needs to be sub-divided
    * @param desiredVolume Desired volume for each piece cut by processor joints
    * @param diagonalLength Length of diagonal of bounding box
    * @param boundingBox Bounding box of the initial rock volume
    * @return List of processor joints that will be used to seed initial RDD
    */
  private def findProcessorJoints(joints: Seq[Joint], normal: DenseVector[Double],
                                  origin: (Double, Double, Double),
                                  center: (Double, Double, Double), initialVolume: Block,
                                  desiredVolume: Double,
                                  boundingBox: (Double, Double, Double, Double, Double, Double),
                                  seeds: Int):
                                  Seq[Joint] = {
    // Calculate diagonal length
    val x_diff = boundingBox._4 - boundingBox._1
    val y_diff = boundingBox._5 - boundingBox._2
    val z_diff = boundingBox._6 - boundingBox._3
    val diagonalLength = math.sqrt(x_diff*x_diff + y_diff*y_diff + z_diff*z_diff)

    val joint = Joint((normal(0), normal(1), normal(2)), origin, center, phi = 0.0, cohesion = 0.0,
                      shape = Nil, artificialJoint = Some(true))
    val blocks = initialVolume.cut(joint)
    val nonRedundantBlocks = blocks.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }
    val tolerance = 0.05

    // If first block has satisfactory volume
    if ((nonRedundantBlocks.head.volume < (1.0 + tolerance)*desiredVolume) && 
        (nonRedundantBlocks.head.volume > (1.0 - tolerance)*desiredVolume) &&
        (nonRedundantBlocks.length == 2)) {
      if (nonRedundantBlocks.tail.head.volume <= (1.0 + tolerance)*desiredVolume) {
        // If both blocks have satifactory volumes, prepend joint to joints
        joint +: joints
      } else {
        // If last block isn't small enough
        val remainingBlock = nonRedundantBlocks.tail.head
        val newCenter = (joint.centerX + 0.1, joint.centerY + 0.1, joint.centerZ + 0.1)
        findProcessorJoints(joints, normal, origin, newCenter, remainingBlock,
                            desiredVolume, boundingBox, seeds)
      }
    // Block volumes not satisfactory, need to iterate to find processor joint
    } else {
      println("None satisfactory")
      val centerVec0 = DenseVector[Double](center._1, center._2, center._3)
      val centerVec1 = DenseVector[Double](boundingBox._4, boundingBox._5, boundingBox._6)
      // First distance guess
      val x_diff0 = boundingBox._4 - centerVec0(0)
      val y_diff0 = boundingBox._5 - centerVec0(1)
      val z_diff0 = boundingBox._6 - centerVec0(2)
      val dist0 = diagonalLength - math.sqrt(x_diff0*x_diff0 + y_diff0*y_diff0 + z_diff0*z_diff0)
      // Second distance guess
      val x_diff1 = boundingBox._4 - centerVec1(0)
      val y_diff1 = boundingBox._5 - centerVec1(1)
      val z_diff1 = boundingBox._6 - centerVec1(2)
      val dist1 = diagonalLength - math.sqrt(x_diff1*x_diff1 + y_diff1*y_diff1 + z_diff1*z_diff1)
      val newCenter = bisectionSolver(dist0, dist1, initialVolume, tolerance, seeds, 
                                            boundingBox, origin, desiredVolume, 0)
      findProcessorJoints(joints, normal, origin, newCenter, initialVolume,
                          desiredVolume, boundingBox, seeds)
    }
  }

  def bisectionSolver(initialDist0: Double, initialDist1: Double, block: Block, 
                                    tolerance: Double, numSeeds: Int,
                                    boundingBox: (Double, Double, Double, Double, Double, Double),
                                    origin: (Double, Double, Double), desiredVolume: Double,
                                    iterations: Int):
                                    (Double, Double, Double) = {
    // println("Number of iterations: "+iterations)
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
                       shape = Nil, artificialJoint = Some(true))
    val joint1 = Joint((normal(0), normal(1), normal(2)), origin, center1, phi = 0.0, cohesion = 0.0,
                       shape = Nil, artificialJoint = Some(true))
    val blocks_0 = block.cut(joint0)
    val blocks_1 = block.cut(joint1)
    val blocks0 = blocks_0.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }
    val blocks1 = blocks_1.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }

    // Check if both guesses produce 2 blocks each
    if (blocks0.length != 2) {
      // Joint outside lower left corner of bounding box
      if ((center0._1 < boundingBox._4) &&
        (center0._2 < boundingBox._5) &&
        (center0._3 < boundingBox._6)) {
        // println("Block0: Lower left")
        val centerVec = DenseVector[Double](center0._1, center0._2, center0._3)
        val newCenterVec = centerVec + 0.25*diagonalLength*normal
        val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
        val x_change = newCenter._1 - boundingBox._1
        val y_change = newCenter._2 - boundingBox._2
        val z_change = newCenter._3 - boundingBox._3
        val newDistance = math.sqrt(x_change*x_change + y_change*y_change + z_change*z_change)
        bisectionSolver(newDistance, initialDist1, block, tolerance, numSeeds, 
                              boundingBox, origin, desiredVolume, iterations)
      // Joint outside upper left corner of bounding box
      } else {
        // println("Block0: Upper right")
        val centerVec = DenseVector[Double](center0._1, center0._2, center0._3)
        val newCenterVec = centerVec - 0.25*diagonalLength*normal
        val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
        val x_change = newCenter._1 - boundingBox._1
        val y_change = newCenter._2 - boundingBox._2
        val z_change = newCenter._3 - boundingBox._3
        val newDistance = math.sqrt(x_change*x_change + y_change*y_change + z_change*z_change)
        bisectionSolver(newDistance, initialDist1, block, tolerance, numSeeds,
                              boundingBox, origin, desiredVolume, iterations)
      }
    }
    if (blocks1.length != 2) {
      // Joint outside lower left corner of bounding box
      if ((center1._1 < boundingBox._4) &&
        (center1._2 < boundingBox._5) &&
        (center1._3 < boundingBox._6)) {
        // println("Block1: Lower left")
        val centerVec = DenseVector[Double](center1._1, center1._2, center1._3)
        val newCenterVec = centerVec + 0.25*diagonalLength*normal
        val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
        val x_change = newCenter._1 - boundingBox._1
        val y_change = newCenter._2 - boundingBox._2
        val z_change = newCenter._3 - boundingBox._3
        val newDistance = math.sqrt(x_change*x_change + y_change*y_change + z_change*z_change)
        bisectionSolver(initialDist0, newDistance, block, tolerance, numSeeds,
                              boundingBox, origin, desiredVolume, iterations)
      // Joint outside upper right corner of bounding box 
      } else {
        // println("Block1: Upper right")
        val centerVec = DenseVector[Double](center1._1, center1._2, center1._3)
        val newCenterVec = centerVec - 0.25*diagonalLength*normal
        val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
        val x_change = newCenter._1 - boundingBox._1
        val y_change = newCenter._2 - boundingBox._2
        val z_change = newCenter._3 - boundingBox._3
        val newDistance = math.sqrt(x_change*x_change + y_change*y_change + z_change*z_change)
        bisectionSolver(initialDist0, newDistance, block, tolerance, numSeeds,
                              boundingBox, origin, desiredVolume, iterations)
      }
    }

    val updatedDist = (initialDist0 + initialDist1)/2.0
    val updatedCenterVec = lowerLeft + updatedDist*normal
    val updatedCenter = (updatedCenterVec(0), updatedCenterVec(1), updatedCenterVec(2))
    val updatedJoint = Joint((normal(0), normal(1), normal(2)), origin, updatedCenter, phi = 0.0,
      cohesion = 0.0, shape = Nil, artificialJoint = Some(true))
    val updated_Blocks = block.cut(updatedJoint)
    val updatedBlocks = updated_Blocks.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }
    val b = Seq(initialDist0, initialDist1).max
    val a = Seq(initialDist0, initialDist1).min
    val f_p = updatedBlocks.head.volume/desiredVolume - 1.0
    val f_b = if (initialDist1 > initialDist0) {
      blocks1.head.volume/desiredVolume - 1.0
    } else {
      blocks0.head.volume/desiredVolume - 1.0
    }

    // println("f_bisection: "+f_p)
    // println("bisection dist: "+updatedDist)
    // Bi-section gives appropriate volumes
    if ((math.abs(updatedBlocks.head.volume/desiredVolume - 1.0) < tolerance) ||
        (iterations > iterationLimit)) {
      // println("Bi-section: This is the volume: "+updatedBlocks.head.volume)
      if (iterations > iterationLimit) println("Unable to converge within iteration limit, "+
                                               "returning result of last iteration")
      (updatedJoint.centerX, updatedJoint.centerY, updatedJoint.centerZ)
      // Do another iteration with interval updated using bi-section - lower bound
    } else if (f_p * f_b < 0.0) {
      bisectionSolver(updatedDist, b, block, tolerance, numSeeds,
        boundingBox, origin, desiredVolume, iterations + 1)
      // Do another iteration with interval updated using bi-section - upper bound
    } else {
      bisectionSolver(a, updatedDist, block, tolerance, numSeeds,
        boundingBox, origin, desiredVolume, iterations + 1)
    }
  }
}


