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
                          (boundingBox._1, boundingBox._2, boundingBox._3), rockVolume, volumePerPart,
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
    * @param center Center of the joint plane - should start with lower left corner of bounding box
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
    val cornerBound = (boundingBox._4, boundingBox._5, boundingBox._6)

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
        val newCenter = (joint.centerX, joint.centerY, joint.centerZ)
        findProcessorJoints(joints, normal, origin, newCenter, remainingBlock,
                            desiredVolume, boundingBox, seeds)
      }
    // If cut volume of first block is too big
    } else if ((nonRedundantBlocks.head.volume > 1.05*desiredVolume) &&
               (nonRedundantBlocks.length == 2)) {
      println("TOO BIG!!")
      val centerVec0 = DenseVector[Double](center._1, center._2, center._3)
      val centerVec1 = centerVec0 - (1.0/(seeds + 5.0))*diagonalLength*normal
      // First distance guess
      val x_diff0 = cornerBound._1 - centerVec0(0)
      val y_diff0 = cornerBound._2 - centerVec0(1)
      val z_diff0 = cornerBound._3 - centerVec0(2)
      val dist0 = diagonalLength - math.sqrt(x_diff0*x_diff0 + y_diff0*y_diff0 + z_diff0*z_diff0)
      // Second distance guess
      val x_diff1 = cornerBound._1 - centerVec1(0)
      val y_diff1 = cornerBound._2 - centerVec1(1)
      val z_diff1 = cornerBound._3 - centerVec1(2)
      val dist1 = diagonalLength - math.sqrt(x_diff1*x_diff1 + y_diff1*y_diff1 + z_diff1*z_diff1)
      val newCenter = secantSolver(dist0, dist1, initialVolume, tolerance, boundingBox,
                                   origin, desiredVolume, 0)
      findProcessorJoints(joints, normal, origin, newCenter, initialVolume,
                          desiredVolume, boundingBox, seeds)
    // If cut volume of first block is too small
    } else if (nonRedundantBlocks.length == 2) {
      println("TOO SMALL!!")
      val centerVec0 = DenseVector[Double](center._1, center._2, center._3)
      val centerVec1 = centerVec0 + (1.0/(seeds + 5.0))*diagonalLength*normal
      // First distance guess
      val x_diff0 = cornerBound._1 - centerVec0(0)
      val y_diff0 = cornerBound._2 - centerVec0(1)
      val z_diff0 = cornerBound._3 - centerVec0(2)
      val dist0 = diagonalLength - math.sqrt(x_diff0*x_diff0 + y_diff0*y_diff0 + z_diff0*z_diff0)
      // Second distance guess
      val x_diff1 = cornerBound._1 - centerVec1(0)
      val y_diff1 = cornerBound._2 - centerVec1(1)
      val z_diff1 = cornerBound._3 - centerVec1(2)
      val dist1 = diagonalLength - math.sqrt(x_diff1*x_diff1 + y_diff1*y_diff1 + z_diff1*z_diff1)
      val newCenter = secantSolver(dist0, dist1, initialVolume, tolerance, boundingBox,
                                   origin, desiredVolume, 0)
      findProcessorJoints(joints, normal, origin, newCenter, initialVolume,
                          desiredVolume, boundingBox, seeds)
    // Joint is outside block - lower left corner
    } else if ((center._1 < cornerBound._1) &&
               (center._2 < cornerBound._2) &&
               (center._3 < cornerBound._3)) {
      println("Lower left")
      val centerVec = DenseVector[Double](center._1, center._2, center._3)
      val newCenterVec = centerVec + (1.0/(seeds + 5.0))*diagonalLength*normal
      val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
      // println("New center: "+newCenter)
      // println("Diagonal length: "+diagonalLength)
      // println("This is the initial centerVec: "+centerVec)
      // println("This is the normal: "+normal)
      findProcessorJoints(joints, normal, origin, newCenter, initialVolume,
                          desiredVolume, boundingBox, seeds)
    // Joint is outside block - upper right corner
    } else {
      println("Upper right")
      val centerVec = DenseVector[Double](center._1, center._2, center._3)
      val newCenterVec = centerVec - (1.0/(seeds + 5.0))*diagonalLength*normal
      val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
      println(newCenter)
      findProcessorJoints(joints, normal, origin, newCenter, initialVolume,
                          desiredVolume, boundingBox, seeds)
    }
  }

  private def secantSolver(initialDist0: Double, initialDist1: Double, block: Block, tolerance: Double,
                           boundingBox: (Double, Double, Double, Double, Double, Double),
                           origin: (Double, Double, Double), desiredVolume: Double, iterations: Int):
                           (Double, Double, Double) = {

    val iterationLimit = 100
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

    if ((blocks_0.length != 2) || (blocks_1.length != 2)) {
      throw new IllegalArgumentException("ERROR: Bad initial guesses given as inputs to "+
                                         "LoadBalancer.secantSolver")
    }

    // Remove redundant faces before calculating volumes
    val blocks0 = blocks_0.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }
    val blocks1 = blocks_1.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }
    println("Well hello!!")
    // Update distance using secant method
    val f_prime = (blocks1.head.volume - blocks0.head.volume)/(initialDist1 - initialDist0)
    println("Look at those volumes!")
    val newDist = initialDist1 - blocks1.head.volume/f_prime
    val newCenterVec = lowerLeft + newDist*normal
    val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
    println("Calc'd new center: "+newCenter)
    println("Last volume calc: "+blocks1.head.volume)
    println("This is the desired volume: "+desiredVolume)
    val newJoint = Joint((normal(0), normal(1), normal(2)), origin, newCenter, phi = 0.0, cohesion = 0.0,
                         shape = Nil, artificialJoint = Some(true))
    println("Constructed new joint")
    val new_Blocks = block.cut(newJoint)

    // Remove redundant faces before calculating volumes
    val newBlocks = new_Blocks.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }

    // Check if new joint gives appropriate volume otherwise, do another iteration
    println("Lets calc some more volumes")
    if ((math.abs(newBlocks.head.volume/desiredVolume - 1.0) < tolerance) ||
        (iterations == iterationLimit)) {
      if (iterations == iterationLimit) println("MAXIMUM NUMBER OF ITERATIONS REACHED, RETURNING"+
                                                " LAST ITERATION RESULTS")
      println("Secant found solution, exiting")
      (newJoint.centerX, newJoint.centerY, newJoint.centerZ)
    } else {
      println("Let's try one more iteration")
      secantSolver(initialDist1, newDist, block, tolerance, boundingBox, origin,
                   desiredVolume, iterations + 1)
    }
  }
}
