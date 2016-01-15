package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.{DenseVector, DenseMatrix}
import scala.annotation.tailrec

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
      findProcessorJoints(Seq[Joint](), (boundingBox._1, boundingBox._2, boundingBox._3),
                          (rockVolume.centerX, rockVolume.centerY, rockVolume.centerZ), rockVolume, volumePerPart)

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
  @tailrec
  private def findProcessorJoints(joints: Seq[Joint],
                                  origin: (Double, Double, Double),
                                  center: (Double, Double, Double), initialVolume: Block,
                                  desiredVolume: Double):
                                  Seq[Joint] = {
    println("STARTING CENTER: "+center)
    // Calculate bounding box
    val vertices = initialVolume.findVertices.values.flatten
    val x_max = (vertices.map{ case vertex => vertex._1}).max
    val x_min = (vertices.map{ case vertex => vertex._1}).min
    val y_max = (vertices.map{ case vertex => vertex._2}).max
    val y_min = (vertices.map{ case vertex => vertex._2}).min
    val z_max = (vertices.map{ case vertex => vertex._3}).max
    val z_min = (vertices.map{ case vertex => vertex._3}).min
    val boundingBox = (x_min, y_min, z_min, x_max, y_max, z_max)

    // Calculate diagonal length
    val x_diff = boundingBox._4 - boundingBox._1
    val y_diff = boundingBox._5 - boundingBox._2
    val z_diff = boundingBox._6 - boundingBox._3
    val diagonalLength = math.sqrt(x_diff*x_diff + y_diff*y_diff + z_diff*z_diff)
    val normal = linalg.normalize(DenseVector[Double](x_diff, y_diff, z_diff))

    val joint = Joint((normal(0), normal(1), normal(2)), origin, center, phi = 0.0, cohesion = 0.0,
                      shape = Nil, artificialJoint = Some(true))
    println("Initial Volume: "+initialVolume.volume)
    println("Desired volume: "+desiredVolume)
    val blocks = initialVolume.cut(joint)
    val nonRedundantBlocks = blocks.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }
    val volumeBlocks = nonRedundantBlocks.sortWith(_.volume < _.volume)
    val tolerance = 0.05

    // If first block has satisfactory volume
    if ((volumeBlocks.head.volume < (1.0 + tolerance)*desiredVolume) && 
        (volumeBlocks.head.volume > (1.0 - tolerance)*desiredVolume) &&
        (volumeBlocks.length == 2)) {
      if (volumeBlocks.tail.head.volume <= (1.0 + tolerance)*desiredVolume) {
        // If both blocks have satifactory volumes, prepend joint to joints
        println("Adding joint and returning")
        joint +: joints
      } else {
        // If last block isn't small enough
        println("Adding joint and continuing")
        println(joint)
        val remainingBlock = volumeBlocks.tail.head
        val newCenter = (joint.centerX, joint.centerY, joint.centerZ)
        val centroid = remainingBlock.centroid
        val centroidBlock = Block(centroid, remainingBlock.updateFaces(centroid))
        findProcessorJoints(joint +: joints, origin, newCenter, centroidBlock,
                            desiredVolume)
      }
    // Block volumes not satisfactory, need to iterate to find processor joint
    } else {
      val centroidNS = initialVolume.centroid
      val centerVec0 = if ((center._1 < centroidNS._1) &&
                           (center._2 < centroidNS._2) &&
                           (center._3 < centroidNS._3)) {
          DenseVector[Double](center._1, center._2, center._3)
        } else {
          DenseVector[Double](boundingBox._1, boundingBox._2, boundingBox._3)
        }
      val centerVec1 = if ((center._1 < centroidNS._1) &&
                           (center._2 < centroidNS._2) &&
                           (center._3 < centroidNS._3)) {
          DenseVector[Double](boundingBox._4, boundingBox._5, boundingBox._6)
        } else {
          DenseVector[Double](center._1, center._2, center._3)
        }
      println("None satisfactory")
      // \nLower center: \n"+centerVec0+"\n"+"Upper center: "+"\n"+centerVec1)
      // First distance guess
      val x_diff0 =centerVec0(0) - boundingBox._1
      val y_diff0 =centerVec0(1) - boundingBox._2
      val z_diff0 =centerVec0(2) - boundingBox._3
      val dist0 = math.sqrt(x_diff0*x_diff0 + y_diff0*y_diff0 + z_diff0*z_diff0)
      // Second distance guess
      val dist1 = diagonalLength
      val newCenter = secantBisectionSolver(dist0, dist1, initialVolume, tolerance, 
                                            boundingBox, origin, desiredVolume, 0)

      findProcessorJoints(joints, origin, newCenter, initialVolume,
                          desiredVolume)
    }
  }

  @tailrec
  def secantBisectionSolver(initialDist0: Double, initialDist1: Double, block: Block, 
                                    tolerance: Double,
                                    boundingBox: (Double, Double, Double, Double, Double, Double),
                                    origin: (Double, Double, Double), desiredVolume: Double,
                                    iterations: Int):
                                    (Double, Double, Double) = {
    println("init0: "+initialDist0)
    println("init1: "+initialDist1)
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
    val volumeBlocks0 = blocks0.sortWith(_.volume < _.volume)
    val volumeBlocks1 = blocks1.sortWith(_.volume < _.volume)

    // Secant step
    val f_0 = volumeBlocks0.head.volume/desiredVolume - 1.0
    val f_1 = volumeBlocks1.head.volume/desiredVolume - 1.0
    val f_prime = (f_1 - f_0)/(initialDist1 - initialDist0)
    val secantDist = initialDist1 - f_1/f_prime
    // Bisection step
    val bisectionDist = (initialDist0 + initialDist1)/2.0
    // Select new step
    val b = Seq(initialDist0, initialDist1).max
    val a = Seq(initialDist0, initialDist1).min
    val newDist = if ((secantDist < a) || (secantDist > b) || (secantDist.isNaN)) bisectionDist 
                  else secantDist

    // Update based on new distance
    val newCenterVec = lowerLeft + newDist*normal
    val newCenter = (newCenterVec(0), newCenterVec(1), newCenterVec(2))
    val newJoint = Joint((normal(0), normal(1), normal(2)), origin, newCenter, phi = 0.0,
                         cohesion = 0.0, shape = Nil, artificialJoint = Some(true))
    val new_Blocks = block.cut(newJoint)
    val nonRedundantNew = new_Blocks.map { case block @ Block(center, _) =>
      Block(center, block.nonRedundantFaces)
    }
    val newBlocks = nonRedundantNew.sortWith(_.volume < _.volume)
    val f_p = newBlocks.head.volume/desiredVolume - 1.0
    val f_b = if (initialDist1 > initialDist0) {
      volumeBlocks1.head.volume/desiredVolume - 1.0
    } else {
      volumeBlocks0.head.volume/desiredVolume - 1.0
    }

    // Check if initial guesses are within block, otherwise update and retry
    if (volumeBlocks0.length != 2) {
      if ((center0Vec(0) < block.centerX) &&
          (center0Vec(1) < block.centerY) &&
          (center0Vec(2) < block.centerZ)) {
        println("LL - init0: "+center0Vec)
        // Outside lower left portion of block
        val incrementedCenter = center0Vec + 0.01*diagonalLength*normal
        val x_incr = incrementedCenter(0) - boundingBox._1
        val y_incr = incrementedCenter(1) - boundingBox._2
        val z_incr = incrementedCenter(2) - boundingBox._3
        val incrementedDist = 
          if ((x_incr < 0.0) &&
              (y_incr < 0.0) &&
              (z_incr < 0.0))
            -math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
          else 
            math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
        secantBisectionSolver(incrementedDist, initialDist1, block, tolerance,
                              boundingBox, origin, desiredVolume, 0)
      } else {
        // Outside upper right portion of bounding box
        println("UR - init0: "+center0Vec)
        val incrementedCenter = center0Vec - 0.01*diagonalLength*normal
        val x_incr = incrementedCenter(0) - boundingBox._1
        val y_incr = incrementedCenter(1) - boundingBox._2
        val z_incr = incrementedCenter(2) - boundingBox._3
        val incrementedDist = 
          if ((x_incr < 0.0) &&
              (y_incr < 0.0) &&
              (z_incr < 0.0))
            -math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
          else 
            math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
        secantBisectionSolver(incrementedDist, initialDist1, block, tolerance,
                              boundingBox, origin, desiredVolume, 0)
      }

    } else if (volumeBlocks1.length != 2) {
      if ((center1Vec(0) < block.centerX) &&
          (center1Vec(1) < block.centerY) &&
          (center1Vec(2) < block.centerZ)) {
        println("LL - init1: "+center1Vec)
        val incrementedCenter = center1Vec + 0.01*diagonalLength*normal
        val x_incr = incrementedCenter(0) - boundingBox._1
        val y_incr = incrementedCenter(1) - boundingBox._2
        val z_incr = incrementedCenter(2) - boundingBox._3
        val incrementedDist = 
          if ((x_incr < 0.0) &&
              (y_incr < 0.0) &&
              (z_incr < 0.0))
            -math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
          else 
            math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
        secantBisectionSolver(initialDist0, incrementedDist, block, tolerance,
                              boundingBox, origin, desiredVolume, 0)
      } else {
        println("UR - init1: "+center1Vec)
        val incrementedCenter = center1Vec - 0.01*diagonalLength*normal
        val x_incr = incrementedCenter(0) - boundingBox._1
        val y_incr = incrementedCenter(1) - boundingBox._2
        val z_incr = incrementedCenter(2) - boundingBox._3
        val incrementedDist = 
          if ((x_incr < 0.0) &&
              (y_incr < 0.0) &&
              (z_incr < 0.0))
            -math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
          else 
            math.sqrt(x_incr*x_incr + y_incr*y_incr + z_incr*z_incr)
        secantBisectionSolver(initialDist0, incrementedDist, block, tolerance,
                              boundingBox, origin, desiredVolume, 0)
      }
    } else if ((math.abs(newBlocks.head.volume/desiredVolume - 1.0) < tolerance) ||
               (iterations > iterationLimit)) {
      // Either gives appropriate volume of iteration limit has been reached
      if (iterations > iterationLimit) println("Unable to converge within iteration limit, "+
                                               "returning result of last iteration: \n"+
                                                (newJoint.centerX, newJoint.centerY, newJoint.centerZ))
      println("End blocks volumes: \n"+newBlocks.head.volume+"\n"+newBlocks.tail.head.volume)
      println("Vol - end iteration: "+newBlocks.head.volume)
      (newJoint.centerX, newJoint.centerY, newJoint.centerZ)
    } else if (f_p * f_b < 0.0) {
      println("Int blocks volumes: \n"+newBlocks.head.volume+"\n"+newBlocks.tail.head.volume)
      // Do another iteration with lower bound of interval updated
      secantBisectionSolver(newDist, b, block, tolerance, boundingBox, origin,
                            desiredVolume, iterations + 1)
    } else {
      // Do another iteration with upper bound of interval updated
      println("Int blocks volumes: \n"+newBlocks.head.volume+"\n"+newBlocks.tail.head.volume)
      secantBisectionSolver(a, newDist, block, tolerance, boundingBox, origin,
                            desiredVolume, iterations + 1)
    }
  }
}


