package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.DenseVector

import scala.annotation.tailrec
import scala.collection.mutable

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
    *
    * @param rockVolume Initial block defining the entire rock volume of interest
    * @param numProcessors Number of processors used in analysis.
    * @return Set of processor joints that will be used to seed the initial RDD
    */
  def generateProcessorJoints(rockVolume: Block, numProcessors: Integer): Seq[Joint] = {
    // Calculate bounding box
    val vertices = rockVolume.findVertices.values.flatten
    val xMin = vertices.map(_(0)).min
    val yMin = vertices.map(_(1)).min
    val zMin = vertices.map(_(2)).min

    // Diagonal vector from lower left to upper right corner of bounding box
    val volumePerPart = rockVolume.volume/numProcessors
    val centroid = rockVolume.centroid
    val centroidVolume = Block(centroid, rockVolume.updateFaces(centroid))
    val processorJoints =
      findProcessorJoints(Seq.empty[Joint], Array(centroidVolume.centerX, centroidVolume.centerY, centroidVolume.centerZ),
                          Array(xMin, yMin, zMin),
                          centroidVolume, volumePerPart)

    assert(processorJoints.length + 1 == numProcessors)
    processorJoints
  }

  /**
    * Finds the best set of processor joints to divide initial volume into approximately equal volumes
    *
    * @param joints List to prepend processor joints to during each recursion
    * @param origin Origin which the joint is in reference to
    * @param center Center of the joint plane - start with lower left corner of bounding box
    * @param initialVolume Intial volume that needs to be sub-divided
    * @param desiredVolume Desired volume for each piece cut by processor joints
    * @return List of processor joints that will be used to seed initial RDD
    */
  @tailrec
  private def findProcessorJoints(joints: Seq[Joint], origin: Array[Double], center: Array[Double],
                                  initialVolume: Block, desiredVolume: Double): Seq[Joint] = {
    // Calculate bounding box
    val vertices = initialVolume.findVertices.values.flatten
    val xMax = vertices.map(_(0)).max
    val xMin = vertices.map(_(0)).min
    val yMax = vertices.map(_(1)).max
    val yMin = vertices.map(_(1)).min
    val zMax = vertices.map(_(2)).max
    val zMin = vertices.map(_(2)).min
    val boundingBox = (xMin, yMin, zMin, xMax, yMax, zMax)

    // Calculate diagonal length
    val xDiff = boundingBox._4 - boundingBox._1
    val yDiff = boundingBox._5 - boundingBox._2
    val zDiff = boundingBox._6 - boundingBox._3
    val diagonalLength = math.sqrt(xDiff*xDiff + yDiff*yDiff + zDiff*zDiff)
    val normal = linalg.normalize(DenseVector[Double](xDiff, yDiff, zDiff))

    val joint = Joint(Array(normal(0), normal(1), normal(2)), origin, center, phi = 0.0, cohesion = 0.0,
                      shape = Vector.empty, processorJoint = true)
    val blocks = initialVolume.cut(joint)
    val nonRedundantBlocks = blocks.map { case block @ Block(blockCenter, _, _) =>
      Block(blockCenter, block.nonRedundantFaces)
    }
    val sortedBlocks = nonRedundantBlocks.sortWith{ (left, right) =>
      centroidCompare(left.centroid, right.centroid) }

    // If first block has satisfactory volume
    if ((sortedBlocks.head.volume < (1.0 + VOLUME_TOLERANCE)*desiredVolume) && 
        (sortedBlocks.head.volume > (1.0 - VOLUME_TOLERANCE)*desiredVolume) &&
        (sortedBlocks.length == 2)) {
      if (sortedBlocks.tail.head.volume <= (1.0 + VOLUME_TOLERANCE)*desiredVolume) {
        // If both blocks have satisfactory volumes, prepend joint to joints
        joint +: joints
      } else {
        // If last block isn't small enough
        val remainingBlock = sortedBlocks.tail.head
        val newCenter = Array(joint.centerX, joint.centerY, joint.centerZ)
        val centroid = remainingBlock.centroid
        val centroidBlock = Block(centroid, remainingBlock.updateFaces(centroid))
        findProcessorJoints(joint +: joints, origin, newCenter, centroidBlock,
                            desiredVolume)
      }
    // Block volumes not satisfactory, need to iterate to find processor joint
    } else {
      val centerVec0 = DenseVector[Double](center)
      // First distance guess
      val xDiff0 = centerVec0(0) - xMin
      val yDiff0 = centerVec0(1) - yMin
      val zDiff0 = centerVec0(2) - zMin
      val dist0 = math.sqrt(xDiff0*xDiff0 + yDiff0*yDiff0 + zDiff0*zDiff0)
      // Second distance guess
      val dist1 = diagonalLength
      val boundingBox = Array(xMin, yMin, zMin, xMax, yMax, zMax)
      val nextCenter = secantBisectionSolver(dist0, dist1, initialVolume, VOLUME_TOLERANCE, boundingBox, origin,
                                             desiredVolume, 0)
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
    *
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
  private def secantBisectionSolver(initialDist0: Double, initialDist1: Double, block: Block, tolerance: Double,
                                    boundingBox: Array[Double], origin: Array[Double], desiredVolume: Double,
                                    iterations: Int): Array[Double] = {
    val iterationLimit = 100
    // Calculate diagonal length
    val xDiff = boundingBox(3) - boundingBox(0)
    val yDiff = boundingBox(4) - boundingBox(1)
    val zDiff = boundingBox(5) - boundingBox(2)
    val diagonalLength = math.sqrt(xDiff*xDiff + yDiff*yDiff + zDiff*zDiff)

    // Normal vector for processor joints
    val normal = breeze.linalg.normalize(DenseVector[Double](xDiff, yDiff, zDiff))
    val lowerLeft = DenseVector[Double](boundingBox(0), boundingBox(1), boundingBox(2))
    // Centers for initial joint guesses
    val center0Vec = lowerLeft + initialDist0*normal
    val center0 = Array(center0Vec(0), center0Vec(1), center0Vec(2))
    val center1Vec = lowerLeft + initialDist1*normal
    val center1 = Array(center1Vec(0), center1Vec(1), center1Vec(2))

    // Joint guesses and corresponding blocks
    val joint0 = Joint(Array(normal(0), normal(1), normal(2)), origin, center0, phi = 0.0, cohesion = 0.0,
                       shape = Vector.empty, processorJoint = true)
    val joint1 = Joint(Array(normal(0), normal(1), normal(2)), origin, center1, phi = 0.0, cohesion = 0.0,
                       shape = Vector.empty, processorJoint = true)
    val blocks_0 = block.cut(joint0)
    val blocks_1 = block.cut(joint1)
    val blocks0 = blocks_0.map { case block @ Block(center, _, _) =>
      Block(center, block.nonRedundantFaces)
    }
    val blocks1 = blocks_1.map { case block @ Block(center, _, _) =>
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
    val newDist = if ((secantDist < a) || (secantDist > b) || secantDist.isNaN) {
      bisectionDist
    } else {
      secantDist
    }

    // Update based on new distance
    val newCenterVec = lowerLeft + newDist*normal
    val newCenter = Array(newCenterVec(0), newCenterVec(1), newCenterVec(2))
    val newJoint = Joint(Array(normal(0), normal(1), normal(2)), origin, newCenter, phi = 0.0,
                         cohesion = 0.0, shape = Vector.empty, processorJoint = true)
    val new_Blocks = block.cut(newJoint)
    val nonRedundantNew = new_Blocks.map { case block@Block(center, _, _) =>
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
        val x_incr = incrementedCenter(0) - boundingBox(0)
        val y_incr = incrementedCenter(1) - boundingBox(1)
        val z_incr = incrementedCenter(2) - boundingBox(2)
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
        val x_incr = incrementedCenter(0) - boundingBox(0)
        val y_incr = incrementedCenter(1) - boundingBox(1)
        val z_incr = incrementedCenter(2) - boundingBox(2)
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
        val x_incr = incrementedCenter(0) - boundingBox(0)
        val y_incr = incrementedCenter(1) - boundingBox(1)
        val z_incr = incrementedCenter(2) - boundingBox(2)
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
        val x_incr = incrementedCenter(0) - boundingBox(0)
        val y_incr = incrementedCenter(1) - boundingBox(1)
        val z_incr = incrementedCenter(2) - boundingBox(2)
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
      if (iterations > iterationLimit) println("Unable to converge within iteration limit, " +
                                               "returning result of last iteration")
      Array(newJoint.centerX, newJoint.centerY, newJoint.centerZ)
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
    *
    * @param centroid1 First centroid, input as a tuple
    * @param centroid2 Second centroid, input as a tuple
    * @return True if centroid1 is less positive than centroid2, false otherwise
    */
  private def centroidCompare(centroid1: Array[Double], centroid2:Array[Double]): Boolean = {
    assert(centroid1.length == 3 && centroid2.length == 3)
    (centroid1(0) < centroid2(0)) && (centroid1(1) < centroid2(1)) && (centroid1(2) < centroid2(2))
  }

  /**
    * Merge all blocks that are adjacent and separated only by a processor face.
    * All of the blocks should share a common processor face, i.e. this should
    * be called after the blocks have been grouped by their processor faces.
    *
    * @param blocks The blocks to merge.
    * @return A pair of block Seqs. The first item in the pair is the merged
    *         blocks, and the second item in the pair is any blocks that were
    *         left unmatched at the end of the merging process.
    */
  def removeCommonProcessorJoint(commonJoint: ((Double, Double, Double), Double), blocks: Seq[Block]):
      (Seq[Block], Seq[Block]) = {

    val ((commonA, commonB, commonC), commonD) = commonJoint

    val (leftBlocks, rightBlocks) = blocks.partition { block =>
      val processorFace = block.faces.find(_.isProcessorFace).get
      if (math.abs(processorFace.a) > NumericUtils.EPSILON) {
        processorFace.a >= NumericUtils.EPSILON
      } else if (math.abs(processorFace.b) > NumericUtils.EPSILON) {
        processorFace.b >= NumericUtils.EPSILON
      } else {
        processorFace.c >= NumericUtils.EPSILON
      }
    }

    // Keep track of unmatched blocks, not easy to do in functional style
    val remainingRightBlocks = rightBlocks.to[mutable.Set]
    val mergedBlocks = mutable.Set.empty[Block]
    val orphanBlocks = mutable.Set.empty[Block]

    for (leftBlock <- leftBlocks) {
      // If bounding spheres don't intersect, blocks can't be mates
      val mateCandidates = remainingRightBlocks filter { rightBlock =>
        val sphereDistance = math.sqrt(math.pow(leftBlock.sphereCenterX - rightBlock.sphereCenterX, 2) +
                                       math.pow(leftBlock.sphereCenterY - rightBlock.sphereCenterY, 2) +
                                       math.pow(leftBlock.sphereCenterZ - rightBlock.sphereCenterZ, 2))
        sphereDistance < (leftBlock.sphereRadius + rightBlock.sphereRadius)
      }

      // Use a 'view' to avoid unnecessary computation
      val candidateBlocks = mateCandidates.view.map { rightBlock =>
        val newFaces = (leftBlock.faces ++ rightBlock.faces).filter { face =>
          !face.isProcessorFace ||
          NumericUtils.roundToTolerance(math.abs(face.a)) != commonA ||
          NumericUtils.roundToTolerance(math.abs(face.b)) != commonB ||
          NumericUtils.roundToTolerance(math.abs(face.c)) != commonC ||
          NumericUtils.roundToTolerance(math.abs(face.d)) != commonD
        }
        val globalOrigin = Array(0.0, 0.0, 0.0)
        // No need to update newFaces since the inputs are already referenced to global origin
        val newBlock = Block(globalOrigin, newFaces)
        (rightBlock, Block(globalOrigin, newBlock.nonRedundantFaces))
      }

      val matchOpt = candidateBlocks.find(_._2.faces.nonEmpty)
      if (matchOpt.isDefined) {
        val (rightMate, mergedBlock) = matchOpt.get
        remainingRightBlocks.remove(rightMate)
        if (mergedBlock.faces.exists(_.isProcessorFace)) {
          orphanBlocks.add(mergedBlock)
        } else {
          mergedBlocks.add(mergedBlock)
        }
      } else {
        orphanBlocks.add(leftBlock)
      }
    }

    orphanBlocks ++= remainingRightBlocks
    (mergedBlocks.toSeq, orphanBlocks.toSeq)
  }

  /**
    * Given a collection of processor blocks (blocks containing at least one
    * processor face), merge any blocks that share a common processor face.
    *
    * @param processorBlocks A Seq of blocks, each of which must contain at
    *                        least one processor face.
    * @return A Seq of merged blocks. None of them should contain a processor
    *         face after the merging process.
    */
  def mergeProcessorBlocks(processorBlocks: Seq[Block]): Seq[Block] = {
    val globalOrigin = Array(0.0, 0.0, 0.0)
    var orphanBlocks = processorBlocks.map { block => Block(globalOrigin, block.updateFaces(globalOrigin)) }
    var mergedBlocks = Seq.empty[Block]

    // We only iterate more than once if a block contains multiple processor faces
    while (orphanBlocks.nonEmpty) {
      val normVecBlocks = orphanBlocks.groupBy { block =>
        val processorFace = block.faces.find(_.isProcessorFace).get
        ((NumericUtils.roundToTolerance(math.abs(processorFace.a)),
          NumericUtils.roundToTolerance(math.abs(processorFace.b)),
          NumericUtils.roundToTolerance(math.abs(processorFace.c))),
          NumericUtils.roundToTolerance(math.abs(processorFace.d)))
      }

      val mergeResults = normVecBlocks.toSeq.map { case (commonJoint, blocks) =>
        removeCommonProcessorJoint(commonJoint, blocks)
      }
      mergedBlocks = mergedBlocks ++ mergeResults.flatMap(_._1)
      orphanBlocks = mergeResults.flatMap(_._2)
    }

    mergedBlocks
  }

  def printBlocks(blocks: Seq[Block]): Unit = {
    blocks foreach { block =>
      println(s"Center: ${block.center.mkString(",")}")
      println("Faces:")
      block.faces.foreach{ face =>
        println(s"a: ${face.a}, b: ${face.b}, c: ${face.c}, d: ${face.d}, procFace: ${face.isProcessorFace}")
      }
    }
  }
}
