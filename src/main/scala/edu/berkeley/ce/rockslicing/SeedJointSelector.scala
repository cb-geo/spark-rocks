package edu.berkeley.ce.rockslicing

import scala.annotation.tailrec

/**
  * Selects joints from the input joint set that will be used to seed the RDD in order to maintain
  * approximately balanced loads among processors.
  */
object SeedJointSelector {
  val THRESHOLD = 0.50
  /**
    * Searches though sequence of joint sets to find best seed joints
    *
    * @param jointSets Seq of Seq's of Joints representing input joint sets
    * @param inputVolume Block representing input rock volume
    * @param numProcessors Number of processors to be used in the analysis
    * @return Returns a Seq containing the seed joints that best divide the input volume
    */
  @tailrec
  def searchJointSets(jointSets: Seq[Seq[Joint]], inputVolume: Block,
                      numProcessors: Int): Seq[Joint] = {
    if (jointSets.isEmpty) {
      throw new IllegalStateException("ERROR: Unable to find satisfactory seed joints")
    }
    if (jointSets.head(0).shape.isEmpty) {
      // Check that joint set is persistent
      val seedJointOption = findSeedJoints(jointSets.head, inputVolume, numProcessors, inputVolume.volume)

      // Check whether joint set yields satisfactory seed joints. If it does not, check next joint set.
      if (seedJointOption.isEmpty) {
          searchJointSets(jointSets.tail, inputVolume, numProcessors)
      } else {
        seedJointOption.get
      }
    } else {
      searchJointSets(jointSets.tail, inputVolume, numProcessors)
    }
  }

  /**
    * Cycles through persistent joint set to find joints that approximately balances load across processes
    *
    * @param jointSet Seq of Joints representing a persistent joint set
    * @param rockVolume Block representing input rock volume that should be divided into approximately equal pieces
    *                   for each process to operate on
    * @param numProcessors Number of processes that will be used in analysis
    * @param tolerance Tolerance in deviation from ideal volume per processor. Default initial value is 1%, though it
    *                  will be updated as necessary in recursive calls.
    * @param stepSize Step size that tolerance will be incremented by if unable to find enough seed joints. Default
    *                 initial value is 1%, though it will be updated as necessary in recursive calls.
    * @return If able to find the required number of seed joints, this function will return a Seq of Joints. If unable
    *         to find sufficient seed joints, it will return None.
    */
  @tailrec
  private def findSeedJoints(jointSet: Seq[Joint], rockVolume: Block, numProcessors: Integer, tolerance: Double = 0.01,
                             stepSize: Double = 0.01): Option[Seq[Joint]] = {
    if (jointSet.length < numProcessors - 1) {
      throw new IllegalArgumentException(s"Error: Not enough joints to generate required number of seed joints. ${jointSet.length} joints but"+
        s" $numProcessors processors. Try using fewer processors.")
    }

    val totalVolume = rockVolume.volume
    val volumePerProc = rockVolume.volume / numProcessors
    val seedJoints = selectSeedJoints(jointSet, rockVolume, volumePerProc, totalVolume,
      Seq.empty[Joint], tolerance, numProcessors)

    if (stepSize < 0.01) {
      // Unable to find satisfactory seed joints using current joint set
      None
    } else if (seedJoints.length != numProcessors - 1) {
      val newStepSize = if (tolerance >= 1.0) { stepSize / 2.0 } else { stepSize }
      val newTolerance = if (tolerance >= 1.0) { newStepSize } else { tolerance + stepSize }
      findSeedJoints(jointSet, rockVolume, numProcessors, newTolerance, newStepSize)
    } else {
      val initialBlock = Seq(rockVolume)
      val blocks = seedJoints.foldLeft(initialBlock) { (currentBlocks, joint) =>
        currentBlocks.flatMap(_.cut(joint))
      }
      val nonRedundantBlocks = blocks map { case block @ Block(blockCenter, _, _) =>
          Block(blockCenter, block.nonRedundantFaces)
      }
      val volumes = nonRedundantBlocks.map(_.volume)
      val avgVolume = volumes.sum / volumes.length
      println("\nLOAD BALANCE INFORMATION:")
      println(s"Average Volume: %.2f".format(avgVolume))
      println(s"Max Volume: %.2f".format(volumes.max))
      println(s"Min Volume: %.2f".format(volumes.min))
      Some(seedJoints)
    }
  }

  /**
    * Iterates through input joint set and selects joints that divide the initial block into approximately
    * equal volumes
    *
    * @param jointSet Seq of Joints representing a persistent joint set
    * @param initialBlock Block that should be subdivided by seed joints into approximately equal volumes
    * @param volumePerProcessor Ideal volume per processor for load balance
    * @param totalVolume Total volume of rock volume in analysis
    * @param selectedJoints Seq of Joints that have been identified as seed joints
    * @param tolerance Acceptable deviation from ideal volume per processor
    * @param numProcessors Number of processors that will be used in the analysis
    * @return Seq of joints that divide initial block into approximately equal volumes
    */
  @tailrec
  private def selectSeedJoints(jointSet: Seq[Joint], initialBlock: Block, volumePerProcessor: Double,
                            totalVolume: Double, selectedJoints: Seq[Joint], tolerance: Double,
                            numProcessors: Int): Seq[Joint] = {
    if (selectedJoints.length == numProcessors - 1) {
      // Enough seed joints have been found
      selectedJoints
    } else if (jointSet.length > 1) {
      // More than 1 joint in input joint set
      val jointOption = testVolumes(jointSet.head, jointSet.tail.head, initialBlock,
        volumePerProcessor, totalVolume, tolerance)

      if (jointOption.isEmpty) {
        // Joint did not meet criteria
        selectSeedJoints(jointSet.tail, initialBlock, volumePerProcessor, totalVolume, selectedJoints,
          tolerance, numProcessors)
      } else {
        // Joint meets criteria
        val (seedJoint, remainingVolume) = jointOption.get
        if (remainingVolume.volume <= volumePerProcessor) {
          // Remaining volume small enough, return
          seedJoint +: selectedJoints
        } else {
          // Keep cycling through joints
          selectSeedJoints(jointSet.tail, remainingVolume, volumePerProcessor, totalVolume,
            seedJoint +: selectedJoints, tolerance, numProcessors)
        }
      }
    } else {
      // Only one joint in input joint set
      val lastBlocks = initialBlock cut jointSet.head
      val nonRedundantLastBlocks = lastBlocks map { case block @ Block(blockCenter, _, _) =>
          Block(blockCenter, block.nonRedundantFaces)
      }
      val sortedLastBlocks = nonRedundantLastBlocks.sortWith(_.volume < _.volume)
      if (sortedLastBlocks.length == 1) {
        // Joint does not intersect volume
        selectedJoints
      } else {
        // Joint intersect volume
        jointSet.head +: selectedJoints
      }
    }
  }

  /**
    * Test which, if any, of two input joints divides the input block into blocks of approximately the
    * desired volume. Since joints most likely will not divide input block into exactly the desired volume,
    * consider two joints simultaneously and compare how they divide the input block. When one joint yields a block
    * smaller than the desired volume and the other yields a block larger than the desired volume, select joint that
    * yields block closest to the desired volume.
    * When less than THRESHOLD percent of the total volume remains, the tolerance on the joint selection is loosened
    * to ensure that an adequate number of seed joints can be found.
    *
    * @param joint1 First input joint
    * @param joint2 Second input joint
    * @param inputBlock Block that is to be subdivided
    * @param desiredVolume Desired volume of subdivision(s)
    * @param totalVolume Total volume of rock volume in analysis
    * @param tolerance Acceptable deviation from ideal volume per processor
    * @return Returns tuple containing joint that provides best division of volume and remaining volume that is to be
    *         subdivided further. If neither joint yields acceptable subdivision, function returns None.
    */
  private def testVolumes(joint1: Joint, joint2: Joint, inputBlock: Block,
                          desiredVolume: Double, totalVolume: Double,
                          tolerance: Double): Option[(Joint, Block)] = {
    val blockSet1 = inputBlock cut joint1
    val blockSet2 = inputBlock cut joint2

    val nonRedundantBlocks1 = blockSet1 map { case block @ Block(blockCenter, _, _) =>
        Block(blockCenter, block.nonRedundantFaces)
    }
    val nonRedundantBlocks2 = blockSet2 map { case block @ Block(blockCenter, _, _) =>
      Block(blockCenter, block.nonRedundantFaces)
    }

    // Sort generated blocks based on volume
    val sortedBlocks1 = nonRedundantBlocks1.sortWith(_.volume < _.volume)
    val sortedBlocks2 = nonRedundantBlocks2.sortWith(_.volume < _.volume)

    if (sortedBlocks1.length == 1) {
      // joint1 does not intersect the block
      if (sortedBlocks2.length == 1) {
        // Neither joint intersects the block
        None
      } else if (sortedBlocks2(0).volume >= desiredVolume) {
        // joint2 sufficiently divides block, remaining volume should still be further subdivided
        Some(joint2, sortedBlocks2(1))
      } else if (sortedBlocks2(1).volume <= desiredVolume) {
        // joint2 sufficiently divides block, remaining volume requires no further subdivision
        Some(joint2, sortedBlocks2(1))
      } else {
        // joint 2 does not divide block adequately
        None
      }
    } else if (sortedBlocks2.length == 1) {
      // joint 2 does not intersect the block
      if (sortedBlocks1(0).volume >= desiredVolume) {
        // joint1 sufficiently divides block, remaining volume should still be further subdivided
        Some(joint1, sortedBlocks1(1))
      } else if (sortedBlocks1(1).volume <= desiredVolume) {
        // joint1 sufficiently divides block, remaining volume requires no further subdivision
        Some(joint1, sortedBlocks1(1))
      } else {
        // joint 1 does not divide the block adequately
        None
      }
    } else {
      // BOTH joints intersect the block
      // Compare volumes to desired volumes
      val volumeDiff1 = (sortedBlocks1(0).volume - desiredVolume) / desiredVolume
      val volumeDiff2 = (sortedBlocks2(0).volume - desiredVolume) / desiredVolume

      // Return joint that gives volume closest to desired volume.
      // When less than 50% of the total volume remains, begin loosening constraints on joint selected until
      // a sufficient number of joints have been found. When more than TOLERANCE of the total volume still remains,
      // seed joints are chosen when one of the input joints yields a block smaller than the desired volume and
      // the other yields a block larger then the desired volume. The joint yielding a block with volume closest
      // to the desired volume is selected.
      if ((math.abs(volumeDiff1) <= tolerance || math.abs(volumeDiff2) <= tolerance) &&
        (sortedBlocks1(1).volume <= THRESHOLD * totalVolume || sortedBlocks2(1).volume <= THRESHOLD * totalVolume)) {
        if (math.abs(volumeDiff1) >= math.abs(volumeDiff2)) {
          Some(joint2, sortedBlocks2(1))
        } else {
          Some(joint1, sortedBlocks1(1))
        }
      } else if (volumeDiff1 <= 0.0 && volumeDiff2 >= 0.0) {
        if (math.abs(volumeDiff1) >= math.abs(volumeDiff2)) {
          Some(joint2, sortedBlocks2(1))
        } else {
          Some(joint1, sortedBlocks1(1))
        }
      } else {
        None
      }
    }
  }
}