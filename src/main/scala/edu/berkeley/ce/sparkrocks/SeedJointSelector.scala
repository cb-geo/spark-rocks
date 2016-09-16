package edu.berkeley.ce.sparkrocks

import scala.annotation.tailrec

/**
  * Selects joints from the input joint set that will be used to seed the RDD in order to
  * maintain approximately balanced loads among processors.
  */
object SeedJointSelector {

  @tailrec
  def generateSeedBlocks(jointSets: Seq[Seq[Joint]], inputVolume: Block, numPartitions: Int,
                         jointSetSpan: Int = 1, seedBlocks: Seq[Block] = Seq.empty[Block],
                         remainingJoints: Seq[Joint] = Seq.empty[Joint]): (Seq[Block], Seq[Joint]) = {
    if (jointSetSpan > jointSets.length) {
      if (seedBlocks.isEmpty) {
        throw new IllegalStateException("ERROR: Unable to find satisfactory seed joints")
      } else {
        println(
          s"""WARNING: Unable to find at least desired number of partitions. Continuing run with
        ${seedBlocks.length} partitions.""")
        (seedBlocks, remainingJoints)
      }
    } else {
      // Find joints that cut input volume into approximately equal pieces
      val (partitionBlocks, leftOverJoints) = findSeedBlocks(jointSets, inputVolume, numPartitions, jointSetSpan)
      // Sufficient partitions found
      if (partitionBlocks.length >= numPartitions) {
        (partitionBlocks, leftOverJoints)
        // Unable to find sufficient partitions, run with greater span
      } else {
        generateSeedBlocks(jointSets, inputVolume, numPartitions, jointSetSpan + 1, partitionBlocks, leftOverJoints)
      }
    }
  }

  @tailrec
  def findSeedBlocks(jointSets: Seq[Seq[Joint]], inputVolume: Block, numPartitions: Int, jointSetSpan: Int,
                     seedBlocks: Seq[Block] = Seq.empty[Block], remainingJoints: Seq[Joint] = Seq.empty[Joint]):
  (Seq[Block], Seq[Joint]) = {
    // All joint sets checked or span too large
    if (jointSets.isEmpty || jointSetSpan > jointSets.length) {
      (seedBlocks, remainingJoints)
    } else if (jointSets.head(0).shape.isEmpty) {
      val (seedJoints, leftOverJoints) = searchJointSets(jointSets, inputVolume, numPartitions, jointSetSpan)
      val partitioning = seedJoints.foldLeft(Seq(inputVolume)) { (currentBlocks, joint) =>
        currentBlocks.flatMap(_.cut(joint))
      }
      // Insufficient partitions found
      if (partitioning.length < numPartitions) {
        findSeedBlocks(jointSets.tail, inputVolume, numPartitions, jointSetSpan, partitioning,
          remainingJoints ++ jointSets.head)
        // Sufficient partitions, return
      } else {
        (partitioning, remainingJoints ++ leftOverJoints)
      }
      // Joint set non-persistent, check next one
    } else {
      findSeedBlocks(jointSets.tail, inputVolume, numPartitions, jointSetSpan, seedBlocks, remainingJoints)
    }
  }

  @tailrec
  def searchJointSets(jointSets: Seq[Seq[Joint]], inputVolume: Block, numPartitions: Int, jointSetSpan: Int,
                      spanCount: Int = 1, seedJoints: Seq[Joint] = Seq.empty[Joint],
                      remainingJoints: Seq[Joint] = Seq.empty[Joint]): (Seq[Joint], Seq[Joint]) = {
    // Checked specified span of joint sets, return
    if (spanCount > jointSetSpan) {
      (seedJoints, remainingJoints)
    } else {
      val totalVolume = inputVolume.volume
      // Determine remaining partitions that need to be found. Though unlikely, ensure that division by
      // zero does not occur
      val remainingPartitions = if (numPartitions != seedJoints.length) {
        math.abs(numPartitions - seedJoints.length)
      } else {
        1
      }
      val volumePerPiece = totalVolume / remainingPartitions
      val newSeedJoints = findSeedJoints(jointSets.head, inputVolume, totalVolume, volumePerPiece)
      val remainingFromJointSet = jointSets.head.diff(newSeedJoints)
      searchJointSets(jointSets.tail, inputVolume, numPartitions, jointSetSpan, spanCount + 1,
        seedJoints ++ newSeedJoints, remainingJoints ++ remainingFromJointSet)
    }
  }

  @tailrec
  def findSeedJoints(jointSet: Seq[Joint], initialVolume: Block, totalVolume: Double, volumePerPiece: Double,
                     selectedJoints: Seq[Joint] = Seq.empty[Joint]): Seq[Joint] = {
    // More than one joint in input joint set
    if (jointSet.length > 1) {
      val jointOption = testVolumes(jointSet(0), jointSet(1), initialVolume, volumePerPiece)
      // Joint did not meet criteria
      if (jointOption.isEmpty) {
        println("JOINT DID NOT MEET CRITERIA")
        findSeedJoints(jointSet.tail, initialVolume, totalVolume, volumePerPiece, selectedJoints)
      } else {
        println("JOINT MEETS CRITERIA!!!!")
        // Joint meets criteria
        val (seedJoint, remainingVolume) = jointOption.get
        // Remaining volume small enough, return
        if (remainingVolume.volume <= volumePerPiece) {
          selectedJoints :+ seedJoint
          // Large volume still remains, keep cycling
        } else {
          findSeedJoints(jointSet.tail, remainingVolume, totalVolume, volumePerPiece, selectedJoints :+ seedJoint)
        }
      }
      // Only one joint in input set
    } else {
      val lastBlocks = initialVolume cut jointSet.head
      // Joint does not intersect volume
      if (lastBlocks.length == 1) {
        println("DOESN'T INTERSECT VOLUME!")
        selectedJoints
        // Joint intersects volume
      } else {
        println("LAST JOINT INTERSECTS VOLUME!")
        selectedJoints :+ jointSet.head
      }
    }
  }

  def testVolumes(joint1: Joint, joint2: Joint, initialVolume: Block, desiredVolume: Double):
  Option[(Joint, Block)] = {
    val blockSet1 = initialVolume cut joint1
    val blockSet2 = initialVolume cut joint2

    val nonRedundantBlocks1 = blockSet1 map { case block@Block(blockCenter, _, _) =>
      Block(blockCenter, block.nonRedundantFaces)
    }
    val nonRedundantBlocks2 = blockSet2 map { case block@Block(blockCenter, _, _) =>
      Block(blockCenter, block.nonRedundantFaces)
    }

    // Sort generated blocks based on volume
    val sortedBlocks1 = nonRedundantBlocks1.sortWith(_.volume < _.volume)
    val sortedBlocks2 = nonRedundantBlocks2.sortWith(_.volume < _.volume)

    // joint1 does not intersect the block
    if (sortedBlocks1.length == 1) {
      // Neither joint intersects the block
      if (sortedBlocks2.length == 1) {
        None
        // joint2 sufficiently divides block, remaining volume should still be further subdivided
      } else if (sortedBlocks2(0).volume >= desiredVolume) {
        Some(joint2, sortedBlocks2(1))
        // joint2 sufficiently divides block, remaining volume requires no further subdivision
      } else if (sortedBlocks2(1).volume <= desiredVolume) {
        Some(joint2, sortedBlocks2(1))
        // joint 2 does not divide block adequately
      } else {
        None
      }
      // joint 2 does not intersect the block
    } else if (sortedBlocks2.length == 1) {
      // joint1 sufficiently divides block, remaining volume should still be further subdivided
      if (sortedBlocks1(0).volume >= desiredVolume) {
        Some(joint1, sortedBlocks1(1))
        // joint1 sufficiently divides block, remaining volume requires no further subdivision
      } else if (sortedBlocks1(1).volume <= desiredVolume) {
        Some(joint1, sortedBlocks1(1))
        // joint 1 does not divide the block adequately
      } else {
        None
      }
    // BOTH joints intersect the block
    // Remaining volume very small
    } else if (sortedBlocks1(0).volume <= desiredVolume && sortedBlocks1(1).volume <= desiredVolume) {
      Some(joint1, sortedBlocks1(1))
    } else if (sortedBlocks2(0).volume <= desiredVolume && sortedBlocks2(1).volume <= desiredVolume) {
      Some(joint2, sortedBlocks2(1))
    // Remaining volume not very small
    } else {
      // Compare volumes to desired volumes
      val volumeDiff1 = sortedBlocks1(0).volume - desiredVolume
      val volumeDiff2 = sortedBlocks2(0).volume - desiredVolume

      // Return joint that gives volume closest to desired volume.
      // When less than 50% of the total volume remains, begin loosening constraints on joint selected until
      // a sufficient number of joints have been found. When more than THRESHOLD of the total volume still remains,
      // seed joints are chosen when one of the input joints yields a block smaller than the desired volume and
      // the other yields a block larger then the desired volume. The joint yielding a block with volume closest
      // to the desired volume is selected.
      if (volumeDiff1 <= 0.0 && volumeDiff2 >= 0.0) {
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