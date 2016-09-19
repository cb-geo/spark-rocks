package edu.berkeley.ce.sparkrocks

import scala.annotation.tailrec

/**
  * Selects joints from the input joint set that will be used to seed the RDD in order to
  * maintain approximately balanced loads among processors.
  */
object SeedJointSelector {
  // Threshold for joint acceptance
  val THRESHOLD = 0.1

  /**
    * Generate specified number of partitions using input joint sets
    *
    * @param jointSets Seq of Seq's of Joints representing input joint sets
    * @param inputVolume Block representing input rock volume
    * @param numPartitions Number of partitions desired
    * @param jointSetSpan Number of joint sets to search at a time for partition generation
    * @param seedBlocks Blocks that will serve as partitions
    * @param remainingJoints Seq of joints that were not used in seed block generation
    * @return Returns a tuple containing a Seq of blocks and a Seq of joints. The Seq of blocks
    *         are the the desired partitions. The Seq of joints are the remaining joints that were
    *         not used to generate the partitions.
    */
  @tailrec
  def generateSeedBlocks(jointSets: Seq[Seq[Joint]], inputVolume: Block, numPartitions: Int,
                         jointSetSpan: Int = 1, seedBlocks: Seq[Block] = Seq.empty[Block],
                         remainingJoints: Seq[Joint] = Seq.empty[Joint]): (Seq[Block], Seq[Joint]) = {
    if (jointSetSpan > jointSets.length) {
      if (seedBlocks.isEmpty) {
        throw new IllegalStateException("ERROR: Unable to find satisfactory seed joints")
      } else {
        val volumes = seedBlocks.map(_.volume)
        val avgVolume = volumes.foldLeft(0.0)(_ + _) / volumes.length
        println("\nLoad Balance Info:")
        println(s"Number of partitions: ${volumes.length}")
        println(s"Average Volume: $avgVolume")
        println(s"Max volume: ${volumes.max}")
        println(s"Min volume: ${volumes.min}")
        (seedBlocks, remainingJoints)
      }
    } else {
      // Find joints that cut input volume into approximately equal pieces
      val (partitionBlocks, leftOverJoints) = findSeedBlocks(jointSets, inputVolume, numPartitions, jointSetSpan)
      // Sufficient partitions found
      if (partitionBlocks.length == numPartitions) {
        val volumes = partitionBlocks.map(_.volume)
        val avgVolume = volumes.foldLeft(0.0)(_ + _) / volumes.length
        println("\nLoad Balance Info:")
        println(s"Number of partitions: ${volumes.length}")
        println(s"Average Volume: $avgVolume")
        println(s"Max volume: ${volumes.max}")
        println(s"Min volume: ${volumes.min}")
        (partitionBlocks, remainingJoints ++ leftOverJoints ++ jointSets.tail.flatten)
      // Unable to find sufficient partitions, run with greater span
      } else {
        generateSeedBlocks(jointSets, inputVolume, numPartitions, jointSetSpan + 1, partitionBlocks,
          remainingJoints ++ leftOverJoints)
      }
    }
  }

  /**
    * Find specified number of seed blocks by cycling through persistent joint sets, searching for
    * joints that divide input volume into approximately equal volumes.
    *
    * @param jointSets Seq of Seq's of Joints representing input joint sets
    * @param inputVolume Block representing input rock volume
    * @param numPartitions Number of partitions desired
    * @param jointSetSpan Number of joint sets to search at a time for partition generation
    * @param seedBlocks Blocks that will serve as partitions
    * @param remainingJoints Seq of joints that were not used in seed block generation
    * @return Returns a tuple containing a Seq of blocks and a Seq of joints. The Seq of blocks
    *         are the the desired partitions. The Seq of joints are the remaining joints that were
    *         not used to generate the partitions.
    */
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

  /**
    * Search input joint sets for joints that divide input volume into approximately equal volumes
    * based on desired number of partitions.
    *
    * @param jointSets Seq of Seq's of Joints representing input joint sets
    * @param inputVolume Block representing input rock volume
    * @param numPartitions Number of partitions desired
    * @param jointSetSpan Number of joint sets to search at a time for partition generation
    * @param spanCount Number or joint sets to search for seed joints
    * @param seedJoints Seq of joints that were not used in seed block generation
    * @param remainingJoints Seq of joints that were not selected seed joints
    * @return Returns a tuple containing two Seq's of Joints. The first Seq contains the seed joints that
    *         best divide the input volume. The second Seq contains the joints that remain from the joint
    *         sets that have been checked.
    */
  @tailrec
  def searchJointSets(jointSets: Seq[Seq[Joint]], inputVolume: Block, numPartitions: Int, jointSetSpan: Int,
                      spanCount: Int = 1, seedJoints: Seq[Joint] = Seq.empty[Joint],
                      remainingJoints: Seq[Joint] = Seq.empty[Joint]): (Seq[Joint], Seq[Joint]) = {
    // Checked specified span of joint sets, return
    if (spanCount > jointSetSpan) {
      (seedJoints, remainingJoints)
    } else {
      val totalVolume = inputVolume.volume
      // Determine remaining partitions that need to be found.
      val remainingPartitions = numPartitions / jointSetSpan
      val volumePerPiece = totalVolume / remainingPartitions
      val newSeedJoints = findSeedJoints(jointSets.head, inputVolume, totalVolume, volumePerPiece)
      val remainingFromJointSet = jointSets.head.diff(newSeedJoints)
      searchJointSets(jointSets.tail, inputVolume, numPartitions, jointSetSpan, spanCount + 1,
        seedJoints ++ newSeedJoints, remainingJoints ++ remainingFromJointSet)
    }
  }

  /**
    * Cycles through persistent joint set to find joints that approximately balances load across processes
    *
    * @param jointSet Seq of Joints representing a persistent joint set
    * @param initialVolume Block representing input rock volume that should be divided into approximately equal pieces
    *                      for each process to operate on
    * @param totalVolume Total volume of rock volume in analysis
    * @param volumePerPiece Ideal volume per partition for load balance
    * @param selectedJoints Seq of Joints that have been identified as seed joints
    * @return Seq of joints that divide initial block into approximately equal volumes
    */
  @tailrec
  def findSeedJoints(jointSet: Seq[Joint], initialVolume: Block, totalVolume: Double, volumePerPiece: Double,
                     selectedJoints: Seq[Joint] = Seq.empty[Joint]): Seq[Joint] = {
    // More than one joint in input joint set
    if (jointSet.length > 1) {
      val jointOption = testVolumes(jointSet(0), initialVolume, volumePerPiece)
      // Joint did not meet criteria
      if (jointOption.isEmpty) {
        findSeedJoints(jointSet.tail, initialVolume, totalVolume, volumePerPiece, selectedJoints)
      } else {
        // Joint meets criteria
        val remainingVolume = jointOption.get
        // Remaining volume small enough, return
        if (remainingVolume.volume <= volumePerPiece) {
          selectedJoints :+ jointSet(0)
          // Large volume still remains, keep cycling
        } else {
          findSeedJoints(jointSet.tail, remainingVolume, totalVolume, volumePerPiece, selectedJoints :+ jointSet(0))
        }
      }
    // Only one joint in input set
    // Continue subdividing
    } else if (initialVolume.volume > volumePerPiece) {
      val lastBlocks = initialVolume cut jointSet.head
      // Joint does not intersect volume
      if (lastBlocks.length == 1) {
        selectedJoints
        // Joint intersects volume
      } else {
        selectedJoints :+ jointSet.head
      }
    // Sufficiently subdivided  
    } else {
      selectedJoints
    }
  }

  /**
    * Test whether input joint divides initial volume into blocks of approximately the desired volume
    *
    * @param joint Joint to test
    * @param initialVolume Volume that is to be subdivided
    * @param desiredVolume Desired volume of subdivision
    * @return If joint satisfactorily subdivides initial volume, returns remaining volume that is to
    *         be further subdivided. Otherwise, returns None.
    */
  def testVolumes(joint: Joint, initialVolume: Block, desiredVolume: Double): Option[Block] = {
    val blocks = initialVolume cut joint
    val nonRedundantBlocks = blocks map { case block @ Block(blockCenter, _, _) =>
      Block(blockCenter, block.nonRedundantFaces)
    }

    // Sort generated blocks based on volume
    val sortedBlocks = nonRedundantBlocks.sortWith(_.volume < _.volume)

    // Joint does not intersect the block
    if (sortedBlocks.length == 1) {
      None
    } else {
      val smallBlockDiff = math.abs(sortedBlocks(0).volume - desiredVolume) / desiredVolume
      val largeBlockDiff = math.abs(sortedBlocks(1).volume - desiredVolume) / desiredVolume

      // Joint satisfactory, large volume remaining
      if (smallBlockDiff <= THRESHOLD) {
        Some(sortedBlocks(1))
      // Small volume remaining, select joint  
      } else if (sortedBlocks(1).volume <= desiredVolume) {
        Some(sortedBlocks(1))
      // Joint not satisfactory  
      } else {
        None
      }
    }
  }
}
