package edu.berkeley.ce.sparkrocks

import scala.annotation.tailrec

/**
  * Selects joints from the input joint set that will be used to seed the RDD in order to
  * maintain approximately balanced loads among processors.
  */
object SeedJointSelector {
  // Threshold for joint acceptance
  val THRESHOLD = 0.35
  val LOWER_BOUND = 0.65

  /**
    * Generate specified number of partitions using input joint sets
    *
    * @param jointSets Seq of Seq's of Joints representing input joint sets
    * @param inputVolume Block representing input rock volume
    * @param numPartitions Number of partitions desired
    * @param jointSetSpan Number of joint sets to search at a time for partition generation
    * @return Returns a tuple containing a Seq of blocks and a Seq of joints. The Seq of blocks
    *         are the the desired partitions. The Seq of joints are the remaining joints that were
    *         not used to generate the partitions.
    */
  @tailrec
  def generateSeedBlocks(jointSets: Seq[Seq[Joint]], inputVolume: Block, numPartitions: Int,
                         jointSetSpan: Int = 1): (Seq[Block], Seq[Joint]) = {
    assert(jointSets.nonEmpty)

    // Find joints that cut input volume into approximately equal pieces
    val (partitionBlocks, leftOverJoints) = findSeedBlocks(jointSets, inputVolume, numPartitions, jointSetSpan)

    if (partitionBlocks.length >= numPartitions || jointSetSpan == jointSets.length) {
      // Sufficient partitions found OR joint set span at maximum
      val nonRedundantPartitions = partitionBlocks.map { case block @ Block(center, _, _) =>
          Block(center, block.nonRedundantFaces)
      }
      val volumes = nonRedundantPartitions.map(_.volume)

      val avgVolume = volumes.sum / volumes.length
      println("\nLoad Balance Info:")
      println(s"Number of partitions: ${volumes.length}")
      println(s"Average Volume: %.2f".format(avgVolume))
      println(s"Max volume: %.2f".format(volumes.max))
      println(s"Min volume: %.2f".format(volumes.min))
      (partitionBlocks, leftOverJoints ++ jointSets.drop(jointSetSpan).flatten)
    } else {
      // Unable to find sufficient partitions, run with greater span
      generateSeedBlocks(jointSets, inputVolume, numPartitions, jointSetSpan + 1)
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
    if (jointSets.isEmpty || jointSetSpan > jointSets.length) {
      // All joint sets checked or span too large
      (seedBlocks, remainingJoints)
    } else if (jointSets.head(0).shape.isEmpty) {
      val (seedJoints, leftOverJoints) = searchJointSets(jointSets, inputVolume, numPartitions, jointSetSpan)
      val partitioning = seedJoints.foldLeft(Seq(inputVolume)) { (currentBlocks, joint) =>
        currentBlocks.flatMap(_.cut(joint))
      }
      if (partitioning.length < numPartitions) {
        // Insufficient partitions found
        if (jointSetSpan == jointSets.length) {
          // Joint span will exceed joint set length on next recursion
          (partitioning, remainingJoints ++ leftOverJoints)
        } else {
          // Joint span won't exceed joint set length on next recursion
          findSeedBlocks(jointSets.tail, inputVolume, numPartitions, jointSetSpan, partitioning,
            remainingJoints ++ jointSets.head)
        }
      } else {
        // Sufficient partitions, return
        (partitioning, remainingJoints ++ leftOverJoints)
      }
    } else {
      // Joint set non-persistent, check next one
      findSeedBlocks(jointSets.tail, inputVolume, numPartitions, jointSetSpan, seedBlocks,
        remainingJoints ++ jointSets.head)
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
    if (spanCount > jointSetSpan) {
      // Checked specified span of joint sets, return
      (seedJoints, remainingJoints)
    } else {
      /*
       * Determine remaining partitions that need to be found. It is not possible to know before hand how many
       * joints will cross each other when selecting seed joints from multiple joint sets, making it difficult
       * to determine exactly how many partitions are yet to found. This approach essentially divides the number
       * of partitions required equally among the number of joint sets. In most cases this will lead to a
       * greater number of partitions, but tries to ensure that at least the number of partitions requested
       * is found.
       */
      val totalVolume = inputVolume.volume
      val remainingPartitions = numPartitions / jointSetSpan
      val volumePerPiece = totalVolume / remainingPartitions
      val (newSeedJoints, remainingFromJointSet) = findSeedJoints(jointSets.head, inputVolume,
        totalVolume, volumePerPiece)
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
    * @param selectedJoints Seq of joints that have been identified as seed joints
    * @param remainingJoints Seq of joints that were not selected as seed joints
    * @return Tuple containing two Seq's of joints. The first Seq contains joints that divide initial block into
    *         approximately equal volumes. The second Seq is the joints remaining from the input joint set.
    */
  @tailrec
  def findSeedJoints(jointSet: Seq[Joint], initialVolume: Block, totalVolume: Double, volumePerPiece: Double,
                     selectedJoints: Seq[Joint] = Seq.empty[Joint],
                     remainingJoints: Seq[Joint] = Seq.empty[Joint]): (Seq[Joint], Seq[Joint]) = {
    if (jointSet.length > 1) {
      // More than one joint in input joint set
      val jointOption = testVolumes(jointSet(0), initialVolume, volumePerPiece)
      if (jointOption.isEmpty) {
        // Joint did not meet criteria
        findSeedJoints(jointSet.tail, initialVolume, totalVolume, volumePerPiece, selectedJoints,
          remainingJoints :+ jointSet(0))
      } else {
        // Joint meets criteria
        val remainingVolume = jointOption.get
        val nonRedundantVolume = Block(remainingVolume.center, remainingVolume.nonRedundantFaces)
        if (nonRedundantVolume.volume <= volumePerPiece &&
            nonRedundantVolume.volume >= LOWER_BOUND * volumePerPiece) {
          // Remaining volume small enough, return
          (selectedJoints :+ jointSet(0), remainingJoints ++ jointSet.tail)
        } else {
          // Large volume still remains, keep cycling
          findSeedJoints(jointSet.tail, nonRedundantVolume, totalVolume, volumePerPiece,
            selectedJoints :+ jointSet(0), remainingJoints)
        }
      }
    } else if (initialVolume.volume > volumePerPiece) {
      // Continue subdividing using single joint
      val lastBlocks = initialVolume cut jointSet(0)
      if (lastBlocks.length == 1) {
        // Joint does not intersect volume
        (selectedJoints, remainingJoints :+ jointSet(0))
      } else {
        // Joint intersects volume
        (selectedJoints :+ jointSet(0), remainingJoints)
      }
    } else {
      // Sufficiently subdivided
      (selectedJoints, remainingJoints :+ jointSet(0))
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

    if (sortedBlocks.length == 1) {
      // Joint does not intersect the block
      None
    } else {
      val smallBlockDiff = math.abs(sortedBlocks(0).volume - desiredVolume) / desiredVolume

      if (smallBlockDiff <= THRESHOLD) {
        // Joint satisfactory, large volume remaining
        Some(sortedBlocks(1))
      } else if (sortedBlocks(1).volume <= desiredVolume && sortedBlocks(1).volume >= LOWER_BOUND * desiredVolume) {
        // Small volume remaining, select joint
        Some(sortedBlocks(1))
      } else {
        // Joint not satisfactory
        None
      }
    }
  }
}
