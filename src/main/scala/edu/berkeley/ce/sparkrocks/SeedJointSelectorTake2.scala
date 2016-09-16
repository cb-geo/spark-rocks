package edu.berkeley.ce.sparkrocks

import scala.annotation.tailrec

/**
  * Selects joints from the input joint set that will be used to seed the RDD in order to
  * maintain approximately balanced loads among processors.
  */
object SeedJointSelectorTake2 {

  @tailrec
  def generateSeedBlocks(jointSets: Seq[Seq[Joint]], inputVolume: Block, numPartitions: Int,
                         jointSetSpan: Int = 1, seedBlocks: Seq[Block] = Seq.empty[Block],
                         remainingJoints: Seq[Joint] = Seq.empty[Joint]): (Seq[Block], Seq[Joint]) = {
    if (jointSetSpan > jointSets.length) {
      if (seedBlocks.isEmpty) {
        throw new IllegalStateException("ERROR: Unable to find satisfactory seed joints")
      } else {
        println(s"""WARNING: Unable to find at least desired number of partitions. Continuing run with
        ${seedBlocks.length} partitions.""")
        (seedBlocks, remainingJoints)
      }
    }
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

  @tailrec
  def findSeedBlocks(jointSets: Seq[Seq[Joint]], inputVolume: Block, numPartitions: Int, jointSetSpan: Int,
                     seedBlocks: Seq[Block] = Seq.empty[Block], remainingJoints: Seq[Joint] = Seq.empty[Joint]):
  (Seq[Block], Seq[Joint]) = {
    // All joint sets checked or span too large
    if (jointSets.isEmpty || jointSetSpan > jointSets.length) {
      (seedBlocks, remainingJoints)
    }
    // Check that joint set is persistent
    if (jointSets.head(0).shape.isEmpty) {
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

  def searchJointSets(jointSets: Seq[Seq[Joint]], inputVolume: Block, numPartitions: Int, jointSetSpan: Int,
                      spanCount: Int = 1, seedJoints: Seq[Joint] = Seq.empty[Joint],
                      remainingJoints: Seq[Joint] = Seq.empty[Joint]): (Seq[Joint], Seq[Joint]) = {
    // Checked specified span of joint sets, return
    if (spanCount > jointSetSpan) {
      (seedJoints, remainingJoints)
    } else {
      val newSeedJoints = findSeedJoints(jointSets.head, inputVolume, numPartitions,
        spanCount, jointSetSpan)
      val remainingFromJointSet = jointSets.head.diff(newSeedJoints)
      searchJointSets(jointSets.tail, inputVolume, numPartitions, jointSetSpan, spanCount + 1,
        seedJoints ++ newSeedJoints, remainingJoints ++ remainingFromJointSet)
    }
  }

  def findSeedJoints(): Seq[Joint] = {

  }
}
