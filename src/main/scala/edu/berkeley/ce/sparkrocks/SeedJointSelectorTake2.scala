package edu.berkeley.ce.sparkrocks

import scala.annotation.tailrec

/**
  * Selects joints from the input joint set that will be used to seed the RDD in order to
  * maintain approximately balanced loads among processors.
  */
object SeedJointSelectorTake2 {

  @tailrec
  def generateSeedBlocks(jointSets: Seq[Seq[Joint]], inputVolume: Seq[Block], numPartitions: Int,
                         jointSetSpan: Int = 1): (Seq[Block], Seq[Joint]) = {
    if (jointSetSpan > jointSets.length) {
      throw new IllegalStateException("ERROR: Unable to find satisfactory seed joints")
    }
    // Find joints that cut input volume into approximately equal pieces
    val (seedJoints, remainingJoints) = findSeedJoints(jointSets, inputVolume, numPartitions, jointSetSpan)
    // Unable to find sufficient number of seed joints
    val seedBlocks = seedJoints.foldLeft(inputVolume) { (currentBlocks, joint) =>
      currentBlocks.flatMap(_.cut(joint))
    }
    // Sufficient partitions found
    if (seedBlocks.length >= numPartitions) {
      (seedBlocks, remainingJoints)
    // Unable to find sufficient partitions, run with greater span
    } else {
      generateSeedBlocks(jointSets, inputVolume, numPartitions, jointSetSpan + 1)
    }
  }

  @tailrec
  def findSeedJoints(jointSets: Seq[Seq[Joint]], inputVolume: Seq[Block], numPartitions: Int, jointSetSpan: Int,
                     seedJoints: Seq[Joint] = Seq.empty[Joint], remainingJoints: Seq[Joint] = Seq.empty[Joint]):
  (Seq[Joint], Seq[Joint]) = {
    // All joint sets checked, return
    if (jointSets.isEmpty) {
      (seedJoints, remainingJoints)
    }
    // Check that joint set is persistent
    if (jointSets.head(0).shape.isEmpty) {
      val selectedJoints = searchJointSets(jointSets.head, inputVolume, )
    // Joint set non-persistent, check next one
    } else {
      findSeedJoints(jointSets.tail, inputVolume, numPartitions, jointSetSpan, seedJoints, remainingJoints)
    }
  }
}
