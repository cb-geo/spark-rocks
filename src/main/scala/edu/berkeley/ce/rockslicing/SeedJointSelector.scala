package edu.berkeley.ce.rockslicing

import scala.annotation.tailrec

/**
  * Selects joints from the input joint set that will be used to seed the RDD in order to maintain
  * approximately balanced loads among processors.
  */
object SeedJointSelector {

  def findSeedJoints(jointSet: Seq[Joint], rockVolume: Block, numProcessors: Integer): Option[Seq[Joint]] = {
    val volumePerProc = rockVolume.volume / numProcessors
    val seedJoints = cycleJointSet(jointSet, rockVolume, volumePerProc, Seq.empty[Joint])
    if (seedJoints.length != numProcessors) {
      println(s"Error: Could not find the required $numProcessors, could only find ${seedJoints.length}")
      None
    } else {
      Some(seedJoints)
    }
  }

  @tailrec
  private def cycleJointSet(jointSet: Seq[Joint], initialBlock: Block, volumePerProcessor: Double,
                            selectedJoints: Seq[Joint]): Seq[Joint] = {
    if (jointSet.length > 1) {
      // More than 1 joint in input joint set
      val jointOption = testVolumes(jointSet.head, jointSet.tail.head, initialBlock, volumePerProcessor)

      if (jointOption.isEmpty) {
        // No joints meet criteria
        cycleJointSet(jointSet.tail, initialBlock, volumePerProcessor, selectedJoints)
      } else {
        // Joint meets criteria
        val (seedJoint, remainingVolume) = jointOption.get
        if (remainingVolume.volume <= volumePerProcessor) {
          // Remaining volume small enough, return
          seedJoint +: selectedJoints
        } else {
          // Keep cycling through joints
          cycleJointSet(jointSet.tail, remainingVolume, volumePerProcessor, seedJoint +: selectedJoints)
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
        if (sortedLastBlocks(1).volume <= volumePerProcessor) {
          // Remaining volume small enough, return
          jointSet.head +: selectedJoints
        } else {
          // Remaining volume not small enough, return anyway
          println("ERROR: Remaining volume is still too large")
          selectedJoints
        }
      }
    }
  }

  private def testVolumes(joint1: Joint, joint2: Joint, initialBlock: Block,
                          desiredVolume: Double):Option[(Joint, Block)] = {
    val blockSet1 = initialBlock cut joint1
    val blockSet2 = initialBlock cut joint2

    val nonRedundantBlocks1 = blockSet1 map { case block @ Block(blockCenter, _, _) =>
        Block(blockCenter, block.nonRedundantFaces)
    }
    val nonRedundantBlocks2 = blockSet2 map { case block @ Block(blockCenter, _, _) =>
      Block(blockCenter, block.nonRedundantFaces)
    }

    val sortedBlocks1 = nonRedundantBlocks1.sortWith(_.volume < _.volume)
    val sortedBlocks2 = nonRedundantBlocks2.sortWith(_.volume < _.volume)

    if (sortedBlocks1.length == 1) {
      println("Joint1 outside volume")
      None
    } else if (sortedBlocks1.length == 1 && sortedBlocks2.length == 1) {
      println("Both joints outside volume")
      None
    } else if (sortedBlocks2.length == 1 && sortedBlocks1(1).volume <= desiredVolume) {
      println("Joint 1 satisfactory, joint2 outside volume")
      Some(joint1, sortedBlocks1(1))
    } else if (sortedBlocks2.length == 1) {
      println("Something is going wrong...")
      None
    } else if (sortedBlocks1(0).volume <= desiredVolume && sortedBlocks2(0).volume >= desiredVolume) {
      println("Joint2 satisfactory")
      Some(joint2, sortedBlocks2(1))
    } else if (sortedBlocks1(0).volume <= desiredVolume && sortedBlocks1(1).volume <= desiredVolume) {
      println("Joint1 satisfactory, last joint. BOTH volumes small")
      Some(joint1, sortedBlocks1(1))
    } else if (sortedBlocks2(0).volume <= desiredVolume && sortedBlocks2(1).volume <= desiredVolume) {
      println("Joint2 satisfactory, last joint. BOTH volumes small")
      Some(joint2, sortedBlocks2(1))
    } else if (sortedBlocks1(0).volume >= desiredVolume && sortedBlocks1(1).volume <= desiredVolume) {
      println("Joint1 satisfactory, last joint. LAST volume small")
      Some(joint1, sortedBlocks1(1))
    } else if (sortedBlocks2(0).volume >= desiredVolume && sortedBlocks2(1).volume <= desiredVolume) {
      println("Joint2 satisfactory, last joint. LAST volume small")
      Some(joint2, sortedBlocks2(1))
    } else {
      println("No joints satisfactory")
      None
    }
  }
}