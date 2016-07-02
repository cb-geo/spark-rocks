package edu.berkeley.ce.rockslicing

import scala.annotation.tailrec

/**
  * Selects joints from the input joint set that will be used to seed the RDD in order to maintain
  * approximately balanced loads among processors.
  */
object SeedJointSelector {

  def findSeedJoints(jointSet: Seq[Joint], rockVolume: Block, numProcessors: Integer): Option[Seq[Joint]] = {
    if (jointSet.length < numProcessors - 1) {
      println(s"Error: Not enough joints to generate required number of seed joints. ${jointSet.length} joints but"+
      s"$numProcessors processors. Try using less processors.")
      return None
    }
    val volumePerProc = rockVolume.volume / (numProcessors + 1)
    println("Volume per processor "+volumePerProc)
    println("Total volume: "+rockVolume.volume+"\n")
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
//    if (jointSet.length > 1) {
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
//    } else {
//      // Only one joint in input joint set
//      val lastBlocks = initialBlock cut jointSet.head
//      val nonRedundantLastBlocks = lastBlocks map { case block @ Block(blockCenter, _, _) =>
//          Block(blockCenter, block.nonRedundantFaces)
//      }
//      val sortedLastBlocks = nonRedundantLastBlocks.sortWith(_.volume < _.volume)
//      if (sortedLastBlocks.length == 1) {
//        // Joint does not intersect volume
//        selectedJoints
//      } else {
//        // Joint intersect volume
//        if (sortedLastBlocks(1).volume <= volumePerProcessor) {
//          // Remaining volume small enough, return
//          jointSet.head +: selectedJoints
//        } else {
//          // Remaining volume not small enough, return anyway
//          println("ERROR: Remaining volume is still too large")
//          selectedJoints
//        }
//      }
//    }
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
      println("Joint 1:")
      println(s"Normal: ${joint1.a}, ${joint1.b}, ${joint1.c}")
      println(s"Center: ${joint1.centerX}, ${joint1.centerY}, ${joint1.centerZ}")
      println(s"Origin: ${joint1.localX}, ${joint1.localY}, ${joint1.localY}")
      println("Joint 1 outside volume\n")
      None
    } else if (sortedBlocks1.length == 1 && sortedBlocks2.length == 1) {
      println("Joint 1:")
      println(s"Normal: ${joint1.a}, ${joint1.b}, ${joint1.c}")
      println(s"Center: ${joint1.centerX}, ${joint1.centerY}, ${joint1.centerZ}")
      println(s"Origin: ${joint1.localX}, ${joint1.localY}, ${joint1.localY}")
      println("Joint 2:")
      println(s"Normal: ${joint2.a}, ${joint2.b}, ${joint2.c}")
      println(s"Center: ${joint2.centerX}, ${joint2.centerY}, ${joint2.centerZ}")
      println(s"Origin: ${joint2.localX}, ${joint2.localY}, ${joint2.localY}")
      println("Both joints outside volume\n")
      None
    } else if (sortedBlocks2.length == 1 && sortedBlocks1(1).volume <= desiredVolume) {
      println("Joint 1:")
      println(s"Normal: ${joint1.a}, ${joint1.b}, ${joint1.c}")
      println(s"Center: ${joint1.centerX}, ${joint1.centerY}, ${joint1.centerZ}")
      println(s"Origin: ${joint1.localX}, ${joint1.localY}, ${joint1.localY}")
      println("Joint 2:")
      println(s"Normal: ${joint2.a}, ${joint2.b}, ${joint2.c}")
      println(s"Center: ${joint2.centerX}, ${joint2.centerY}, ${joint2.centerZ}")
      println(s"Origin: ${joint2.localX}, ${joint2.localY}, ${joint2.localY}")
      println("Joint 1 satisfactory, joint2 outside volume\n")
      Some(joint1, sortedBlocks1(1))
    } else if (sortedBlocks2.length == 1) {
      println("Joint 2:")
      println(s"Normal: ${joint2.a}, ${joint2.b}, ${joint2.c}")
      println(s"Center: ${joint2.centerX}, ${joint2.centerY}, ${joint2.centerZ}")
      println(s"Origin: ${joint2.localX}, ${joint2.localY}, ${joint2.localY}")
      println("Something is going wrong...\n")
      None
    } else if (sortedBlocks1(0).volume <= desiredVolume && sortedBlocks2(0).volume >= desiredVolume) {
      println("Joint 1:")
      println(s"Normal: ${joint1.a}, ${joint1.b}, ${joint1.c}")
      println(s"Center: ${joint1.centerX}, ${joint1.centerY}, ${joint1.centerZ}")
      println(s"Origin: ${joint1.localX}, ${joint1.localY}, ${joint1.localY}")
      println("Joint 2:")
      println(s"Normal: ${joint2.a}, ${joint2.b}, ${joint2.c}")
      println(s"Center: ${joint2.centerX}, ${joint2.centerY}, ${joint2.centerZ}")
      println(s"Origin: ${joint2.localX}, ${joint2.localY}, ${joint2.localY}")
      println("Joint2 satisfactory\n")
      Some(joint2, sortedBlocks2(1))
    } else if (sortedBlocks1(0).volume <= desiredVolume && sortedBlocks1(1).volume <= desiredVolume) {
      println("Joint 1:")
      println(s"Normal: ${joint1.a}, ${joint1.b}, ${joint1.c}")
      println(s"Center: ${joint1.centerX}, ${joint1.centerY}, ${joint1.centerZ}")
      println(s"Origin: ${joint1.localX}, ${joint1.localY}, ${joint1.localY}")
      println("Joint 2:")
      println(s"Normal: ${joint2.a}, ${joint2.b}, ${joint2.c}")
      println(s"Center: ${joint2.centerX}, ${joint2.centerY}, ${joint2.centerZ}")
      println(s"Origin: ${joint2.localX}, ${joint2.localY}, ${joint2.localY}")
      println("Joint1 satisfactory, last joint. BOTH volumes small\n")
      Some(joint1, sortedBlocks1(1))
    } else if (sortedBlocks2(0).volume <= desiredVolume && sortedBlocks2(1).volume <= desiredVolume) {
      println("Joint 1:")
      println(s"Normal: ${joint1.a}, ${joint1.b}, ${joint1.c}")
      println(s"Center: ${joint1.centerX}, ${joint1.centerY}, ${joint1.centerZ}")
      println(s"Origin: ${joint1.localX}, ${joint1.localY}, ${joint1.localY}")
      println("Joint 2:")
      println(s"Normal: ${joint2.a}, ${joint2.b}, ${joint2.c}")
      println(s"Center: ${joint2.centerX}, ${joint2.centerY}, ${joint2.centerZ}")
      println(s"Origin: ${joint2.localX}, ${joint2.localY}, ${joint2.localY}")
      println("Joint2 satisfactory, last joint. BOTH volumes small\n")
      Some(joint2, sortedBlocks2(1))
    } else if (sortedBlocks1(0).volume >= desiredVolume && sortedBlocks1(1).volume <= desiredVolume) {
      println("Joint 1:")
      println(s"Normal: ${joint1.a}, ${joint1.b}, ${joint1.c}")
      println(s"Center: ${joint1.centerX}, ${joint1.centerY}, ${joint1.centerZ}")
      println(s"Origin: ${joint1.localX}, ${joint1.localY}, ${joint1.localY}")
      println("Joint 2:")
      println(s"Normal: ${joint2.a}, ${joint2.b}, ${joint2.c}")
      println(s"Center: ${joint2.centerX}, ${joint2.centerY}, ${joint2.centerZ}")
      println(s"Origin: ${joint2.localX}, ${joint2.localY}, ${joint2.localY}")
      println("Joint1 satisfactory, last joint. LAST volume small\n")
      Some(joint1, sortedBlocks1(1))
    } else if (sortedBlocks2(0).volume >= desiredVolume && sortedBlocks2(1).volume <= desiredVolume) {
      println("Joint 1:")
      println(s"Normal: ${joint1.a}, ${joint1.b}, ${joint1.c}")
      println(s"Center: ${joint1.centerX}, ${joint1.centerY}, ${joint1.centerZ}")
      println(s"Origin: ${joint1.localX}, ${joint1.localY}, ${joint1.localY}")
      println("Joint 2:")
      println(s"Normal: ${joint2.a}, ${joint2.b}, ${joint2.c}")
      println(s"Center: ${joint2.centerX}, ${joint2.centerY}, ${joint2.centerZ}")
      println(s"Origin: ${joint2.localX}, ${joint2.localY}, ${joint2.localY}")
      println("Joint2 satisfactory, last joint. LAST volume small\n")
      Some(joint2, sortedBlocks2(1))
    } else {
      println("Joint 1:")
      println(s"Normal: ${joint1.a}, ${joint1.b}, ${joint1.c}")
      println(s"Center: ${joint1.centerX}, ${joint1.centerY}, ${joint1.centerZ}")
      println(s"Origin: ${joint1.localX}, ${joint1.localY}, ${joint1.localY}")
      println("Joint 2:")
      println(s"Normal: ${joint2.a}, ${joint2.b}, ${joint2.c}")
      println(s"Center: ${joint2.centerX}, ${joint2.centerY}, ${joint2.centerZ}")
      println(s"Origin: ${joint2.localX}, ${joint2.localY}, ${joint2.localY}")
      println("No joints satisfactory\n")
      None
    }
  }
}