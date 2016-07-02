package edu.berkeley.ce.rockslicing

import org.scalatest.FunSuite

class SeedJointSelectorSpec extends FunSuite {

  private def compareJoints(joint1: Joint, joint2: Joint): Boolean = {
    val norm1 = Array(NumericUtils.roundToTolerance(joint1.a), NumericUtils.roundToTolerance(joint1.b),
      NumericUtils.roundToTolerance(joint1.c))
    val norm2 = Array(NumericUtils.roundToTolerance(joint2.a), NumericUtils.roundToTolerance(joint2.b),
      NumericUtils.roundToTolerance(joint2.c))
    val center1 = Array(NumericUtils.roundToTolerance(joint1.centerX), NumericUtils.roundToTolerance(joint1.centerY),
      NumericUtils.roundToTolerance(joint1.centerZ))
    val center2 = Array(NumericUtils.roundToTolerance(joint2.centerX), NumericUtils.roundToTolerance(joint2.centerY),
      NumericUtils.roundToTolerance(joint2.centerZ))
    val origin1 = Array(NumericUtils.roundToTolerance(joint1.localX), NumericUtils.roundToTolerance(joint1.localY),
      NumericUtils.roundToTolerance(joint1.localZ))
    val origin2 = Array(NumericUtils.roundToTolerance(joint2.localX), NumericUtils.roundToTolerance(joint2.localY),
      NumericUtils.roundToTolerance(joint2.localZ))

    (norm1 sameElements norm2) && (center1 sameElements center2) && (origin1 sameElements origin2)
  }

//  test("Single joint should be selected as seed joint - should be x-z plane at center of unit cube") {
//    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
//    val boundingBox = Array[Double](0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
//    val rockVolume = Array[Array[Double]](
//      Array(0.0, 90.0, 0.0, 0.0, 0.0, 30.0, 0.0),
//      Array(0.0, 90.0, 0.0, 1.0, 0.0, 30.0, 0.0),
//      Array(90.0, 90.0, 0.0, 0.0, 0.0, 30.0, 0.0),
//      Array(90.0, 90.0, 1.0, 0.0, 0.0, 30.0, 0.0),
//      Array(0.0, 0.0, 0.0, 0.0, 0.0, 30.0, 0.0),
//      Array(0.0, 0.0, 0.0, 0.0, 1.0, 30.0, 0.0)
//    )
//    val jointData = Array[Array[Double]](
//      Array(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
//    )
//    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
//    val seedJoints = SeedJointSelector.findSeedJoints(generatedInput.jointSets.head,
//      Block(generatedInput.origin, generatedInput.rockVolume), numProcessors = 1)
//
//    val expectedJoints = Seq[Joint](
//      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(0.5, 0.5, 0.5), phi = 30.0,
//        cohesion = 0.0, shape = Vector.empty)
//    )
//
//    val jointComparison = expectedJoints.zip(seedJoints.get) map { case (joint1, joint2) =>
//      compareJoints(joint1, joint2)
//    }
//
//    assert(!jointComparison.contains(false))
//  }
//
//  test("Single joint should be selected as seed joint - should be x-z plane at center of two cube") {
//    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
//    val boundingBox = Array[Double](0.0, 0.0, 0.0, 2.0, 2.0, 2.0)
//    val rockVolume = Array[Array[Double]](
//      Array(0.0, 90.0, 0.0, 0.0, 0.0, 30.0, 0.0),
//      Array(0.0, 90.0, 0.0, 2.0, 0.0, 30.0, 0.0),
//      Array(90.0, 90.0, 0.0, 0.0, 0.0, 30.0, 0.0),
//      Array(90.0, 90.0, 2.0, 0.0, 0.0, 30.0, 0.0),
//      Array(0.0, 0.0, 0.0, 0.0, 0.0, 30.0, 0.0),
//      Array(0.0, 0.0, 0.0, 0.0, 2.0, 30.0, 0.0)
//    )
//    val jointData = Array[Array[Double]](
//      Array(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
//    )
//    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
//    val seedJoints = SeedJointSelector.findSeedJoints(generatedInput.jointSets.head,
//      Block(generatedInput.origin, generatedInput.rockVolume), numProcessors = 1)
//
//    val expectedJoints = Seq[Joint](
//      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(1.0, 1.0, 1.0), phi = 30.0,
//        cohesion = 0.0, shape = Vector.empty)
//    )
//
//    val jointComparison = expectedJoints.zip(seedJoints.get) map { case (joint1, joint2) =>
//      compareJoints(joint1, joint2)
//    }
//
//    assert(!jointComparison.contains(false))
//  }

  test("Two seed joints should be selected - x-z planes dividing two cube into thirds") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = Array[Double](0.0, 0.0, 0.0, 2.0, 2.0, 2.0)
    val rockVolume = Array[Array[Double]](
      Array(0.0, 90.0, 0.0, 0.0, 0.0, 30.0, 0.0),
      Array(0.0, 90.0, 0.0, 2.0, 0.0, 30.0, 0.0),
      Array(90.0, 90.0, 0.0, 0.0, 0.0, 30.0, 0.0),
      Array(90.0, 90.0, 2.0, 0.0, 0.0, 30.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, 0.0, 30.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, 2.0, 30.0, 0.0)
    )
    val jointData = Array[Array[Double]](
      Array(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val seedJoints = SeedJointSelector.findSeedJoints(generatedInput.jointSets.head,
      Block(generatedInput.origin, generatedInput.rockVolume), numProcessors = 2)

    val expectedJoints = Seq[Joint](
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(1.0, 1.0, 1.0), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty)
    )

    println("EXPECTED JOINTS:")
    expectedJoints.foreach { joint =>
      println(s"Normal: ${joint.a}, ${joint.b}, ${joint.c}")
      println(s"Center: ${joint.centerX}, ${joint.centerY}, ${joint.centerZ}")
      println(s"Origin: ${joint.localX}, ${joint.localY}, ${joint.localY}")
    }

    println("\nGENERATED JOINTS:")
    if (seedJoints.isEmpty) {
      println("EMPTY!!!!!!!!!!!!")
    } else {
      seedJoints.get.foreach { joint =>
        println(s"Normal: ${joint.a}, ${joint.b}, ${joint.c}")
        println(s"Center: ${joint.centerX}, ${joint.centerY}, ${joint.centerZ}")
        println(s"Origin: ${joint.localX}, ${joint.localY}, ${joint.localY}")
      }
    }

    val jointComparison = expectedJoints.zip(seedJoints.get) map { case (joint1, joint2) =>
      compareJoints(joint1, joint2)
    }

    assert(!jointComparison.contains(false))
  }
}
