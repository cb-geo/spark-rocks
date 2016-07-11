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

  test("Single joint should be selected as seed joint - should be x-z plane at center of unit cube") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = Array[Double](0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
    val rockVolume = Array[Array[Double]](
      Array(0.0, 90.0, 0.0, 0.0, 0.0, 30.0, 0.0),
      Array(0.0, 90.0, 0.0, 1.0, 0.0, 30.0, 0.0),
      Array(90.0, 90.0, 0.0, 0.0, 0.0, 30.0, 0.0),
      Array(90.0, 90.0, 1.0, 0.0, 0.0, 30.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, 0.0, 30.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, 1.0, 30.0, 0.0)
    )
    val jointData = Array[Array[Double]](
      Array(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val seedJoints = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 2)

    val expectedJoints = Seq[Joint](
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(0.5, 0.5, 0.5), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty)
    )

    val jointComparison = expectedJoints.zip(seedJoints.get) map { case (joint1, joint2) =>
      compareJoints(joint1, joint2)
    }

    assert(!jointComparison.contains(false))
  }

  test("Single joint should be selected as seed joint - should be x-z plane at center of two cube") {
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
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val seedJoints = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 2)

    val expectedJoints = Seq[Joint](
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(1.0, 1.0, 1.0), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty)
      )

    val jointComparison = expectedJoints.zip(seedJoints.get) map { case (joint1, joint2) =>
      compareJoints(joint1, joint2)
    }

    assert(!jointComparison.contains(false))
  }

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
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val seedJoints = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 3)

    val expectedJoints = Seq[Joint](
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(0.5, 0.5, 0.5), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty),
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(1.5, 1.5, 1.5), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty)
    )

    val jointComparison = expectedJoints.zip(seedJoints.get) map { case (joint1, joint2) =>
      compareJoints(joint1, joint2)
    }

    assert(!jointComparison.contains(false))
  }

  test("Three seed joints should be selected - x-z planes dividing two cube into quarters") {
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
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val seedJoints = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 4)

    val expectedJoints = Seq[Joint](
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(0.5, 0.5, 0.5), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty),
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(1.0, 1.0, 1.0), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty),
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(1.5, 1.5, 1.5), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty)
    )

    val jointComparison = expectedJoints.zip(seedJoints.get) map { case (joint1, joint2) =>
      compareJoints(joint1, joint2)
    }

    assert(!jointComparison.contains(false))
  }

  test("20 seed joints should be selected - x-z planes dividing two cube") {
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
      Array(0.0, 90.0, 0.02, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val seedJoints = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 21)

    assert(seedJoints.get.length == 20)
  }

  test("20 seed joints should be selected - pi planes dividing two cube") {
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
      Array(45.0, 45.0, 0.01, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val seedJoints = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 21)

    assert(seedJoints.get.length == 20)
  }

  test("Seed joint test on first Spaulding joint set") {
    val globalOrigin = Array[Double](0.0, 0.0, 0.0)
    val boundingBox = Array[Double](-5.0, -5.0, -5.0, 5.0, 5.0, 5.0)
    val rockVolume = Array[Array[Double]](
      Array(90.0, 90.0, 4.0, 0.0, 0.0, 30.0, 0.0),
      Array(0.0, 90.0, 0.0, 4.0, 0.0, 30.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, 4.0, 30.0, 0.0),
      Array(90.0, 90.0, -4.0, 0.0, 0.0, 30.0, 0.0),
      Array(0.0, 90.0, 0.0, -4.0, 0.0, 30.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, -4.0, 30.0, 0.0)
    )
    val jointData = Array[Array[Double]](
      Array(60.0, 25.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val seedJoints = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 4)

    assert(seedJoints.get.length == 3)
  }
}