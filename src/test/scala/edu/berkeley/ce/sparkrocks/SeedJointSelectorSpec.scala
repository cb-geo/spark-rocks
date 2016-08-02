package edu.berkeley.ce.sparkrocks

import org.scalatest.FunSuite

class SeedJointSelectorSpec extends FunSuite {
  test("Single joint should be selected as seed joint - should be x-z plane at center of unit cube") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](1.0, 1.0, 1.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 1.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(1.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 1.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedJoints, _) = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 2)

    val expectedJoints = Seq[Joint](
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(0.5, 0.5, 0.5), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty)
    )

    assert(expectedJoints.zip(seedJoints) forall { case (joint1, joint2) =>
      joint1.approximateEquals(joint1)
    })
  }

  test("Single joint should be selected as seed joint - should be x-z plane at center of two cube") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedJoints, _) = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 2)

    val expectedJoints = Seq[Joint](
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(1.0, 1.0, 1.0), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty)
      )

    assert(expectedJoints.zip(seedJoints) forall { case (joint1, joint2) =>
      joint1.approximateEquals(joint1)
    })
  }

  test("Two seed joints should be selected - x-z planes dividing two cube into thirds") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedJoints, _) = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 3)

    val expectedJoints = Seq[Joint](
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(0.5, 0.5, 0.5), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty),
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(1.5, 1.5, 1.5), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty)
    )

    assert(expectedJoints.zip(seedJoints) forall { case (joint1, joint2) =>
      joint1.approximateEquals(joint1)
    })
  }

  test("Three seed joints should be selected - x-z planes dividing two cube into quarters") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedJoints, _) = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 4)

    val expectedJoints = Seq[Joint](
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(0.5, 0.5, 0.5), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty),
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(1.0, 1.0, 1.0), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty),
      Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(1.5, 1.5, 1.5), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty)
    )

    assert(expectedJoints.zip(seedJoints) forall { case (joint1, joint2) =>
      joint1.approximateEquals(joint1)
    })
  }

  test("20 seed joints should be selected - x-z planes dividing two cube") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.02, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedJoints, _) = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 21)

    assert(seedJoints.length == 20)
  }

  test("20 seed joints should be selected - pi planes dividing two cube") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(45.0, 45.0, 0.01, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedJoints, _) = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 21)

    assert(seedJoints.length == 20)
  }

  test("Seed joint test on first Spaulding joint set") {
    val globalOrigin = Array[Double](0.0, 0.0, 0.0)
    val boundingBox = (Array[Double](-5.0, -5.0, -5.0), Array[Double](5.0, 5.0, 5.0))
    val rockVolume = Seq[InputFace](
      InputFace(90.0, 90.0, Array(4.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 4.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 4.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(-4.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, -4.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, -4.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(60.0, 25.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedJoints, _) = SeedJointSelector.searchJointSets(generatedInput.jointSets, initialBlock, 4)

    assert(seedJoints.length == 3)
  }
}