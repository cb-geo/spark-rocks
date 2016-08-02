package edu.berkeley.ce.sparkrocks

import org.scalatest.FunSuite

import scala.math.sqrt

class JointGeneratorSpec extends FunSuite {
  test("Face should have outward pointing normal (0.0, 0.0, 1.0) and distance 1.0") {
    val globalOrigin = Array[Double](1.0, 1.0, 1.0)
    val boundingBox = (Array[Double](-1.0, -1.0, -1.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )

    val generatedRockData = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val expectedFace = Face(Array(0.0, 0.0, 1.0), 1.0, phi = 30.0, cohesion = 0.0)
    assert(expectedFace == generatedRockData.rockVolume.map(_.roundToTolerance()).head)
  }

  test("Face should have outward pointing normal (0.0, 0.0, -1.0) and distance 1.0") {
    val globalOrigin = Array[Double](1.0, 1.0, 1.0)
    val boundingBox = (Array[Double](-1.0, -1.0, -1.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )

    val generatedRockData = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val expectedFace = Face(Array(0.0, 0.0, -1.0), 1.0, phi = 30.0, cohesion = 0.0)
    assert(expectedFace == generatedRockData.rockVolume.map(_.roundToTolerance()).head)
  }

  test("Face should have outward pointing normal (1.0, 0.0, 0.0) and distance 1.0") {
    val globalOrigin = Array[Double](1.0, 1.0, 1.0)
    val boundingBox = (Array[Double](-1.0, -1.0, -1.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )

    val generatedRockData = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val expectedFace = Face(Array(1.0, 0.0, 0.0), 1.0, phi = 30.0, cohesion = 0.0)
    assert(expectedFace == generatedRockData.rockVolume.map(_.roundToTolerance()).head)
  }

  test("Face should have outward pointing normal (-1.0, 0.0, 0.0) and distance 1.0") {
    val globalOrigin = Array[Double](1.0, 1.0, 1.0)
    val boundingBox = (Array[Double](-1.0, -1.0, -1.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )

    val generatedRockData = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val expectedFace = Face(Array(-1.0, 0.0, 0.0), 1.0, phi = 30.0, cohesion = 0.0)
    assert(expectedFace == generatedRockData.rockVolume.map(_.roundToTolerance()).head)
  }

  test("Face should have outward pointing normal (0.0, 1.0, 0.0) and distance 1.0") {
    val globalOrigin = Array[Double](1.0, 1.0, 1.0)
    val boundingBox = (Array[Double](-1.0, -1.0, -1.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )

    val generatedRockData = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val expectedFace = Face(Array(0.0, 1.0, 0.0), 1.0, phi = 30.0, cohesion = 0.0)
    assert(expectedFace == generatedRockData.rockVolume.map(_.roundToTolerance()).head)
  }

  test("Face should have outward pointing normal (0.0, -1.0, 0.0) and distance 1.0") {
    val globalOrigin = Array[Double](1.0, 1.0, 1.0)
    val boundingBox = (Array[Double](-1.0, -1.0, -1.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )

    val generatedRockData = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val expectedFace = Face(Array(0.0, -1.0, 0.0), 1.0, phi = 30.0, cohesion = 0.0)
    assert(expectedFace == generatedRockData.rockVolume.map(_.roundToTolerance()).head)
  }

  test("Face should have outward pointing normal (0.0, 1/sqrt(2.0), 1.0/sqrt(2.0)) and distance 1.0") {
    val globalOrigin = Array[Double](0.0, 0.0, 0.0)
    val boundingBox = (Array[Double](-1.0, -1.0, -1.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(180.0, 45.0, Array(0.0, 1.0/sqrt(2.0), 1.0/sqrt(2.0)), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 45.0, 0.5, 100.0, 30.0, 0.0)
    )

    val generatedRockData = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val expectedFace = Seq(Face(Array(0.0, 1.0/sqrt(2.0), 1.0/sqrt(2.0)), 1.0, phi = 30.0, cohesion = 0.0))
    assert(expectedFace.map(_.roundToTolerance()) == generatedRockData.rockVolume.map(_.roundToTolerance()))
  }

  test("Face should have outward pointing normal (1.0/2.0, 0.0, sqrt(3.0)/2.0) and distance 1.0") {
    val globalOrigin = Array[Double](0.0, 0.0, 0.0)
    val boundingBox = (Array[Double](-1.0, -1.0, -1.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(270, 30.0, Array(1.0/2.0, 0.0, sqrt(3.0)/2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 45.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedRockData = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val expectedFace = Seq(Face(Array(1.0/2.0, 0.0, sqrt(3.0)/2.0), 1.0, phi = 30.0, cohesion = 0.0))
    assert(expectedFace.map(_.roundToTolerance()) == generatedRockData.rockVolume.map(_.roundToTolerance()))
  }

  test("Rock volume faces should be two cube") {
    val globalOrigin = Array[Double](1.0, 1.0, 1.0)
    val boundingBox = (Array[Double](-1.0, -1.0, -1.0), Array[Double](3.0, 3.0, 3.0))
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
    val expectedOrigin = Array[Double](1.0, 1.0, 1.0)
    val expectedLowerLeftCorner = Array[Double](-1.0, -1.0, -1.0)
    val expectedUpperRightCorner = Array[Double](3.0, 3.0, 3.0)

    val expectedRockVolume = Seq[Face](
      Face(Array(0.0, -1.0, 0.0), 1.0, 30.0, 0.0),
      Face(Array(0.0, 1.0, 0.0), 1.0, 30.0, 0.0),
      Face(Array(-1.0, 0.0, 0.0), 1.0, 30.0, 0.0),
      Face(Array(1.0, 0.0, 0.0), 1.0, 30.0, 0.0),
      Face(Array(0.0, 0.0, -1.0), 1.0, 30.0, 0.0),
      Face(Array(0.0, 0.0, 1.0), 1.0, 30.0, 0.0)
    )
    assert((expectedOrigin sameElements generatedInput.origin) &&
      (expectedLowerLeftCorner sameElements generatedInput.lowerLeftCorner) &&
      (expectedUpperRightCorner sameElements generatedInput.upperRightCorner) &&
      expectedRockVolume == generatedInput.rockVolume.map(_.roundToTolerance())
    )
  }

  test("Master joint should be x-y plane centered at (-1.0, -1.0, -1.0)") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](-1.0, -1.0, -1.0), Array[Double](3.0, 3.0, 3.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 0.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)

    val expectedJoints = Seq(
      Joint(Array(0.0, 0.0, -1.0), Array(0.5, 0.5, 0.5), Array(-1.0, -1.0, -1.0), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty)
    )
    assert(expectedJoints == generatedInput.masterJoints)
  }

  test("Master joint should have normal (sqrt(3.0)/2.0, 0.0, 1.0/2.0) and be centered at (-1.0, -1.0, -1.0)") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](-1.0, -1.0, -1.0), Array[Double](3.0, 3.0, 3.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(270.0, 30.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)

    val expectedJoints = Seq(
      Joint(Array(-1.0/2.0, 0.0, -sqrt(3.0)/2.0), Array(0.5, 0.5, 0.5), Array(-1.0, -1.0, -1.0), phi = 30.0,
        cohesion = 0.0, shape = Vector.empty)
    )

    assert(expectedJoints.zip(generatedInput.masterJoints) forall { case (joint1, joint2) =>
      joint1.approximateEquals(joint1)
    })
  }

  test("Generated joint set should be x-z plane, spaced 0.5 apart") {
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
    val expectedOrigin = Array[Double](0.5, 0.5, 0.5)
    val expectedLowerLeftCorner = Array[Double](0.0, 0.0, 0.0)
    val expectedUpperRightCorner = Array[Double](1.0, 1.0, 1.0)

    val expectedJoints = Seq[Seq[Joint]](
      Seq[Joint](
        Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(1.0, 1.0, 1.0), phi = 30.0,
          cohesion = 0.0, shape = Vector.empty),
        Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(0.5, 0.5, 0.5), phi = 30.0,
          cohesion = 0.0, shape = Vector.empty),
        Joint(Array(0.0, 1.0, 0.0), Array(0.5, 0.5, 0.5), Array(0.0, 0.0, 0.0), phi = 30.0,
          cohesion = 0.0, shape = Vector.empty))
    )

    assert((expectedOrigin sameElements generatedInput.origin) &&
      (expectedLowerLeftCorner sameElements generatedInput.lowerLeftCorner) &&
      (expectedUpperRightCorner sameElements generatedInput.upperRightCorner) &&
      expectedJoints.head.zip(generatedInput.jointSets.head).forall { case (joint1, joint2) =>
        joint1.approximateEquals(joint1)
      })
  }

  test("Generated joint set should be pi planes, spaced sqrt(3.0)/2.0 apart") {
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
      // Pi-plane in terms of strike and dip with sqrt(3.0)/2.0 spacing
      JointSet(225.0, 54.73561032, 0.866025404, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val expectedOrigin = Array[Double](0.5, 0.5, 0.5)
    val expectedLowerLeftCorner = Array[Double](0.0, 0.0, 0.0)
    val expectedUpperRightCorner = Array[Double](1.0, 1.0, 1.0)

    val expectedJoints = Seq[Seq[Joint]](
      Seq[Joint](
        Joint(Array(-1.0/sqrt(3.0), -1.0/sqrt(3.0), -1.0/sqrt(3.0)), Array(0.5, 0.5, 0.5),
          Array(1.0, 1.0, 1.0), phi = 30.0, cohesion = 0.0, shape = Vector.empty),
        Joint(Array(-1.0/sqrt(3.0), -1.0/sqrt(3.0), -1.0/sqrt(3.0)), Array(0.5, 0.5, 0.5),
          Array(0.5, 0.5, 0.5), phi = 30.0, cohesion = 0.0, shape = Vector.empty),
        Joint(Array(-1.0/sqrt(3.0), -1.0/sqrt(3.0), -1.0/sqrt(3.0)), Array(0.5, 0.5, 0.5),
          Array(0.0, 0.0, 0.0), phi = 30.0, cohesion = 0.0, shape = Vector.empty))
    )

    assert((expectedOrigin sameElements generatedInput.origin) &&
      (expectedLowerLeftCorner sameElements generatedInput.lowerLeftCorner) &&
      (expectedUpperRightCorner sameElements generatedInput.upperRightCorner) &&
      expectedJoints.head.zip(generatedInput.jointSets.head).forall { case (joint1, joint2) =>
        joint1.approximateEquals(joint1)
      })
  }

  test("Generated joint set should be pi planes rotated 90 degrees CCW, spaced sqrt(3.0)/2.0 apart") {
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
      // Pi-plane in terms of strike and dip with sqrt(3.0)/2.0 spacing
      JointSet(135.0, 54.73561032, 0.866025404, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val expectedOrigin = Array[Double](0.5, 0.5, 0.5)
    val expectedLowerLeftCorner = Array[Double](0.0, 0.0, 0.0)
    val expectedUpperRightCorner = Array[Double](1.0, 1.0, 1.0)

    val expectedJoints = Seq[Seq[Joint]](
      Seq[Joint](
        Joint(Array(1.0/sqrt(3.0), -1.0/sqrt(3.0), -1.0/sqrt(3.0)), Array(0.5, 0.5, 0.5),
          Array(0.0, 1.0, 1.0), phi = 30.0, cohesion = 0.0, shape = Vector.empty),
        Joint(Array(1.0/sqrt(3.0), -1.0/sqrt(3.0), -1.0/sqrt(3.0)), Array(0.5, 0.5, 0.5),
          Array(0.5, 0.5, 0.5), phi = 30.0, cohesion = 0.0, shape = Vector.empty),
        Joint(Array(1.0/sqrt(3.0), -1.0/sqrt(3.0), -1.0/sqrt(3.0)), Array(0.5, 0.5, 0.5),
          Array(1.0, 0.0, 0.0), phi = 30.0, cohesion = 0.0, shape = Vector.empty))
    )

    assert((expectedOrigin sameElements generatedInput.origin) &&
      (expectedLowerLeftCorner sameElements generatedInput.lowerLeftCorner) &&
      (expectedUpperRightCorner sameElements generatedInput.upperRightCorner) &&
      expectedJoints.head.zip(generatedInput.jointSets.head).forall { case (joint1, joint2) =>
        joint1.approximateEquals(joint1)
      })
  }
}
