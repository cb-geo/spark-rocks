package edu.berkeley.ce.rockslicing

import org.scalatest.FunSuite

import scala.io.Source

class InputProcessingSpec extends FunSuite {

  def compareJointSets(jointSet1: JointSet, jointSet2: JointSet): Boolean = {
    if (jointSet1.strike == jointSet2.strike &&
      jointSet1.dip == jointSet2.dip &&
      jointSet1.jointSpacing == jointSet2.jointSpacing &&
      jointSet1.persistence == jointSet2.persistence &&
      jointSet1.stochasticFlag == jointSet2.stochasticFlag &&
      jointSet1.strikeStDev == jointSet2.strikeStDev &&
      jointSet1.dipStDev == jointSet2.dipStDev &&
      jointSet1.jointSpacingStDev == jointSet2.jointSpacingStDev &&
      jointSet1.persistenceStDev == jointSet2.persistenceStDev
    ) {
      true
    } else {
      false
    }
  }

  test("Input file without global origin specified should produce error") {
    val inputStr = "0.0 0.0 0.0 2.0 2.0 2.0\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 0.0\n" +
      "90.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "\n" +
      "60.0 25.0 0.5 100.0 30.0 0.0\n" +
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7 0.05\n" +
      "315.0 67.0 1.0 70.0 14.0 12 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file with improper global origin should produce error") {
    val inputStr = "0.0 0.0\n" +
      "0.0 0.0 0.0 2.0 2.0 2.0\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 0.0\n" +
      "90.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "\n" +
      "60.0 25.0 0.5 100.0 30.0 0.0\n" +
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7 0.05\n" +
      "315.0 67.0 1.0 70.0 14.0 12 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file with invalid double in definition of global origin should produce error") {
    val inputStr = "0.0 foo 0.0\n" +
      "0.0 0.0 0.0 2.0 2.0 2.0\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 0.0\n" +
      "90.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "\n" +
      "60.0 25.0 0.5 100.0 30.0 0.0\n" +
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7 0.05\n" +
      "315.0 67.0 1.0 70.0 14.0 12 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file without empty line for joint transition should produce error") {
    val inputStr = "1.0 1.0 1.0\n" +
      "0.0 0.0 0.0 2.0 2.0 2.0\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 0.0\n" +
      "90.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "60.0 25.0 0.5 100.0 30.0 0.0\n" +
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7 0.05\n" +
      "315.0 67.0 1.0 70.0 14.0 12 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file without bounding box should produce an error") {
    val inputStr = "1.0 1.0 1.0\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 0.0\n" +
      "90.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "60.0 25.0 0.5 100.0 30.0 0.0\n" +
      "\n" +
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7 0.05\n" +
      "315.0 67.0 1.0 70.0 14.0 12 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file with improper bounding box should produce an error") {
    val inputStr = "1.0 1.0 1.0\n" +
      "0.0 0.0 0.0 2.0 2.0\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 0.0\n" +
      "90.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "60.0 25.0 0.5 100.0 30.0 0.0\n" +
      "\n" +
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7 0.05\n" +
      "315.0 67.0 1.0 70.0 14.0 12 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file with invalid double in bounding box should produce an error") {
    val inputStr = "1.0 1.0 1.0\n" +
      "0.0 0.0 0.0 2.0 2.0 snafu\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 0.0\n" +
      "90.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "60.0 25.0 0.5 100.0 30.0 0.0\n" +
      "\n" +
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7 0.05\n" +
      "315.0 67.0 1.0 70.0 14.0 12 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file with improper rock volume face should produce error") {
    val inputStr = "1.0 1.0 1.0\n" +
      "0.0 0.0 0.0 2.0 2.0 2.0\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 0.0\n" +
      "90.0 90.0 0.0 0.0 30.0 0.0\n" + //invalid face
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "\n" +
      "60.0 25.0 0.5 100.0 30.0 0.0\n" +
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7 0.05\n" +
      "315.0 67.0 1.0 70.0 14.0 12 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file with invalid double in definition of rock volume face should produce error") {
    val inputStr = "1.0 1.0 1.0\n" +
      "0.0 0.0 0.0 2.0 2.0 2.0\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 bar\n" + //invalid face
      "90.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "\n" +
      "60.0 25.0 0.5 100.0 30.0 0.0\n" +
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7 0.05\n" +
      "315.0 67.0 1.0 70.0 14.0 12 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file with joint specified by too few values should produce error") {
    val inputStr = "1.0 1.0 1.0\n" +
      "0.0 0.0 0.0 2.0 2.0 2.0\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 0.0\n" +
      "90.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "\n" +
      "60.0 25.0 0.5 100.0 30.0\n" + //invalid joint
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7 0.05\n" +
      "315.0 67.0 1.0 70.0 14.0 12 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file with joint containing invalid optional parameters should produce error") {
    val inputStr = "1.0 1.0 1.0\n" +
      "0.0 0.0 0.0 2.0 2.0 2.0\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 0.0\n" +
      "90.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "\n" +
      "60.0 25.0 0.5 100.0 30.0 0.0\n" +
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7\n" + //invalid joint
      "315.0 67.0 1.0 70.0 14.0 12 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file with invalid double in definition of joint should produce error") {
    val inputStr = "1.0 1.0 1.0\n" +
      "0.0 0.0 0.0 2.0 2.0 2.0\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 0.0\n" +
      "90.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "\n" +
      "60.0 25.0 0.5 100.0 30.0 baz\n" + //invalid joint
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7 0.05\n" +
      "315.0 67.0 1.0 70.0 14.0 12 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Properly formatted input file should lead to expected rock volume and joints") {
    val inputStr = "1.0 1.0 1.0\n" +
      "0.0 0.0 0.0 2.0 2.0 2.0\n" +
      "0.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 90.0 0.0 2.0 0.0 30.0 0.0\n" +
      "90.0 90.0 0.0 0.0 0.0 30.0 0.0\n" +
      "90.0 90.0 2.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 0.0 30.0 0.0\n" +
      "0.0 0.0 0.0 0.0 2.0 30.0 0.0\n" +
      "\n" +
      "60.0 25.0 0.5 100.0 30.0 0.0\n" +
      "132.0 30.0 0.5 100.0 27.0 0.0 2.6 3.7 0.05\n" +
      "315.0 67.0 1.0 70.0 14.0 12.0 1.3 7.2 0.07 10.0"
    val inputSrc = Source.fromString(inputStr)
    val (globalOrigin, boundingBox, rockVolume, joints) = InputProcessor.readInput(inputSrc).get

    val expectedOrigin = Array(1.0, 1.0, 1.0)
    val expectedBoundingBox = Array(0.0, 0.0, 0.0, 2.0, 2.0, 2.0)

    val expectedFaces = Seq[Array[Double]](
      Array(0.0, 90.0, 0.0, 0.0, 0.0, 30.0, 0.0),
      Array(0.0, 90.0, 0.0, 2.0, 0.0, 30.0, 0.0),
      Array(90.0, 90.0, 0.0, 0.0, 0.0, 30.0, 0.0),
      Array(90.0, 90.0, 2.0, 0.0, 0.0, 30.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, 0.0, 30.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, 2.0, 30.0, 0.0)
    )

    val expectedJoints = Seq[JointSet](
      JointSet(Array(60.0, 25.0, 0.5, 100.0, 30.0, 0.0)),
      JointSet(Array(132.0, 30.0, 0.5, 100.0, 27.0, 0.0, 2.6, 3.7, 0.05)),
      JointSet(Array(315.0, 67.0, 1.0, 70.0, 14.0, 12.0, 1.3, 7.2, 0.07, 10.0))
    )
    val faceCheck = expectedFaces.zip(rockVolume) map { case (face1, face2) =>
        face1 sameElements face2
    }

    val jointCheck = expectedJoints.zip(joints) map { case (joint1, joint2) =>
        compareJointSets(joint1, joint2)
    }

    assert((globalOrigin sameElements expectedOrigin) && (boundingBox sameElements expectedBoundingBox) &&
      !faceCheck.contains(false) && !jointCheck.contains(false))
  }
}
