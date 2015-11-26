package edu.berkeley.ce.rockslicing

import org.scalatest.FunSuite

import scala.io.Source

class InputProcessingSpec extends FunSuite {

  test("Input file without \"%\" should produce error") {
    val inputStr = "-1.0 0.0 0.0 0.0 0.0 0.0\n" +
                   "1.0 0.0 0.0 2.0 0.0 0.0\n" +
                   "0.0 -1.0 0.0 0.0 0.0 0.0\n" +
                   "0.0 1.0 0.0 2.0 0.0 0.0\n" +
                   "0.0 0.0 -1.0 0.0 0.0 0.0\n" +
                   "0.0 0.0 1.0 2.0 0.0 0.0\n" +
                   "1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0\n"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file with rock volume face should produce error") {
    val inputStr = "-1.0 0.0 0.0 0.0 0.0 0.0\n" +
      "1.0 0.0 0.0 2.0 0.0\n" + // Invalid face
      "0.0 -1.0 0.0 0.0 0.0 0.0\n" +
      "0.0 1.0 0.0 2.0 0.0 0.0\n" +
      "0.0 0.0 -1.0 0.0 0.0 0.0\n" +
      "0.0 0.0 1.0 2.0 0.0 0.0\n" +
      "%\n" +
      "1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0\n"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file with joint specified by too few values should produce error") {
    val inputStr = "-1.0 0.0 0.0 0.0 0.0 0.0\n" +
      "1.0 0.0 0.0 2.0 0.0 0.0\n" +
      "0.0 -1.0 0.0 0.0 0.0 0.0\n" +
      "0.0 1.0 0.0 2.0 0.0 0.0\n" +
      "0.0 0.0 -1.0 0.0 0.0 0.0\n" +
      "0.0 0.0 1.0 2.0 0.0 0.0\n" +
      "%\n" +
      "1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0\n" // Invalid joint
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Input file with joint containing invalid optional face should produce error") {
    val inputStr = "-1.0 0.0 0.0 0.0 0.0 0.0\n" +
      "1.0 0.0 0.0 2.0 0.0 0.0\n" +
      "0.0 -1.0 0.0 0.0 0.0 0.0\n" +
      "0.0 1.0 0.0 2.0 0.0 0.0\n" +
      "0.0 0.0 -1.0 0.0 0.0 0.0\n" +
      "0.0 0.0 1.0 2.0 0.0 0.0\n" +
      "%\n" +
      "1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0\n" +
      // Invalid joint
      "0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 -1.0 0.0 0.5 -1.0 0.0 0.0 0.0 1.0 0.0 0.0"
    val inputSrc = Source.fromString(inputStr)
    assert(InputProcessor.readInput(inputSrc).isEmpty)
  }

  test("Properly formatted input file should lead to expected rock volume and joints") {
    val inputStr = "-1.0 0.0 0.0 0.0 0.0 0.0\n" +
      "1.0 0.0 0.0 2.0 0.0 0.0\n" +
      "0.0 -1.0 0.0 0.0 0.0 0.0\n" +
      "0.0 1.0 0.0 2.0 0.0 0.0\n" +
      "0.0 0.0 -1.0 0.0 0.0 0.0\n" +
      "0.0 0.0 1.0 2.0 0.0 0.0\n" +
      "%\n" +
      "1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0\n" +
      "0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 -1.0 0.0 0.5 -1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.5"
    val inputSrc = Source.fromString(inputStr)
    val (rockVolume, joints) = InputProcessor.readInput(inputSrc).get

    val expectedFaces = List[Face](
      Face((-1.0, 0.0, 0.0), 0.0, 0.0, 0.0),
      Face((1.0, 0.0, 0.0), 2.0, 0.0, 0.0),
      Face((0.0, -1.0, 0.0), 0.0, 0.0, 0.0),
      Face((0.0, 1.0, 0.0), 2.0, 0.0, 0.0),
      Face((0.0, 0.0, -1.0), 0.0, 0.0, 0.0),
      Face((0.0, 0.0, 1.0), 2.0, 0.0, 0.0)
    )

    val expectedJoints = List[Joint](
      Joint((1.0, 0.0, 0.0), (0.0, 0.0, 0.0), (1.0, 0.0, 0.0), 0.0, 0.0, Nil),
      Joint((0.0, 0.0, 1.0), (0.0, 0.0, 0.0), (0.0, 0.0, 1.0), 0.0, 0.0, shape = List(
        ((0.0, 1.0, 0.0), 0.0), ((0.0, -1.0, 0.0), 0.5), ((-1.0, 0.0, 0.0), 0.0), ((1.0, 0.0, 0.0), 0.5)))
      )
    assert(rockVolume == expectedFaces && joints == expectedJoints)
  }
}