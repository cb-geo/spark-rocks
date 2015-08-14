package edu.berkeley.ce.rockslicing

import org.scalatest._

import scala.math.sqrt

class BlockVtkSpec extends FunSuite {
  val boundingFaces = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x = 0
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),  // x = 1
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y = 0
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),  // y = 1
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z = 0
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)   // z = 1
  )
  val unitCube = Block((0.0, 0.0, 0.0), boundingFaces)

  test("CCW orientation with center (0.0, 0.0, 0.0). A and B ... CONTINUE HERE") {
    val A = (1.0, 1.0, 0.0)
    val B = (1.0, -1.0, 0.0)
    val center = (0.0, 0.0, 0.0)
    assert(BlockVTK.ccwCompare(A, B, center))
  }
}
