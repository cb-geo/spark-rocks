package edu.berkeley.ce.rockslicing

import org.scalatest._

class LoadBalancerSpec extends FunSuite {
  val boundingFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x = 0
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),  // x = 1
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y = 0
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),  // y = 1
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z = 0
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)   // z = 1
  )
  val unitCube = Block(Array(0.0, 0.0, 0.0), boundingFaces)

  val boundingFaces2 = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 0.666, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
  )
  val twothirdsCube = Block(Array(0.0, 0.0, 0.0), boundingFaces2)

  val boundingFaces3 = List(
    Face(Array(-1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, -1.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
  )
  val twoCube = Block(Array(0.0, 0.0, 0.0), boundingFaces3)

  test("18 seed joints should be generated for unit cube") {
    val numProcessors = 19
    val seedJoints =
      LoadBalancer.generateProcessorJoints(unitCube, numProcessors)
    assert(seedJoints.length + 1 == numProcessors)
  }

  test("Three seed joints should be generated for two thirds cube") {
    val numProcessors = 4
    val seedJoints =
      LoadBalancer.generateProcessorJoints(twothirdsCube, numProcessors)
    assert(seedJoints.length + 1 == numProcessors)
  }

  test("Five seed joints should be generated for two cube") {
    val numProcessors = 6
    val seedJoints =
      LoadBalancer.generateProcessorJoints(twoCube, numProcessors)
    assert(seedJoints.length + 1 == numProcessors)
  }
}
