package edu.berkeley.ce.rockslicing

import org.scalatest._

class LoadBalancerSpec extends FunSuite {
  val boundingFaces = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x = 0
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),  // x = 1
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y = 0
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),  // y = 1
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z = 0
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)   // z = 1
  )
  val unitCube = Block((0.0, 0.0, 0.0), boundingFaces)

  val boundingFaces2 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), 
    Face((1.0, 0.0, 0.0), 0.666, phi=0, cohesion=0),
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), 
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), 
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0) 
  )
  val twothirdsCube = Block((0.0, 0.0, 0.0), boundingFaces2)

  val boundingFaces3 = List(
    Face((-1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face((0.0, -1.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face((0.0, 0.0, -1.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0) 
  )
  val twoCube = Block((0.0, 0.0, 0.0), boundingFaces3)

  test("Twenty seed joints should be generated for unit cube") {
    val numProcessors = 21
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
