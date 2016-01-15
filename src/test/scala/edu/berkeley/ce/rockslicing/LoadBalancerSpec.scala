package edu.berkeley.ce.rockslicing

import org.scalatest._
import scala.math.sqrt
import breeze.linalg.{DenseVector, DenseMatrix}

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

  private def tupleDifference(c1: (Double, Double, Double), c2: (Double, Double, Double)): Double =
    c1 match {
      case (a, b, c) => c2 match {
        case (x, y, z) => math.abs(x - a) + math.abs(y - b) + math.abs(z - c)
      }
    }

  test("Twenty seed joints should be generated for unit cube") {
    val numSeeds = 20
    val seedJoints = 
      LoadBalancer.generateSeedJoints(unitCube, numSeeds)
    assert(seedJoints.length == numSeeds)
  }

  test("Three seed joints should be generated for two thirds cube") {
    val numSeeds = 3
    val seedJoints = 
      LoadBalancer.generateSeedJoints(twothirdsCube, numSeeds)
    assert(seedJoints.length == numSeeds)
  }

  test("Five seed joints should be generated for two cube") {
    val numSeeds = 5
    val seedJoints = 
      LoadBalancer.generateSeedJoints(twoCube, numSeeds)
    assert(seedJoints.length == numSeeds)
  }
}
