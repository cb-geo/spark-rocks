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
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x = 0
    Face((1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0), // x = 2
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y = 0
    Face((0.0, 1.0, 0.0), 2.0, phi=0, cohesion=0), // y = 2
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z = 0
    Face((0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0) // z = 2
  )
  val twoCube = Block((0.0, 0.0, 0.0), boundingFaces2)

  val boundingFaces3 = List(
    Face((-1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), // -x = 1
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),  // x = 1
    Face((0.0, -1.0, 0.0), 1.0, phi=0, cohesion=0), // -y = 1
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),  // y = 1
    Face((0.0, 0.0, -1.0), 1.0, phi=0, cohesion=0), // -z = 1
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)   // z = 1
  )
  val twoCubeNonOrigin = Block((1.0, 1.0, 1.0), boundingFaces3)

  val boundingFaces6 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x = 0
    Face((1.0, 0.0, 0.0), 0.5, phi=0, cohesion=0), // x = 0.5
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y = 0
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0), // y = 1
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z = 0
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0) // z = 1
  )
  val rectPrism = Block((0.0, 0.0, 0.0), boundingFaces6)

  val jointBounds = Seq(
    ((1.0, 0.0, 0.0), 1.0),
    ((-1.0, 0.0, 0.0), 0.0),
    ((0.0, 1.0, 0.0), 1.0),
    ((0.0, -1.0, 0.0), 0.0)
  )

  val jointBounds2 = Seq(
    ((1.0, 0.0, 0.0), 1.0),
    ((-1.0, 0.0, 0.0), 1.0),
    ((0.0, 1.0, 0.0), 1.0),
    ((0.0, -1.0, 0.0), 1.0)
  )

  val jointBounds3 = Seq(
    ((1.0/sqrt(2.0), 1.0/sqrt(2.0), 0.0), 1.0),
    ((-1.0/sqrt(2.0), 1.0/sqrt(2.0), 0.0), 1.0),
    ((-1.0/sqrt(2.0), -1.0/sqrt(2.0), 0.0), 1.0),
    ((1.0/sqrt(2.0), -1.0/sqrt(2.0), 0.0), 1.0)
  )

  test("One seed joint should be selected") {
    val joint1 = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (0.333, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val joint2 = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (0.666, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val joint3 = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (0.48, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val boundingBox = (0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
    val (seedJoints, remainingJoints) = 
      LoadBalancer.generateSeedJoints(Seq(joint1, joint2, joint3), unitCube, 1, boundingBox, 0.0,
                                      Double.PositiveInfinity)
    assert(seedJoints.length == 1)
  }

  test("Two seed joints should be selected") {
    val joint1 = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (0.333, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val joint2 = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (0.666, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val boundingBox = (0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
    val (seedJoints, remainingJoints) = 
      LoadBalancer.generateSeedJoints(Seq(joint1, joint2), unitCube, 2, boundingBox, 0.0,
                                      Double.PositiveInfinity)
    assert(seedJoints.length == 2)
  }

  test("Two seed joints from same joint set should be selected") {
    val joint1 = Joint((1/math.sqrt(3), 1/math.sqrt(3), 1/math.sqrt(3)),
                       localOrigin = (0, 0, 0), center = (0.333, 0.333, 0.333),
                       phi = 30, cohesion = 0, shape = Nil)
    val joint2 = Joint((1/math.sqrt(3), 1/math.sqrt(3), 1/math.sqrt(3)),
                       localOrigin = (0, 0, 0), center = (0.666, 0.666, 0.666),
                       phi = 30, cohesion = 0, shape = Nil)
    val joint3 = Joint((-1/math.sqrt(3), -1/math.sqrt(3), -1/math.sqrt(3)),
                       localOrigin = (0, 0, 0), center = (0.2, 0.2, 0.2),
                       phi = 30, cohesion = 0, shape = Nil)
    val joint4 = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (0.333, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val joint5 = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (0.666, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val joints = Seq(joint5, joint4, joint3, joint2, joint1)
    val boundingBox = (0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
    val (seedJoints, remainingJoints) = 
      LoadBalancer.generateSeedJoints(joints, unitCube, 2, boundingBox, 0.0,
                                      Double.PositiveInfinity)
    assert(seedJoints == Seq(joint4, joint5))
  }

  test("One seed joint from first joint set should be selected") {
    val joint1 = Joint((1/math.sqrt(3), 1/math.sqrt(3), 1/math.sqrt(3)),
                       localOrigin = (0, 0, 0), center = (0.333, 0.333, 0.333),
                       phi = 30, cohesion = 0, shape = Nil)
    val joint2 = Joint((1/math.sqrt(3), 1/math.sqrt(3), 1/math.sqrt(3)),
                       localOrigin = (0, 0, 0), center = (0.666, 0.666, 0.666),
                       phi = 30, cohesion = 0, shape = Nil)
    val joint3 = Joint((-1/math.sqrt(3), -1/math.sqrt(3), -1/math.sqrt(3)),
                       localOrigin = (0, 0, 0), center = (0.5, 0.5, 0.5),
                       phi = 30, cohesion = 0, shape = Nil)
    val joint4 = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (0.333, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val joint5 = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (0.666, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val joints = Seq(joint5, joint4, joint3, joint2, joint1)
    val boundingBox = (0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
    val (seedJoints, remainingJoints) = 
      LoadBalancer.generateSeedJoints(joints, unitCube, 1, boundingBox, 0.0,
                                      Double.PositiveInfinity)
    assert(seedJoints == Seq(joint3))
  }
}
