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

  test("The dot product of the joint normal and vector should be 1") {
    val joint = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (1, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val vector = DenseVector[Double](1.0, 0.0, 0.0)
    println("This is the dot product "+LoadBalancer.jointDotProduct(joint, vector))
    assert(LoadBalancer.jointDotProduct(joint, vector) - 1.0 <= NumericUtils.EPSILON)
  }

  test("The dot product of the joint normal and vector should be 0.0") {
    val joint = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (1, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val vector = DenseVector[Double](0.0, 1.0, 0.0)
    println("This is the dot product "+LoadBalancer.jointDotProduct(joint, vector))
    assert(LoadBalancer.jointDotProduct(joint, vector) - 1.0 <= NumericUtils.EPSILON)
  }

  test("The dot product of the joint normal and vector should be 1/sqrt(2)") {
    val joint = Joint((1/sqrt(2), 1/sqrt(2), 0), localOrigin = (0, 0, 0), center = (1, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val vector = DenseVector[Double](0.0, 1.0, 0.0)
    println("This is the dot product "+LoadBalancer.jointDotProduct(joint, vector))
    assert(LoadBalancer.jointDotProduct(joint, vector) - 1/sqrt(2) <= NumericUtils.EPSILON)
  }

  test("The dot product of the joint normal and vector should be 1/sqrt(2) - joint to vector") {
    val joint = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (1, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val vector = DenseVector[Double](1/sqrt(2), 1/sqrt(2), 0.0)
    println("This is the dot product "+LoadBalancer.jointDotProduct(joint, vector))
    assert(LoadBalancer.jointDotProduct(joint, vector) - 1/sqrt(2) <= NumericUtils.EPSILON)
  }

  test("One seed joint should be selected") {
    val joint1 = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (0.333, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val joint2 = Joint((1, 0, 0), localOrigin = (0, 0, 0), center = (0.666, 0, 0),
                      phi = 30, cohesion = 0, shape = Nil)
    val boundingBox = (0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
    val (seedJoints, remainingJoints) = 
      LoadBalancer.generateSeedJoints(Seq(joint1, joint2), unitCube, 1, boundingBox, 0.0,
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
}
