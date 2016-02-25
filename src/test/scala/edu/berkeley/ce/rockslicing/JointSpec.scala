package edu.berkeley.ce.rockslicing

import org.scalatest._
import scala.math.sqrt

class JointSpec extends FunSuite {
  // Sums the difference between all elements in a boundary list
  private def totalDifference(l1: Seq[(Array[Double], Double)],
                              l2: Seq[(Array[Double], Double)]): Double = {
    val differences = l1.zip(l2) map { case ((norm1, d1), (norm2, d2)) =>
      math.abs(norm1(0) - norm2(0)) + math.abs(norm1(1) - norm2(1)) + math.abs(norm1(2)) - math.abs(norm2(2))
    }
    differences.sum
  }

  val boundaries = Vector((Array(1.0, 0.0, 0.0), 1.0),  // x = 1
                    (Array(-1.0, 0.0, 0.0), 0.0),     // -x = 0
                    (Array(0.0, 1.0, 0.0), 1.0),      // y = 1
                    (Array(0.0, -1.0, 0.0), 0.0))     // -y = 0

  val boundaries2 = Vector((Array(1.0, 0.0, 0.0), 1.0),  // x = 1
                    (Array(-1.0, 0.0, 0.0), 1.0),     // -x = 1
                    (Array(0.0, 1.0, 0.0), 1.0),      // y = 1
                    (Array(0.0, -1.0, 0.0), 1.0))     // -y = 1

  test("Dip direction for horizontal joint should be pi/2.0 exactly") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,-1.0), center=Array(0.0,0.0,0.0),
                      phi=0, cohesion=0, shape=Vector.empty)
    assert(joint.dipDirection == math.Pi/2.0)
  }

  test("Dip direction for vertical joint should be pi/2.0") {
    val joint = Joint(Array(0.0, -1.0, 0.0), localOrigin=Array(0.0,0.0,-1.0), center=Array(0.0,0.0,0.0),
                      phi=0, cohesion=0, shape=Vector.empty)
    assert(math.abs(joint.dipDirection - math.Pi/2.0) < NumericUtils.EPSILON)
  }

  test("Dip direction for vertical joint should be pi") {
    val joint = Joint(Array(-1.0, 0.0, 0.0), localOrigin=Array(0.0,0.0,-1.0), center=Array(0.0,0.0,0.0),
      phi=0, cohesion=0, shape=Vector.empty)
    assert(math.abs(joint.dipDirection - math.Pi) < NumericUtils.EPSILON)
  }

  test("Dip direction for vertical joint should be 3.0*pi/2.0") {
    val joint = Joint(Array(0.0, 1.0, 0.0), localOrigin=Array(0.0,0.0,-1.0), center=Array(0.0,0.0,0.0),
      phi=0, cohesion=0, shape=Vector.empty)
    assert(math.abs(joint.dipDirection - 3.0*math.Pi/2.0) < NumericUtils.EPSILON)
  }

  test("Dip direction for non-vertical joint should be pi/2.0") {
    val joint = Joint(Array(0.0,-1.0/sqrt(2.0), 1.0/sqrt(2.0)), localOrigin=Array(0.0,0.0,-1.0), center=Array(0.0,0.0,0.0),
      phi=0, cohesion=0, shape=Vector.empty)
    assert(math.abs(joint.dipDirection - math.Pi/2.0) < NumericUtils.EPSILON)
  }

  test("Dip direction for non-vertical joint should be 3.0*pi/4.0") {
    val joint = Joint(Array(-1.0/sqrt(3.0),-1.0/sqrt(3.0), 1.0/sqrt(3.0)),
                      localOrigin=Array(0.0,0.0,-1.0), center=Array(0.0,0.0,0.0),
      phi=0, cohesion=0, shape=Vector.empty)
    assert(math.abs(joint.dipDirection - 3.0*math.Pi/4.0) < NumericUtils.EPSILON)
  }

  test("Dip direction for non-vertical joint should be 7.0*pi/4.0") {
    val joint = Joint(Array(1.0/sqrt(3.0), 1.0/sqrt(3.0), 1.0/sqrt(3.0)), localOrigin=Array(0.0,0.0,-1.0),
                      center=Array(0.0,0.0,0.0),
      phi=0, cohesion=0, shape=Vector.empty)
    assert(math.abs(joint.dipDirection - 7.0*math.Pi/4.0) < NumericUtils.EPSILON)
  }

  test("Dip angle for horizontal joint should be 0.0 exactly") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,-1.0), center=Array(0.0,0.0,0.0),
      phi=0, cohesion=0, shape=Vector.empty)
    assert(joint.dipAngle == 0.0)
  }

  test("Dip angle for vertical joint should be pi/2.0 exactly") {
    val joint = Joint(Array(1.0, 0.0, 0.0), localOrigin=Array(0.0,0.0,-1.0), center=Array(0.0,0.0,0.0),
      phi=0, cohesion=0, shape=Vector.empty)
    assert(joint.dipAngle == math.Pi/2.0)
  }

  test("Dip angle for joint with positive z-component in normal should be pi/6.0") {
    val joint = Joint(Array(1.0/sqrt(1.0 + sqrt(3.0)), 0.0, sqrt(3.0)/sqrt(1.0 + sqrt(3.0))),
                      localOrigin=Array(0.0,0.0,-1.0), center=Array(0.0,0.0,0.0),
      phi=0, cohesion=0, shape=Vector.empty)
    assert(math.abs(joint.dipAngle - math.Pi/6.0) < NumericUtils.EPSILON)
  }

  test("Dip angle for joint with negative z-component in normal should be pi/6.0") {
    val joint = Joint(Array(0.0, 1.0/sqrt(1.0 + sqrt(3.0)), -sqrt(3.0)/sqrt(1.0 + sqrt(3.0))),
                       localOrigin=Array(0.0,0.0,-1.0), center=Array(0.0,0.0,0.0),
      phi=0, cohesion=0, shape=Vector.empty)
    assert(math.abs(joint.dipAngle - math.Pi/6.0) < NumericUtils.EPSILON)
  }

  test("Local-to-global coordinates check: Joint with dip direction of pi/2.0 centered " +
       "at origin with positive z component in normal") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0,0.0,0.0),
                       phi=0, cohesion=0, shape=boundaries)
    val newBoundaries = Vector(
      (Array(1.0, 0.0, 0.0), 1.0),
      (Array(-1.0, 0.0, 0.0), 0.0),
      (Array(0.0, -1.0, 0.0), 1.0),
      (Array(0.0, 1.0, 0.0), 0.0)
    )
    assert(totalDifference(joint.globalCoordinates, newBoundaries) < NumericUtils.EPSILON)
  }

  test("Local-to-global coordinates check: Joint with dip direction of pi/2.0 centered " +
       "at origin with negative z component in normal") {
    val joint = Joint(Array(0.0, 0.0, -1.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0,0.0,0.0),
                      phi=0, cohesion=0, shape=boundaries)
    val newBoundaries = Vector(
      (Array(1.0, 0.0, 0.0), 1.0),
      (Array(-1.0, 0.0, 0.0), 0.0),
      (Array(0.0, -1.0, 0.0), 1.0),
      (Array(0.0, 1.0, 0.0), 0.0)
    )
    assert(totalDifference(joint.globalCoordinates, newBoundaries) < NumericUtils.EPSILON)
  }

  test("Local-to-global coordinates check: Joint with dip direction of pi/2.0 centered at Array(1.0, 2.0, 3.0)") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,0.0), center=Array(1.0,2.0,3.0),
                       phi=0, cohesion=0, shape=boundaries)
    val newBoundaries = Vector(
      (Array(1.0, 0.0, 0.0), 2.0),
      (Array(-1.0, 0.0, 0.0), -1.0),
      (Array(0.0, -1.0, 0.0), -1.0),
      (Array(0.0, 1.0, 0.0), 2.0))
    assert(totalDifference(joint.globalCoordinates, newBoundaries) < NumericUtils.EPSILON)
  }

  test("Local-to-global coordinates check: Joint with dip direction of 0.0 centered at origin") {
    val joint = Joint(Array(1.0, 0.0, 0.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0, 0.0, 0.0),
                      phi=0, cohesion=0, shape=boundaries)
    val newBoundaries = Vector(
      (Array(0.0, 1.0, 0.0), 1.0),
      (Array(0.0, -1.0, 0.0), 0.0),
      (Array(0.0, 0.0, -1.0), 1.0),
      (Array(0.0, 0.0, 1.0), 0.0)
    )
    assert(totalDifference(joint.globalCoordinates, newBoundaries) < NumericUtils.EPSILON)
  }

  test("Local-to-global coordinates check: Joint with dip direction of 3*pi/2 centered at origin") {
    val joint = Joint(Array(0.0, 1.0, 0.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0, 0.0, 0.0),
                      phi=0, cohesion=0, shape=boundaries)
    val newBoundaries = Vector(
      (Array(-1.0, 0.0, 0.0), 1.0),
      (Array(1.0, 0.0, 0.0), 0.0),
      (Array(0.0, 0.0, -1.0), 1.0),
      (Array(0.0, 0.0, 1.0), 0.0)
    )
    assert(totalDifference(joint.globalCoordinates, newBoundaries) < NumericUtils.EPSILON)
  }

  test("Local-to-global coordinates check: Joint with dip direction of pi/4 centered at origin") {
    val joint = Joint(Array(1.0/sqrt(2.0), -1.0/sqrt(2.0), 0.0), localOrigin=Array(0.0,0.0,0.0),
                      center=Array(0.0, 0.0, 0.0), phi=0, cohesion=0, shape=boundaries)
    val newBoundaries = Vector(
      (Array(1.0/sqrt(2.0), 1.0/sqrt(2.0), 0.0), 1.0),
      (Array(-1.0/sqrt(2.0), -1.0/sqrt(2.0), 0.0), 0.0),
      (Array(0.0, 0.0, -1.0), 1.0),
      (Array(0.0, 0.0, 1.0), 0.0)
    )
    assert(totalDifference(joint.globalCoordinates, newBoundaries) < NumericUtils.EPSILON)
  }

  test("Local-to-global coordinates check: Joint with dip direction of pi/4 centered at Array(1.0, 2.0, 3.0") {
    val joint = Joint(Array(1.0/sqrt(2.0), -1.0/sqrt(2.0), 0.0), localOrigin=Array(0.0,0.0,0.0),
      center=Array(1.0, 2.0, 3.0), phi=0, cohesion=0, shape=boundaries)
    val newBoundaries = Vector(
      (Array(1.0/sqrt(2.0), 1.0/sqrt(2.0), 0.0), 1.0 + sqrt(1.5*1.5 + 1.5*1.5)),
      (Array(-1.0/sqrt(2.0), -1.0/sqrt(2.0), 0.0), -sqrt(1.5*1.5 + 1.5*1.5)),
      (Array(0.0, 0.0, -1.0), -2.0),
      (Array(0.0, 0.0, 1.0), 3.0)
    )
    assert(totalDifference(joint.globalCoordinates, newBoundaries) < NumericUtils.EPSILON)
  }

  test("Local-to-global coordinates check: Joint with dip direction of pi/2 and " +
       "dip angle of pi/6 centered at origin") {
    val joint = Joint(Array(0.0, -math.sin(math.Pi/6.0), math.cos(math.Pi/6.0)), localOrigin=Array(0.0,0.0,0.0),
                      center=Array(0.0, 0.0, 0.0), phi=0, cohesion=0, shape=boundaries)
    val newBoundaries = Vector(
      (Array(1.0, 0.0, 0.0), 1.0),
      (Array(-1.0, 0.0, 0.0), 0.0),
      (Array(0.0, -math.cos(math.Pi/6.0), -math.sin(math.Pi/6.0)), 1.0),
      (Array(0.0, math.cos(math.Pi/6.0), math.sin(math.Pi/6.0)), 0.0))
    assert(totalDifference(joint.globalCoordinates, newBoundaries) < NumericUtils.EPSILON)
  }

  test("The joint should have distance 1/2.0") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0, 0.0, 1/2.0),
                      phi=0, cohesion=0, shape=Vector.empty)
    assert(math.abs(joint.d - 1/2.0) < NumericUtils.EPSILON)
  }

  test("The joint with non-zero origin should have distance 1/2.0") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,1/2.0), center=Array(0.0, 0.0, 1.0),
       phi=0, cohesion=0, shape=Vector.empty)
    assert(math.abs(joint.d - 1/2.0) < NumericUtils.EPSILON)
  }

  test("The plane x/sqrt(2.0) - z/sqrt(2.0) = 1 at center Array(0.0,0.0,1.0) should have distance 1.0 from local origin") {
    val joint = Joint(Array(1.0/sqrt(2.0), 0.0, -1.0/sqrt(2.0)),
                      localOrigin=Array(-1.0/sqrt(2.0),0.0,1.0 + 1.0/sqrt(2.0)), center=Array(0.0, 0.0, 1.0),
       phi=0, cohesion=0, shape=Vector.empty)
    assert(math.abs(joint.d - 1.0) < NumericUtils.EPSILON)
  }

  test("Joint should have updated distance 1.0 from new local origin") {
    val joint = Joint(Array(1.0, 0.0, 0.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0, 0.0, 0.0),
                      phi=0, cohesion=0, shape=boundaries)
    val newJoint = joint.updateJoint(Array(-1.0, 0.0, 0.0))
    assert(math.abs(newJoint.d - 1.0) < NumericUtils.EPSILON)
  }

  test("Joint should have updated distance -8.0 from new local origin") {
    val joint = Joint(Array(1.0, 0.0, 0.0), localOrigin=Array(-1.0,0.0,0.0), center=Array(0.0, 0.0, 0.0),
                      phi=0, cohesion=0, shape=boundaries)
    val newJoint = joint.updateJoint(Array(8.0, 0.0, 0.0))
    assert(math.abs(newJoint.d + 8.0) < NumericUtils.EPSILON)
  }

  test("Joint should have updated distance -5.0 from new local origin") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,1.0), center=Array(1.0, 2.0, 3.0),
                      phi=0, cohesion=0, shape=boundaries)
    val newJoint = joint.updateJoint(Array(8.0, 0.0, 8.0))
    assert(math.abs(newJoint.d + 5.0) < NumericUtils.EPSILON)
  }

  test("Oblique joint should have updated distance of 1/sqrt(2.0)") {
    val joint = Joint(Array(1.0/sqrt(2.0), 1.0/sqrt(2.0), 0.0), localOrigin = Array(0.0, 0.0, 0.0),
                      center = Array(1.0, 0.0, 0.0), phi = 0.0, cohesion = 0.0, shape = Vector.empty)
    val newJoint = joint.updateJoint(Array(0.0, 0.0, 0.0))
    assert(math.abs(newJoint.d - 1.0/sqrt(2.0)) < NumericUtils.EPSILON)
  }

  test("Oblique joint should have updated distance of 0.0") {
    val joint = Joint(Array(-1.0/sqrt(2.0), 1.0/sqrt(2.0), 0.0), localOrigin = Array(0.0, 0.0, 0.0),
                      center = Array(1.0, 0.0, 0.0), phi = 0.0, cohesion = 0.0, shape = Vector.empty)
    val newJoint = joint.updateJoint(Array(1.0, 0.0, 0.0))
    assert(math.abs(newJoint.d) < NumericUtils.EPSILON)
  }

  test("Joint bounding sphere should have origin Array(0.5, -0.5, 0.0) and radius sqrt(0.5^2 + 0.5^2)") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0,0.0,0.0),
                       phi=0, cohesion=0, shape=boundaries)
    val expectedBoundingSphere = (Array(0.5, -0.5, 0.0), math.sqrt(math.pow(0.5, 2) + math.pow(0.5, 2)))
    val jointBS = joint.boundingSphere.get
    val cleanedBS = (Array(NumericUtils.applyTolerance(jointBS._1(0)),
                      NumericUtils.applyTolerance(jointBS._1(1)),
                      NumericUtils.applyTolerance(jointBS._1(2))),
                     jointBS._2)
    assert((expectedBoundingSphere._1 sameElements cleanedBS._1) && expectedBoundingSphere._2 == cleanedBS._2)
  }

  test("Joint bounding sphere should have origin Array(0.0, 0.0, 0.0) and radius sqrt(2)") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0,0.0,0.0),
                       phi=0, cohesion=0, shape=boundaries2)
    val expectedBoundingSphere = (Array(0.0, 0.0, 0.0), math.sqrt(2))
    val jointBS = joint.boundingSphere.get
    val cleanedBS = (Array(NumericUtils.applyTolerance(jointBS._1(0)),
                      NumericUtils.applyTolerance(jointBS._1(1)),
                      NumericUtils.applyTolerance(jointBS._1(2))),
                     jointBS._2)
    assert((expectedBoundingSphere._1 sameElements cleanedBS._1) && expectedBoundingSphere._2 == cleanedBS._2)
  }

  test("Joint bounding sphere should have origin Array(0.5, 0.0, -0.5) and radius sqrt(0.5^2 + 0.5^2)") {
    val joint = Joint(Array(0.0, -1.0, 0.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0,0.0,0.0),
                       phi=0, cohesion=0, shape=boundaries)
    val expectedBoundingSphere = (Array(0.5, 0.0, -0.5), math.sqrt(math.pow(0.5, 2) + math.pow(0.5, 2)))
    val jointBS = joint.boundingSphere.get
    val cleanedBS = (Array(NumericUtils.applyTolerance(jointBS._1(0)),
                      NumericUtils.applyTolerance(jointBS._1(1)),
                      NumericUtils.applyTolerance(jointBS._1(2))),
                     jointBS._2)
    assert((expectedBoundingSphere._1 sameElements cleanedBS._1) && expectedBoundingSphere._2 == cleanedBS._2)
  }

  test("Joint bounding sphere should have origin Array(1.5, 5.0, -0.5) and radius sqrt(0.5^2 + 0.5^2)") {
    val joint = Joint(Array(0.0, -1.0, 0.0), localOrigin=Array(0.0,0.0,0.0), center=Array(1.0,5.0,0.0),
                       phi=0, cohesion=0, shape=boundaries)
    val expectedBoundingSphere = (Array(1.5, 5.0, -0.5), math.sqrt(math.pow(0.5, 2) + math.pow(0.5, 2)))
    val jointBS = joint.boundingSphere.get
    val cleanedBS = (Array(NumericUtils.applyTolerance(jointBS._1(0)),
                      NumericUtils.applyTolerance(jointBS._1(1)),
                      NumericUtils.applyTolerance(jointBS._1(2))),
                     jointBS._2)
    assert((expectedBoundingSphere._1 sameElements cleanedBS._1) && expectedBoundingSphere._2 == cleanedBS._2)
  }
}
