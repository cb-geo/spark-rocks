import math.sqrt
import org.scalatest._
import edu.berkeley.ce.rockslicing.Joint

class JointSpec extends FunSuite {

  private val EPSILON = 1.0e-6

  // Sums the difference between all elements in a boundary list
  private def totalDifference(l1: Seq[((Double,Double,Double), Double)],
                              l2: Seq[((Double,Double,Double), Double)]): Double = {
    val differences = l1.zip(l2) map { case (((a,b,c),d), ((x,y,z),w)) =>
      math.abs(x-a) + math.abs(y-b) + math.abs(z-c) + math.abs(w-d)
    }
    differences.sum
  }

  val boundaries = List(((1.0, 0.0, 0.0), 1.0),  // x = 1
                    ((-1.0, 0.0, 0.0), 0.0),     // -x = 0
                    ((0.0, 1.0, 0.0), 1.0),      // y = 1
                    ((0.0, -1.0, 0.0), 0.0))     // -y = 0

  test("Dip direction of 0 centered at origin") {
    val joint = Joint((0.0, 0.0, 1.0), localOrigin=(0.0,0.0,-1.0), center=(0.0,0.0,0.0), dipAngle=0,
                      dipDirection=0, phi=0, cohesion=0, shape=boundaries)
    val newBoundaries: List[((Double,Double,Double), Double)] =
        List(((0.0, -1.0, 0.0), 1.0),
             ((0.0, 1.0, 0.0), 0.0),
             ((1.0, 0.0, 0.0), 1.0),
             ((-1.0, 0.0, 0.0), 0.0))
    assert(totalDifference(joint.globalCoordinates, newBoundaries) <= EPSILON)
  }

  test("Dip direction of 0 centered at (1.0, 2.0, 3.0)") {
    val joint = Joint((0.0, 0.0, 1.0), localOrigin=(0.0,0.0,2.0), center=(1.0,2.0,3.0), dipAngle=0,
                      dipDirection=0, phi=0, cohesion=0, shape=boundaries)
    val newBoundaries: List[((Double,Double,Double), Double)] =
        List(((0.0, -1.0, 0.0), -1.0),
             ((0.0, 1.0, 0.0), 2.0),
             ((1.0, 0.0, 0.0), 2.0),
             ((-1.0, 0.0, 0.0), -1.0))
    assert(totalDifference(joint.globalCoordinates, newBoundaries) <= EPSILON)
  }

  test("Dip direction of pi/4 centered at origin") {
    val joint = Joint((0.0, 0.0, 1.0), localOrigin=(0.0,0.0,-1.0), center=(0.0, 0.0, 0.0), dipAngle=0,
                      dipDirection=math.Pi/4.0, phi=0, cohesion=0, shape=boundaries)
    val newBoundaries: List[((Double,Double,Double), Double)] =
      List(((sqrt(2)/2.0, -sqrt(2)/2.0, 0.0), 1.0),
           ((-sqrt(2)/2.0, sqrt(2)/2.0, 0.0), 0.0),
           ((sqrt(2)/2.0, sqrt(2)/2.0, 0.0), 1.0),
           ((-sqrt(2)/2.0, -sqrt(2)/2.0, 0.0), 0.0))
    assert(totalDifference(joint.globalCoordinates, newBoundaries) <= EPSILON)
  }

  test("Dip direction of pi/4 centered at (1.0, 2.0, 3.0)") {
    val joint = Joint((0.0, 0.0, 1.0), localOrigin=(0.0,0.0,2.0), center=(1.0, 2.0, 3.0), dipAngle=0,
                      dipDirection=math.Pi/4.0, phi=0, cohesion=0, shape=boundaries)
    val newBoundaries: List[((Double,Double,Double), Double)] =
      List(((sqrt(2)/2.0, -sqrt(2)/2.0, 0.0), 1 - sqrt(2)/2.0),
           ((-sqrt(2)/2.0, sqrt(2)/2.0, 0.0), sqrt(2)/2.0),
           ((sqrt(2)/2.0, sqrt(2)/2.0, 0.0), 1 + 3*sqrt(2)/2.0),
           ((-sqrt(2)/2.0, -sqrt(2)/2.0, 0.0), -3*sqrt(2)/2.0))
    assert(totalDifference(joint.globalCoordinates, newBoundaries) <= EPSILON)
  }

  test("The joint should have distance 1/2.0") {
    val joint = Joint((0.0, 0.0, 1.0), localOrigin=(0.0,0.0,0.0), center=(0.0, 0.0, 1/2.0), dipAngle=0,
      dipDirection=0, phi=0, cohesion=0, shape=Nil)
    assert(joint.d == 1/2.0)
  }

  test("The joint with non-zero origin should have distance 1/2.0") {
    val joint = Joint((0.0, 0.0, 1.0), localOrigin=(0.0,0.0,1/2.0), center=(0.0, 0.0, 1.0), dipAngle=0,
      dipDirection=0, phi=0, cohesion=0, shape=Nil)
    assert(joint.d == 1/2.0)
  }

  test("The plane x/sqrt(2.0) - z/sqrt(2.0) = 1 at center (0.0,0.0,1.0) should have distance 1.0 from local origin") {
    val joint = Joint((1.0/sqrt(2.0), 0.0, -1.0/sqrt(2.0)), localOrigin=(-1.0/sqrt(2.0),0.0,1.0 + 1.0/sqrt(2.0)), center=(0.0, 0.0, 1.0), dipAngle=0,
      dipDirection=0, phi=0, cohesion=0, shape=Nil)
    var distance = joint.d
    if (math.abs(joint.d - 1.0) < EPSILON) {
      distance = 1.0
    }
    assert(distance == 1.0)
  }

  test("Joint should have updated distance 1.0 from new local origin") {
    val joint = Joint((1.0, 0.0, 0.0), localOrigin=(0.0,0.0,0.0), center=(0.0, 0.0, 0.0), dipAngle=0,
                      dipDirection=math.Pi/4.0, phi=0, cohesion=0, shape=boundaries)
    val newJoint = joint.updateJoint(-1.0, 0.0, 0.0)
    val expectedDistance = 1.0
    assert(newJoint.d == expectedDistance)
  }

  test("Joint should have updated distance -8.0 from new local origin") {
    val joint = Joint((1.0, 0.0, 0.0), localOrigin=(-1.0,0.0,0.0), center=(0.0, 0.0, 0.0), dipAngle=0,
                      dipDirection=math.Pi/4.0, phi=0, cohesion=0, shape=boundaries)
    val newJoint = joint.updateJoint(8.0, 0.0, 0.0)
    val expectedDistance = -8.0
    assert(newJoint.d == expectedDistance)
  }

  test("Joint should have updated distance -5.0 from new local origin") {
    val joint = Joint((0.0, 0.0, 1.0), localOrigin=(0.0,0.0,1.0), center=(1.0, 2.0, 3.0), dipAngle=0,
      dipDirection=math.Pi/4.0, phi=0, cohesion=0, shape=boundaries)
    val newJoint = joint.updateJoint(8.0, 0.0, 8.0)
    val expectedDistance = -5.0
    assert(newJoint.d == expectedDistance)
  }
}
