import org.scalatest._
import scala.collection.mutable.ListBuffer
import edu.berkeley.ce.rockslicing.{Face, Block, Joint, Delaunay, inputProcessor}

class InputSpec extends FunSuite {
  test("The x, y and z averages should be 2.0") {
    val joint1 = Joint((1.0, 0.0, 0.0), 1.0, center=(1.0, 1.0, 3.0), dipAngle=0,
                      dipDirection=0, phi=0, cohesion=0, shape=Nil)
    val joint2 = Joint((1.0, 0.0, 0.0), 1.0, center=(2.0, 3.0, 2.0), dipAngle=0,
                      dipDirection=0, phi=0, cohesion=0, shape=Nil)
    val joint3 = Joint((1.0, 0.0, 0.0), 1.0, center=(3.0, 2.0, 1.0), dipAngle=0,
                      dipDirection=0, phi=0, cohesion=0, shape=Nil)
    val joints = ListBuffer(joint1, joint2, joint3)
    val x_avg = inputProcessor.averageCoords(joints)
    assert(x_avg == (2.0, 2.0, 2.0))
  }
}
