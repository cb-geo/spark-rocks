package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.{DenseVector, DenseMatrix}

object Joint {
  private val EPSILON = 1e-6

  /**
   * Find the distance of the joint plane from the input local origin.
   * @param normalVec Normal vector to the joint plane
   * @param localOrigin The local origin from which the distance is referenced. This should be in global coordinates.
   * @param center The center of the joint plane. This should be in global coordinates.
   * @return
   */
  private def findDistance(normalVec: (Double, Double, Double), localOrigin: (Double, Double, Double),
                            center: (Double, Double, Double)) = {
    val w = DenseVector.zeros[Double](3)
    if (math.abs(normalVec._3) >= EPSILON) {
      w(0) = localOrigin._1
      w(1) = localOrigin._2
      w(2) = localOrigin._3 - center._3
    } else if (math.abs(normalVec._2) >= EPSILON) {
      w(0) = localOrigin._1
      w(1) = localOrigin._2 - center._2
      w(2) = localOrigin._3
    } else if (math.abs(normalVec._1) >= EPSILON) {
      w(0) = localOrigin._1 - center._1
      w(1) = localOrigin._2
      w(2) = localOrigin._3
    }
    val n = DenseVector[Double](normalVec._1, normalVec._2, normalVec._3)
    -(n dot w)/linalg.norm(n)
  }

  /**
   * Find the vector indicating dip direction of the joint plane. Global positive x-axis points North and z-axis oriented
   * with positive upward. Positive y-axis will point west based on this orientation.
   * @param normalVec Normal vector to the joint plane
   * @return Dip direction of the plane, indicating direction of greatest increase in z. Return as vector (a, b, 0)
   */
  private def dipDirVector(normalVec: (Double, Double, Double)) = {
    // Dip direction is in opposite direction of gradient indicating greatest increase in z.
    if ((math.abs(normalVec._3) > Joint.EPSILON) && (math.abs(math.abs(normalVec._3) - 1.0) > Joint.EPSILON)) {
      DenseVector[Double](normalVec._1 / normalVec._3, normalVec._2 / normalVec._3, 0.0)
    } else if (math.abs(normalVec._3) < Joint.EPSILON) {
      // Joint is vertical, assigns non-zero z component that will be caught is dipDir function
      DenseVector[Double](0.0, 0.0, -1.0)
    } else {
      // Joint is horizontal, dip direction arbitrarily assigned to 90 degrees so that strike is 0 degrees
      DenseVector[Double](0.0, -1.0, 0.0)
    }
  }

  /**
   * Finds the dip direction of the input joint as an azimuth. Global positive x-axis points North.
   * @param normalVec Normal vector to the joint plane
   * @return Dip direction as an azimuth in radians
   */
  private def dipDir(normalVec: (Double, Double, Double)) = {
    val dipVector = Joint.dipDirVector(normalVec)
    val xAxis = DenseVector[Double](1.0, 0.0, 0.0)
    if (dipVector(2) != -1.0) {
      if (normalVec._2 > 0.0) {
        2.0*math.Pi - math.acos((xAxis dot dipVector) / (linalg.norm(xAxis) * linalg.norm(dipVector)))
      } else {
        math.acos((xAxis dot dipVector) / (linalg.norm(xAxis) * linalg.norm(dipVector)))
      }
    } else {
      val normal = DenseVector[Double](normalVec._1, normalVec._2, normalVec._3)
      if (normalVec._2 > 0.0) {
        2.0*math.Pi - math.acos((xAxis dot normal) / (linalg.norm(normal) * linalg.norm(xAxis)))
      } else {
        math.acos((xAxis dot normal) / (linalg.norm(normal) * linalg.norm(xAxis)))
      }
    }
  }

  /**
   * Finds the dip angle of the input joint.
   * @param normalVec Normal vector to the joint plane
   * @return Dip angle in radians
   */
  private def dipAngle(normalVec: (Double, Double, Double)) = {
    val dipVector = Joint.dipDirVector(normalVec)
    val normal = DenseVector[Double](normalVec._1, normalVec._2, normalVec._3)
    if ((dipVector(1) != -1.0) && (dipVector(2) != -1.0)) {
      if (normal(2) > 0.0) {
        math.Pi/2.0 - math.acos((normal dot dipVector) / (linalg.norm(normal) * linalg.norm(dipVector)))
      } else {
        math.acos((normal dot dipVector) / (linalg.norm(normal) * linalg.norm(dipVector))) - math.Pi/2.0
      }
    } else if (dipVector(1) == -1.0) {
      // Joint is horizontal
      0.0
    } else {
      // Joint is vertical
      math.Pi/2.0
    }
  }
}

/**
  * A simple data structure to represent a joint.
  * @constructor Create a new joint.
  * @param normalVec The normal vector to the joint. The individual vector components
  * can be accessed as 'a', 'b', and 'c'.
  * @param localOrigin The local origin from which the distance is referenced. The individual
  *        components are accessed as 'localX', 'localY', and 'localZ'.
  * @param center Cartesian coordinates for the center of the joint. The individual
  *        components can be accessed as 'centerX', 'centerY', and 'centerZ'.
  * @param phi The joint's friction angle (phi).
  * @param cohesion The cohesion along the joint
  * @param shape A list of lines specifying the shape of the joint. Each item is a
  * 3-tuple. The first two items specify the line, while the last gives the distance
  * of the line from the joint's center in the local coordinate system.
  */
case class Joint(normalVec: (Double, Double, Double), localOrigin: (Double, Double, Double),
                 center: (Double, Double, Double), phi: Double, cohesion: Double,
                 shape: Seq[((Double, Double, Double),Double)]) {
  val (a, b, c) = normalVec
  val (centerX, centerY, centerZ) = center
  val d = Joint.findDistance(normalVec, localOrigin, center)
  val (localX, localY, localZ) = localOrigin
  val dipAngle = Joint.dipAngle(normalVec)
  val dipDirection = Joint.dipDir(normalVec)

  /** Converts lines defining shape of joint from local to global coordinates
    * @return A seq of pairs, each representing a plane that specifies a boundary of the
    * joint in the global coordinate space. The first item of each pair is a normal
    * vector for the plane, and the second item is the distance of the plane from the origin.
    */
  def globalCoordinates: Seq[((Double, Double, Double), Double)] = {
    var normalDirection = -1.0 // Negative needed simply because of how vertical planes are dealt with
    if (math.abs(c) > Joint.EPSILON) { // For non-verical planes
      normalDirection = -c/math.abs(c) // Ensures that normal will always point in -z global direction (ensures right-handed local coordinates)
    }
    val Nplane = normalDirection * DenseVector[Double](a, b, c)
    val strike = (dipDirection - math.Pi / 2) % (2 * math.Pi) // Strike = dipDirection - pi/2 (US convention)
    val Nstrike = DenseVector[Double](math.cos(- strike), math.sin(- strike), 0.0)
    val Ndip = linalg.cross(Nplane, Nstrike)

    // Q defines the linear transformation to convert to global coordinates
    val Q = DenseMatrix.zeros[Double](3,3)
    Q(::, 0) := Nstrike
    Q(::, 1) := Ndip
    Q(::, 2) := Nplane

    val shapeVectors = shape.map { case ((x, y, _), _) => DenseVector[Double](x, y, 0) }
    val globalShapeVecs = shapeVectors.map { Q*_ }
    val centerVec = DenseVector[Double](centerX, centerY, centerZ)

    // Calculate vectors from point in plane to local origin
    val coordinateVectors = shape.map { case ((a, b, _), d) =>
      val xl = DenseVector.zeros[Double](3)
      if (math.abs(a) > Joint.EPSILON) {
        xl(0) = d/a
        xl(1) = 0.0
        xl(2) = 0.0
      } else {
        xl(0) = 0.0
        xl(1) = d/b
        xl(2) = 0.0
      }
      val pointsGlobal = Q * xl + centerVec
      pointsGlobal - DenseVector[Double](localOrigin._1, localOrigin._2, localOrigin._3)
    }

    val globalDistances = globalShapeVecs.zip(coordinateVectors).map
                              { case (shapeVec, coorVec) => (shapeVec dot coorVec) / linalg.norm(shapeVec)}

    // Convert back to triples to hide underlying Breeze implementation
    val globalShapeTuples = globalShapeVecs.map {x => (x(0), x(1), x(2))}
    globalShapeTuples.zip(globalDistances)
  }

  /**
    * Calculates the distances of the joint relative to a new origin
    * @param blockOrigin: new local origin
    * @return Distance relative to block origin (new local origin)
    */
  def updateJoint(blockOrigin: (Double, Double,Double)): Joint = {
    Joint((a, b, c), blockOrigin, (centerX, centerY, centerZ), phi, cohesion, shape)
  }
}
