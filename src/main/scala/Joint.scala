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
                            center: (Double, Double, Double)): Double = {
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

  private def applyTolerance(d: Double): Double =
    if (math.abs(d) >= EPSILON) d else 0.0

  /**
   * Find a bounding sphere for a non-persistent joint. This function is not
   * intended for use with persistent joints.
   * @param normalVec The normal vector of the plane in which the joint lies
   * @param distance The distance of the joint's plane from its local origin
   * @param centerX The x coordinate of the joint's center
   * @param centerY The y coordinate of the joint's center
   * @param centerZ The z coordinate of the joint's center
   * @param faces A sequence of faces specifying the joint's shape
   * @return A pair where the first element is a triple giving the center of
   *         the bounding sphere and the second element is the radius of the
   *         bounding sphere.
   */
  private def findBoundingSphere(normalVec: (Double,Double,Double), distance: Double, centerX: Double,
      centerY: Double, centerZ: Double, faces: Seq[((Double,Double,Double),Double)]):
      ((Double,Double,Double), Double) = {
    val basisVectors = Array(
      Array[Double](1.0, 0.0, 0.0),
      Array[Double](0.0, 1.0, 0.0),
      Array[Double](0.0, 0.0, 1.0),
      Array[Double](-1.0, 0.0, 0.0),
      Array[Double](0.0, -1.0, 0.0),
      Array[Double](0.0, 0.0, -1.0)
    )

    val maxCoordinates = basisVectors.map { v =>
      val linProg = new LinearProgram(3)
      linProg.setObjFun(v.toArray, LinearProgram.MAX)
      val jointCoeffs = Array[Double](normalVec._1, normalVec._2, normalVec._3).map(applyTolerance)
      val jointRhs = applyTolerance(distance)
      linProg.addConstraint(jointCoeffs, LinearProgram.LE, jointRhs)

      faces foreach { face =>
        val (a,b,c) = face._1
        val d = face._2
        val coeffs = Array[Double](a, b, c).map(applyTolerance)
        val rhs = applyTolerance(d)
        linProg.addConstraint(coeffs, LinearProgram.LE, rhs)
      }
      linProg.solve().get._2
    }

    val pairedCoords = maxCoordinates.take(3).zip(maxCoordinates.takeRight(3))
    val center = pairedCoords.map { case (x,y) => 0.5 * (x+y) }
    val diffVector = pairedCoords.map { case (x,y) => x - y }
    val radius = 0.5 * linalg.norm(DenseVector[Double](diffVector))

    // Shift from Joint local coordinates to global coordinates
    ((center(0) + centerX, center(1) + centerY, center(2) + centerZ), radius)
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
  * @param boundingSphereParam An optional parameter that can be used to specify the bounding
  *                            sphere for the joint, if it is known. This prevents an expensive
  *                            recalculation of the bounding sphere.
  */
case class Joint(normalVec: (Double, Double, Double), localOrigin: (Double, Double, Double),
                 center: (Double, Double, Double), dipAngle: Double, dipDirection: Double, phi: Double,
                 cohesion: Double, shape: Seq[((Double, Double, Double),Double)],
                 boundingSphereParam: Option[((Double,Double,Double),Double)]=null) {
  val (a, b, c) = normalVec
  val (centerX, centerY, centerZ) = center
  val d = Joint.findDistance(normalVec, localOrigin, center)
  val (localX, localY, localZ) = localOrigin
  val boundingSphere = boundingSphereParam match {
    case null => shape match {
      case Nil => None
      case _ => Some(Joint.findBoundingSphere((a, b, c), d, centerX, centerY, centerZ, shape))
    }
    case bs => bs
  }

  /** Converts lines defining shape of joint from local to global coordinates
    * @return A seq of pairs, each representing a plane that specifies a boundary of the
    * joint in the global coordinate space. The first item of each pair is a normal
    * vector for the plane, and the second item is the distance of the plane from the origin.
    */
  def globalCoordinates: Seq[((Double, Double, Double), Double)] = {
    val Nplane = DenseVector[Double](a, b, c)
    val strike = (dipDirection - math.Pi / 2) % (2 * math.Pi) // CHECKED: strike = dipDirection - pi/2 (US convention)
    val Nstrike = DenseVector[Double](math.cos(strike), math.sin(strike), 0.0)
    val Ndip = linalg.cross(Nplane, Nstrike)

    // Q defines the linear transformation to convert to global coordinates
    val Q = DenseMatrix.zeros[Double](3,3)
    Q(::, 0) := Nstrike
    Q(::, 1) := Ndip
    Q(::, 2) := Nplane

    val shapeVectors = shape.map { case ((x, y, _), _) => DenseVector[Double](x, y, 0) }
    val globalShapeVecs = shapeVectors.map { Q*_ }

    val centerVec = DenseVector[Double](centerX, centerY, centerZ)
    val localDistances = shape.map { _._2 }
    val globalDistances = globalShapeVecs.zip(localDistances).map
                              { case (shapeVec, dist) => dist + shapeVec.dot(centerVec) }

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
    Joint((a, b, c), blockOrigin, (centerX, centerY, centerZ), dipAngle, dipDirection, phi, cohesion,
          shape, boundingSphere)
  }
}
