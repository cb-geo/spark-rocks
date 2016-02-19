package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.{DenseMatrix, DenseVector}

object Joint {
  /**
    * Find the distance of the joint plane from the input local origin.
    *
    * @param normalVec Normal vector to the joint plane
    * @param localOrigin The local origin from which the distance is referenced. This should be in global coordinates.
    * @param center The center of the joint plane. This should be in global coordinates.
    * @return
    */
  private def findDistance(normalVec: Array[Double], localOrigin: Array[Double],
                            center: Array[Double]): Double = {
    assert(normalVec.length == 3 && localOrigin.length == 3 && center.length == 3)

    val w = DenseVector.zeros[Double](3)
    w(0) = localOrigin(0) - center(0)
    w(1) = localOrigin(1) - center(1)
    w(2) = localOrigin(2) - center(2)
    val n = DenseVector[Double](normalVec)
    NumericUtils.roundToTolerance(-(n dot w) / linalg.norm(n))
  }

  /**
    * Converts point from local to global coordinates
    *
    * @param point The point to transform
    * @param localOrigin Local origin's global coordinates
    * @param normal Normal to joint plane
    * @param dip Dip direction of plane
    * @return Tuple that contains x, y and z coordinates of point in global coordinates
    */
  private def localPointToGlobal(point: Array[Double], localOrigin: Array[Double],
    normal: Array[Double], dip: Double): Array[Double] = {
    assert(point.length == 3 && localOrigin.length == 3 && normal.length == 3)
    val Nplane = if (normal(2) > -NumericUtils.EPSILON) {
      // Ensures that normal will always point in -z global direction (ensures right-handed local coordinates)
      -1.0 * DenseVector[Double](normal)
    } else {
      DenseVector[Double](normal)
    }
    val strike = (dip - math.Pi / 2) % (2 * math.Pi) // strike = dip - pi/2 (US convention)
    val Nstrike = DenseVector[Double](math.cos(-strike), math.sin(-strike), 0.0)
    val Ndip = linalg.cross(Nplane, Nstrike)

    // Q defines the linear transformation to convert to global coordinates
    val Q = DenseMatrix.zeros[Double](3,3)
    Q(::, 0) := Nstrike
    Q(::, 1) := Ndip
    Q(::, 2) := Nplane

    // Transform point from local to global coordinates
    val transformedPoint = Q * DenseVector[Double](point)
    Array(
      transformedPoint(0) + localOrigin(0),
      transformedPoint(1) + localOrigin(1),
      transformedPoint(2) + localOrigin(2)
    )
  }

  /**
    * Find a bounding sphere for a non-persistent joint. This function is not
    * intended for use with persistent joints.
    *
    * @param normalVec The normal vector of the plane in which the joint lies
    * @param distance The distance of the joint's plane from its local origin
    * @param centerX The x coordinate of the joint's center
    * @param centerY The y coordinate of the joint's center
    * @param centerZ The z coordinate of the joint's center
    * @param faces A sequence of faces specifying the joint's shape
    * @param dip Dip direction of joint plane
    * @return A pair where the first element is a 3-element double array giving the center of
    *         the bounding sphere and the second element is the radius of the
    *         bounding sphere.
    */
  private def findBoundingSphere(normalVec: Array[Double], distance: Double, centerX: Double,
      centerY: Double, centerZ: Double, faces: Seq[(Array[Double],Double)], dip: Double):
      (Array[Double], Double) = {
    assert(normalVec.length == 3)

    // Linear program only in 2-D since constraints are specified entirely
    // by persistence of joint within the joint plane
    val basisVectors = Array(
      Array(1.0, 0.0),
      Array(0.0, 1.0),
      Array(-1.0, 0.0),
      Array(0.0, -1.0)
    )

    val maxCoordinates = basisVectors.map { v =>
      // Only 2 variables in linear program - in-plane bounding circle so 2-D
      val linProg = new LinearProgram(2)
      linProg.setObjFun(v, LinearProgram.MAX)

      faces foreach { case (normal, d) =>
        val a = normal(0)
        val b = normal(1)
        if (d < 0.0) {
          val coeffs = Array(NumericUtils.applyTolerance(-a), NumericUtils.applyTolerance(-b))
          val rhs = NumericUtils.applyTolerance(-d)
          linProg.addConstraint(coeffs, LinearProgram.LE, rhs)
        } else {
          val coeffs = Array(NumericUtils.applyTolerance(a), NumericUtils.applyTolerance(b))
          val rhs = NumericUtils.applyTolerance(d)
          linProg.addConstraint(coeffs, LinearProgram.LE, rhs)
        }
      }
      val results = linProg.solve().get._1
      val resultsSeq = Seq(results(0), results(1))
      // Values of principal axes vectors set to 0.0 exacly, so okay to check for equality of Double
      resultsSeq.filter(math.abs(_) > NumericUtils.EPSILON) match {
        case Nil => 0.0
        case x+:xs => x
      }
    }

    val pairedCoords = maxCoordinates.take(2).zip(maxCoordinates.takeRight(2))
    val center = pairedCoords.map { case (x, y) => 0.5 * (x + y) }
    val diffVector = pairedCoords.map { case (x, y) => x - y }
    val radius = 0.5 * linalg.norm(DenseVector[Double](diffVector))

    // Shift from Joint local coordinates to global coordinates
    val transformedCenter = Joint.localPointToGlobal(Array(center(0), center(1), 0.0),
      Array(centerX, centerY, centerZ), normalVec, dip)
    (transformedCenter, radius)
  }

  /**
    * Find the vector indicating dip direction of the joint plane. Global positive x-axis points North and
    * z-axis oriented with positive upward. Positive y-axis will point west based on this orientation.
    *
    * @param normalVec Normal vector to the joint plane
    * @return Dip direction of the plane, indicating direction of greatest increase in z. Return as vector (a, b, 0)
    */
  private def dipDirVector(normalVec: Array[Double]): DenseVector[Double] = {
    assert(normalVec.length == 3)
    // Dip direction is in opposite direction of gradient indicating greatest increase in z.
    if ((math.abs(normalVec(2)) > NumericUtils.EPSILON) &&
        (math.abs(math.abs(normalVec(2)) - 1.0) > NumericUtils.EPSILON)) {
      DenseVector[Double](normalVec(0) / normalVec(2), normalVec(1) / normalVec(2), 0.0)
    } else if (math.abs(normalVec(2)) < NumericUtils.EPSILON) {
      // Joint is vertical, assigns non-zero z component that will be caught in dipDir function
      DenseVector[Double](0.0, 0.0, -1.0)
    } else {
      // Joint is horizontal, dip direction arbitrarily assigned to 90 degrees so that strike is 0 degrees
      DenseVector[Double](0.0, -1.0, 0.0)
    }
  }

  /**
    * Finds the dip direction of the input joint as an azimuth. Global positive x-axis points North.
    *
    * @param normalVec Normal vector to the joint plane
    * @return Dip direction as an azimuth in radians
    */
  private def dipDir(normalVec: Array[Double]): Double = {
    assert(normalVec.length == 3)
    val dipVector = Joint.dipDirVector(normalVec)
    val xAxis = DenseVector[Double](1.0, 0.0, 0.0)
    if (dipVector(2) != -1.0) { // Checks if joint is vertical - set to -1.0 in dipDirVector for vertical joints
      if (normalVec(1) > 0.0) {
        2.0*math.Pi - math.acos((xAxis dot dipVector) / (linalg.norm(xAxis) * linalg.norm(dipVector)))
      } else {
        math.acos((xAxis dot dipVector) / (linalg.norm(xAxis) * linalg.norm(dipVector)))
      }
    } else {
      val normal = DenseVector[Double](normalVec(0), normalVec(1), normalVec(2))
      if (normalVec(1) > 0.0) {
        2.0*math.Pi - math.acos((xAxis dot normal) / (linalg.norm(normal) * linalg.norm(xAxis)))
      } else {
        math.acos((xAxis dot normal) / (linalg.norm(normal) * linalg.norm(xAxis)))
      }
    }
  }

  /**
    * Finds the dip angle of the input joint.
    *
    * @param normalVec Normal vector to the joint plane
    * @return Dip angle in radians
    */
  private def dipAngle(normalVec: Array[Double]): Double = {
    assert(normalVec.length == 3)
    val dipVector = Joint.dipDirVector(normalVec)
    val normal = DenseVector[Double](normalVec(0), normalVec(1), normalVec(2))
    // Checks for horizontal and vertical joints. This is set in dipDirVector function so can compare doubles exactly
    if ((dipVector(1) != -1.0) && (dipVector(2) != -1.0)) {
      if (normal(2) > 0.0) {
        math.Pi/2.0 - math.acos((normal dot dipVector) / (linalg.norm(normal) * linalg.norm(dipVector)))
      } else {
        math.acos((normal dot dipVector) / (linalg.norm(normal) * linalg.norm(dipVector))) - math.Pi/2.0
      }
    } else if (dipVector(1) == -1.0) { // Joint is horizontal
      0.0
    } else { // Joint is vertical
      math.Pi/2.0
    }
  }
}

/**
  * A simple data structure to represent a joint.
  *
  * @constructor Create a new joint.
  * @param normalVec The normal vector to the joint. The individual vector components
  *        can be accessed as 'a', 'b', and 'c'. Assumed to be a unit vector.
  * @param localOrigin The local origin from which the distance is referenced. The individual
  *        components are accessed as 'localX', 'localY', and 'localZ'.
  * @param center Cartesian coordinates for the center of the joint. The individual
  *        components can be accessed as 'centerX', 'centerY', and 'centerZ'.
  * @param phi The joint's friction angle (phi).
  * @param cohesion The cohesion along the joint
  * @param shape A list of lines specifying the shape of the joint. Each item is a
  *        3-tuple. The first two items specify the line, while the last gives the distance
  *        of the line from the joint's center in the local coordinate system.
  * @param boundingSphereParam An optional parameter that can be used to specify the bounding
  *        sphere for the joint, if it is known. This prevents an expensive recalculation
  *        of the bounding sphere.
  * @param dipAngleParam An optional parameter that can be used to specify the dip angle for the joint.
  *        This avoids recalculation of a known dip angle.
  * @param dipDirectionParam An optional parameter that can be used to specify the dip direction for
  *        the joint. This avoids recalculation of a known dip direction.
  * @param processorJoint Parameter that identifies joint as being artificial joint introduced as part
  *                        of load balancing
  */
case class Joint(normalVec: Array[Double], localOrigin: Array[Double],
                 center: Array[Double], phi: Double, cohesion: Double,
                 shape: Vector[(Array[Double],Double)], dipAngleParam: Option[Double]=None,
                 dipDirectionParam: Option[Double]=None,
                 boundingSphereParam: Option[(Array[Double],Double)]=null,
                 processorJoint: Boolean=false) {
  assert(normalVec.length == 3 && localOrigin.length == 3 && center.length == 3)

  val a = normalVec(0)
  val b = normalVec(1)
  val c = normalVec(2)

  val centerX = center(0)
  val centerY = center(1)
  val centerZ = center(2)
  val d = Joint.findDistance(normalVec, localOrigin, center)

  val localX = localOrigin(0)
  val localY = localOrigin(1)
  val localZ = localOrigin(2)

  val dipAngle = dipAngleParam match {
    case None => Joint.dipAngle(normalVec)
    case Some(da) => da
  }
  val dipDirection = dipDirectionParam match {
    case None => Joint.dipDir(normalVec)
    case Some(dd) => dd
  }

  val boundingSphere = boundingSphereParam match {
    case null =>
      if (shape.isEmpty) {
        None
      } else {
        Some(Joint.findBoundingSphere(normalVec, d, centerX, centerY, centerZ, shape, dipDirection))
      }
    case bs => bs
  }

  /** Converts lines defining shape of joint from local to global coordinates
    *
    * @return A seq of pairs, each representing a plane that specifies a boundary of the
    * joint in the global coordinate space. The first item of each pair is a normal
    * vector for the plane, and the second item is the distance of the plane from the origin.
    */
  def globalCoordinates: Seq[(Array[Double], Double)] = {
    val Nplane = if (c > -NumericUtils.EPSILON) {
      // Ensures that normal will always point in -z global direction (ensures right-handed local coordinates)
      -1.0 * DenseVector[Double](a, b, c)
    } else {
      DenseVector[Double](a, b, c)
    }
    val strike = (dipDirection - math.Pi / 2) % (2 * math.Pi) // Strike = dipDirection - pi/2 (US convention)
    val Nstrike = DenseVector[Double](math.cos(-strike), math.sin(-strike), 0.0)
    val Ndip = linalg.cross(Nplane, Nstrike)

    // Q defines the linear transformation to convert to global coordinates
    val Q = DenseMatrix.zeros[Double](3,3)
    Q(::, 0) := Nstrike
    Q(::, 1) := Ndip
    Q(::, 2) := Nplane

    val shapeVectors = shape.map { case (normal, _) => DenseVector[Double](normal(0), normal(1), 0) }
    val globalShapeVecs = shapeVectors.map { Q*_ }

    val centerVec = DenseVector[Double](centerX - localOrigin(0), centerY - localOrigin(1), centerZ - localOrigin(2))
    val localDistances = shape.map { _._2 }

    val globalDistances = globalShapeVecs.zip(localDistances).map
                              { case (shapeVec, dist) => dist + shapeVec.dot(centerVec) }

    // Convert back to triples to hide underlying Breeze implementation
    val globalShapeTuples = globalShapeVecs.map {x => Array(x(0), x(1), x(2))}
    globalShapeTuples.zip(globalDistances)
  }

  /**
    * Calculates the distances of the joint relative to a new origin
    *
    * @param blockOrigin: new local origin
    * @return Distance relative to block origin (new local origin)
    */
  def updateJoint(blockOrigin: Array[Double]): Joint = {
    assert(blockOrigin.length == 3)
    Joint(normalVec, blockOrigin, center, phi, cohesion, shape, Some(dipAngle),
      Some(dipDirection), boundingSphere, processorJoint)
  }

  /**
    * Compare this joint and input block's faces and determines if joint is one of
    * input block's faces.
    * @param block Input block
    * @return True if joint is one of blocks faces, false otherwise
    */
  def inBlock(block: Block): Boolean = {
    val distance = Joint.findDistance(Array(a, b, c), Array(block.centerX, block.centerY, block.centerZ),
                                      Array(centerX, centerY, centerZ))

    block.faces.exists { face =>
      ((math.abs(face.a - a) < NumericUtils.EPSILON) &&
       (math.abs(face.b - b) < NumericUtils.EPSILON) &&
       (math.abs(face.c - c) < NumericUtils.EPSILON) &&
       (math.abs(face.d - distance) < NumericUtils.EPSILON)) ||
      ((math.abs(face.a + a) < NumericUtils.EPSILON) &&
       (math.abs(face.b + b) < NumericUtils.EPSILON) &&
       (math.abs(face.c + c) < NumericUtils.EPSILON) &&
       (math.abs(face.d + distance) < NumericUtils.EPSILON))
    }
  }
}
