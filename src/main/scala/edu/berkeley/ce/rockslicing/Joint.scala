package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.{DenseMatrix, DenseVector}

object Joint {
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
    w(0) = localOrigin._1 - center._1
    w(1) = localOrigin._2 - center._2
    w(2) = localOrigin._3 - center._3
    val n = DenseVector[Double](normalVec._1, normalVec._2, normalVec._3)
    NumericUtils.roundToTolerance(-(n dot w)/linalg.norm(n))
  }

  /**
    * Converts point from local to global coordinates
    * @param point The point to transform
    * @param localOrigin Local origin's global coordinates
    * @param normal Normal to joint plane
    * @param dip Dip direction of plane
    * @return Tuple that contains x, y and z coordinates of point in global coordinates
    */
  private def localPointToGlobal(point: (Double, Double, Double), localOrigin: (Double, Double, Double),
    normal: (Double, Double, Double), dip: Double): (Double, Double, Double) = {
    val Nplane = if (normal._3 > -NumericUtils.EPSILON) {
      // Ensures that normal will always point in -z global direction (ensures right-handed local coordinates)
      -1.0 * DenseVector[Double](normal._1, normal._2, normal._3)
    } else {
      DenseVector[Double](normal._1, normal._2, normal._3)
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
    val transformedPoint = Q * DenseVector[Double](point._1, point._2, point._3)
    (transformedPoint(0) + localOrigin._1, transformedPoint(1) + localOrigin._2, transformedPoint(2) + localOrigin._3)
  }

  /**
    * Find a bounding sphere for a non-persistent joint. This function is not
    * intended for use with persistent joints.
    * @param normalVec The normal vector of the plane in which the joint lies
    * @param distance The distance of the joint's plane from its local origin
    * @param centerX The x coordinate of the joint's center
    * @param centerY The y coordinate of the joint's center
    * @param centerZ The z coordinate of the joint's center
    * @param faces A sequence of faces specifying the joint's shape
    * @param dip Dip direction of joint plane
    * @return A pair where the first element is a triple giving the center of
    *         the bounding sphere and the second element is the radius of the
    *         bounding sphere.
    */
  private def findBoundingSphere(normalVec: (Double,Double,Double), distance: Double, centerX: Double,
      centerY: Double, centerZ: Double, faces: Seq[((Double,Double,Double),Double)], dip: Double):
      ((Double,Double,Double), Double) = {
    // Linear program only in 2-D since constraints are specified entirely 
    // by persistence of joint within the joint plane
    val basisVectors = Array(
      Array[Double](1.0, 0.0),
      Array[Double](0.0, 1.0),
      Array[Double](-1.0, 0.0),
      Array[Double](0.0, -1.0)
    )

    val maxCoordinates = basisVectors.map { v =>
      // Only 2 variables in linear program - in-plane bounding circle so 2-D
      val linProg = new LinearProgram(2)
      linProg.setObjFun(v.toArray, LinearProgram.MAX)
      
      faces foreach { face =>
        val (a, b, c) = face._1
        val d = face._2
        if (d < 0.0) {
          val coeffs = Array[Double](-a, -b).map(NumericUtils.applyTolerance)
          val rhs = NumericUtils.applyTolerance(-d)
          linProg.addConstraint(coeffs, LinearProgram.LE, rhs)
        } else {
          val coeffs = Array[Double](a, b).map(NumericUtils.applyTolerance)
          val rhs = NumericUtils.applyTolerance(d)
          linProg.addConstraint(coeffs, LinearProgram.LE, rhs)
        }
      }
      val results = linProg.solve().get._1
      val resultsSeq = Seq[Double](results(0), results(1))
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
    val transformedCenter = Joint.localPointToGlobal((center(0), center(1), 0.0),
      (centerX, centerY, centerZ), normalVec, dip)
    (transformedCenter, radius)
  }

  /**
    * Find the vector indicating dip direction of the joint plane. Global positive x-axis points North and
    * z-axis oriented with positive upward. Positive y-axis will point west based on this orientation.
    * @param normalVec Normal vector to the joint plane
    * @return Dip direction of the plane, indicating direction of greatest increase in z. Return as vector (a, b, 0)
    */
  private def dipDirVector(normalVec: (Double, Double, Double)): DenseVector[Double] = {
    // Dip direction is in opposite direction of gradient indicating greatest increase in z.
    if ((math.abs(normalVec._3) > NumericUtils.EPSILON) &&
        (math.abs(math.abs(normalVec._3) - 1.0) > NumericUtils.EPSILON)) {
      DenseVector[Double](normalVec._1 / normalVec._3, normalVec._2 / normalVec._3, 0.0)
    } else if (math.abs(normalVec._3) < NumericUtils.EPSILON) {
      // Joint is vertical, assigns non-zero z component that will be caught in dipDir function
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
  private def dipDir(normalVec: (Double, Double, Double)): Double = {
    val dipVector = Joint.dipDirVector(normalVec)
    val xAxis = DenseVector[Double](1.0, 0.0, 0.0)
    if (dipVector(2) != -1.0) { // Checks if joint is vertical - set to -1.0 in dipDirVector for vertical joints
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
  private def dipAngle(normalVec: (Double, Double, Double)): Double = {
    val dipVector = Joint.dipDirVector(normalVec)
    val normal = DenseVector[Double](normalVec._1, normalVec._2, normalVec._3)
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
  * @constructor Create a new joint.
  * @param normalVec The normal vector to the joint. The individual vector components
  *        can be accessed as 'a', 'b', and 'c'.
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
  */
case class Joint(normalVec: (Double, Double, Double), localOrigin: (Double, Double, Double),
                 center: (Double, Double, Double), phi: Double, cohesion: Double,
                 shape: Seq[((Double, Double, Double),Double)], dipAngleParam: Option[Double]=None,
                 dipDirectionParam: Option[Double]=None,
                 boundingSphereParam: Option[((Double,Double,Double),Double)]=null) {
  val (a, b, c) = normalVec
  val (centerX, centerY, centerZ) = center
  val d = Joint.findDistance(normalVec, localOrigin, center)
  val (localX, localY, localZ) = localOrigin

  val dipAngle = dipAngleParam match {
    case None => Joint.dipAngle(normalVec)
    case Some(da) => da
  }
  val dipDirection = dipDirectionParam match {
    case None => Joint.dipDir(normalVec)
    case Some(dd) => dd
  }

  val boundingSphere = boundingSphereParam match {
    case null => shape match {
      case Nil => None
      case _ => Some(Joint.findBoundingSphere((a, b, c), d, centerX, centerY, centerZ, shape, dipDirection))
    }
    case bs => bs
  }

  /** Converts lines defining shape of joint from local to global coordinates
    * @return A seq of pairs, each representing a plane that specifies a boundary of the
    * joint in the global coordinate space. The first item of each pair is a normal
    * vector for the plane, and the second item is the distance of the plane from the origin.
    */
  def globalCoordinates: Seq[((Double, Double, Double), Double)] = {
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

    val shapeVectors = shape.map { case ((x, y, _), _) => DenseVector[Double](x, y, 0) }
    val globalShapeVecs = shapeVectors.map { Q*_ }

    val centerVec = DenseVector[Double](centerX - localOrigin._1, centerY - localOrigin._2, centerZ - localOrigin._3)
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
  def updateJoint(blockOrigin: (Double, Double,Double)): Joint =
    Joint((a, b, c), blockOrigin, (centerX, centerY, centerZ), phi, cohesion, shape, Some(dipAngle),
          Some(dipDirection), boundingSphere)
}
