package edu.berkeley.ce.sparkrocks

import breeze.linalg
import breeze.linalg.{DenseMatrix, DenseVector}
import org.apache.commons.lang3.builder.HashCodeBuilder

/** A simple data structure to represent the face of a rock block.
  *
  * @constructor Create a new rock face.
  * @param normalVec The normal vector to the face. The individual vector components can
  * be accessed as 'a', 'b', and 'c'. Assumed to be a unit vector.
  * @param distance The distance from the face to the center of the rock block.
  * Accessed as 'd'.
  * @param phi The friction angle (phi) of the face.
  * @param cohesion The cohesion of the face.
  */
@SerialVersionUID(1L)
case class Face(normalVec: Array[Double], distance: Double, phi: Double, cohesion: Double) extends Serializable {
  assert(normalVec.length == 3)
  val a = normalVec(0)
  val b = normalVec(1)
  val c = normalVec(2)
  val d = distance

  /**
    * Checks if each of the parameters of a face is approximately 0.0 and,
    * if so, sets it to 0.0.
    */
  def applyTolerance: Face = {
    val newA = if (math.abs(a) > NumericUtils.EPSILON) a else 0.0
    val newB = if (math.abs(b) > NumericUtils.EPSILON) b else 0.0
    val newC = if (math.abs(c) > NumericUtils.EPSILON) c else 0.0
    val newD = if (math.abs(d) > NumericUtils.EPSILON) d else 0.0
    val newPhi = if (math.abs(phi) > NumericUtils.EPSILON) phi else 0.0
    val newCohesion = if (math.abs(cohesion) > NumericUtils.EPSILON) cohesion else 0.0
    Face(Array(newA, newB, newC), newD, newPhi, newCohesion)
  }

  /**
    * Rounds the components of a face's normal vector to the specified number
    * of decimal places, which defaults to 6.
    * @param decimalPlaces The number of decimal places to round to.
    * @return A new face with a rounded normal vector.
    */
  def roundToTolerance(decimalPlaces: Int=6): Face = {
    Face(Array(NumericUtils.roundToTolerance(a, decimalPlaces), NumericUtils.roundToTolerance(b, decimalPlaces),
         NumericUtils.roundToTolerance(c, decimalPlaces)), NumericUtils.roundToTolerance(d, decimalPlaces),
         phi, cohesion)
  }

  /**
    * Compare this face and input face for approximate equality within specified tolerance
    *
    * @param inputFace Input face
    * @param tolerance Tolerance for difference between face parameters. Defaults to
    *                  NumericUtils.EPSILON
    * @return True if faces are equal, otherwise false
    */
  def approximateEquals(inputFace: Face, tolerance: Double=NumericUtils.EPSILON):
      Boolean = {
    (math.abs(a - inputFace.a) < tolerance) &&
    (math.abs(b - inputFace.b) < tolerance) &&
    (math.abs(c - inputFace.c) < tolerance) &&
    (math.abs(d - inputFace.d) < tolerance)
  }

  /**
    * Compares this face with input face and determines whether faces are shared. Shared
    * faces will have equal and opposite distances from local origin as well as
    * normal vectors in opposite directions
    *
    * @param inputFace Input face
    * @param tolerance Tolerance for difference between compared values. Defaults to
    *                  EPSILON.
    * @return True if faces are shared, false otherwise
    */
  def isSharedWith(inputFace: Face, tolerance: Double = NumericUtils.EPSILON):
      Boolean = {
    (math.abs(a + inputFace.a) <= tolerance) &&
    (math.abs(b + inputFace.b) <= tolerance) &&
    (math.abs(c + inputFace.c) <= tolerance) &&
    (math.abs(d + inputFace.d) <= tolerance)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case f: Face =>
        this.a == f.a && this.b == f.b && this.c == f.c &&
        this.distance == f.distance && this.phi == f.phi &&
        this.cohesion == f.cohesion

      case _ => false
    }
  }

  override def hashCode: Int =
    new HashCodeBuilder()
        .append(a)
        .append(b)
        .append(c)
        .append(distance)
        .append(phi)
        .append(cohesion)
        .toHashCode
}

object Block {
  /**
    * Calculates the rotation matrix to rotate the input plane (specified by its normal)
    * to the desired orientation (specified by the desired normal)
    *
    * @param nCurrent: Current normal of plane
    * @param nDesired: Desired new normal
    * @return 3*3 rotation matrix
    */
  def rotationMatrix(nCurrent: Array[Double], nDesired: Array[Double]): DenseMatrix[Double] = {
    assert(nCurrent.length == 3 && nDesired.length == 3)

    val nC = DenseVector[Double](nCurrent(0), nCurrent(1), nCurrent(2))
    val nD = DenseVector[Double](nDesired(0), nDesired(1), nDesired(2))
    // Check if vectors are parallel
    if (math.abs(linalg.norm(linalg.cross(nC, nD))) > NumericUtils.EPSILON) {
      val v = linalg.cross(nC, nD)
      val s = linalg.norm(v)
      val c = nC dot nD

      val ASkew = DenseMatrix.zeros[Double](3,3)
      ASkew(0,1) = -v(2)
      ASkew(0,2) = v(1)
      ASkew(1,0) = v(2)
      ASkew(1,2) = -v(0)
      ASkew(2,0) = -v(1)
      ASkew(2,1) = v(0)

      DenseMatrix.eye[Double](3) + ASkew + (ASkew * ASkew) * (1-c)/(s*s)
    } else {
      // Vectors are parallel, so return identity
      DenseMatrix.eye[Double](3)
    }
  }

  /**
    * Compares pointA to pointB. If pointA is first relative to pointB rotating counter-clockwise about center,
    * the method returns true. Input vectors should have same z-coordinate - comparison is based in 2-D
    *
    * @param pointA Point of interest
    * @param pointB Point of comparison
    * @param center Reference point for comparison
    * @return Returns TRUE if pointA is first relative to pointB, FALSE otherwise
    */
  private def ccwCompare(pointA: Array[Double], pointB: Array[Double],
                         center: Array[Double]): Boolean = {
    // Check that points are in the same x-y plane
    if (math.abs(NumericUtils.roundToTolerance(pointA(2)) -
      NumericUtils.roundToTolerance(pointB(2))) > NumericUtils.EPSILON) {
      throw new IllegalArgumentException("ERROR: Input to Block.ccwCompare: " +
        "Input points are not in the same plane")
    }

    // Starts counter-clockwise comparison from 12 o'clock. Center serves as origin and 12 o'clock is along
    // vertical line running through this center.
    // Check if points are on opposite sides of the center. Points left of center will be before points
    // right of the center
    if ((pointA(0) - center(0) < -NumericUtils.EPSILON) && (pointB(0) - center(0) >= NumericUtils.EPSILON)) {
      return true
    }
    else if ((pointA(0) - center(0) >= NumericUtils.EPSILON) && (pointB(0) - center(0) < -NumericUtils.EPSILON)) {
      return false
    }

    // Compares points that fall on the x = center._1 line.
    if ((math.abs(pointA(0) - center(0)) < NumericUtils.EPSILON) &&
      (math.abs(pointB(0) - center(0)) < NumericUtils.EPSILON)) {
      // Points furthest away from the center will be before points that are closer to the center
      if ((pointA(1) - center(1) >= NumericUtils.EPSILON) || (pointB(1) - center(1) >= NumericUtils.EPSILON)) {
        return pointA(1) > pointB(1)
      } else {
        return pointB(1) > pointA(1)
      }
    }

    // The cross product of vectors (pointA - center) and (pointB - center) in determinant form. Since it's
    // in 2-D we're only interested in the sign of the resulting z-vector.
    val det = (pointA(0) - center(0)) * (pointB(1) - center(1)) -
      (pointB(0) - center(0)) * (pointA(1) - center(1))
    // If resulting vector points in positive z-direction, pointA is before pointB
    if (det > NumericUtils.EPSILON) {
      true
    } else if (det < -NumericUtils.EPSILON) {
      false
    } else {
      // pointA and pointB are on the same line from the center, so check which one is closer to the center
      val d1 = (pointA(0) - center(0)) * (pointA(0) - center(0)) + (pointA(1) - center(1)) * (pointA(1) - center(1))
      val d2 = (pointB(0) - center(0)) * (pointB(0) - center(0)) + (pointB(1) - center(1)) * (pointB(1) - center(1))
      d1 > d2
    }
  }

  /**
    * Finds the center of a list of vertices - defined as the average coordinate of all the vertices in the list
    *
    * @param vertices List of vertices
    * @return Coordinate of center of vertices
    */
  private def findCenter(vertices: Seq[Array[Double]]): Array[Double] = {
    Array(vertices.map(_(0)).sum / vertices.length.toDouble,
      vertices.map(_(1)).sum / vertices.length.toDouble,
      vertices.map(_(2)).sum / vertices.length.toDouble)
  }
}

/**
  * A rock block.
  *
  * @constructor Create a new rock block
  * @param center Cartesian coordinates for the center of the rock block. The individual
  * components can be accessed as 'centerX', 'centerY', and 'centerZ'.
  * @param faces The faces that define the boundaries of the rock block.
  * @param generation An integer identifying the joint that caused this rock block to be cut
  *                   from its parent. The generation is used to avoid unnecessary checks
  *                   for geometrically redundant faces, but it can be safely left as its
  *                   default value of 0.
  */
@SerialVersionUID(1L)
case class Block(center: Array[Double], faces: Seq[Face], generation: Int=0) extends Serializable {
  assert(center.length == 3)
  val centerX = center(0)
  val centerY = center(1)
  val centerZ = center(2)

  // Computing a bounding sphere is expensive, so we only do it if necessary
  lazy val ((sphereCenterX, sphereCenterY, sphereCenterZ), sphereRadius) = findBoundingSphere match {
    case (sphereCenter, radius) => ((sphereCenter(0), sphereCenter(1), sphereCenter(2)), radius)
  }
  // Computing the maximum radius of an inscribed sphere is also expensive
  lazy val maxInscribableRadius = findMaxInscribableRadius
  // Computing the volume is expensive, so only do it when necessary
  lazy val volume = findVolume

  /**
    * Determines whether the input point is outside the input block
    *
    * @param point The point as a tuple
    * @return True if the point is outside the block, false otherwise
    */
  private def pointOutsideBlock(point: Array[Double]): Boolean = {
    assert(point.length == 3)

    faces exists { face =>
      val pointVector = DenseVector[Double](point(0), point(1), point(2))
      val faceNormal = DenseVector[Double](face.a, face.b, face.c)
      // If distance is negative, point lies within block - planes will not intersect within block
      // since block is convex
      ((pointVector dot faceNormal) - face.d) > NumericUtils.EPSILON
    }
  }

  /**
   * Find a bounding sphere for a rock block.
   *
   * @return A pair where the first element is a triple giving the center of
   *         the bounding sphere and the second element is the radius of the
   *         bounding sphere.
   */
  private def findBoundingSphere: (Array[Double], Double) = {
    val positiveBasisVectors = Array(
      Array[Double](1.0, 0.0, 0.0),
      Array[Double](0.0, 1.0, 0.0),
      Array[Double](0.0, 0.0, 1.0)
    )

    val negativeBasisVectors = Array(
      Array[Double](-1.0, 0.0, 0.0),
      Array[Double](0.0, -1.0, 0.0),
      Array[Double](0.0, 0.0, -1.0)
    )

    // Solve LP to find maximum principal coordinate values
    val maxCoordinates = positiveBasisVectors.map { v =>
      val linProg = new LinearProgram(3)
      linProg.setObjFun(v, LinearProgram.MAX)
      faces foreach { face =>
        val coeffs = Array[Double](face.a, face.b, face.c)
        val rhs = face.d
        linProg.addConstraint(coeffs, LinearProgram.LE, rhs)
      }
      val results = linProg.solve().get._1
      val resultsSeq = Seq[Double](results(0), results(1), results(2))
      resultsSeq match {
        case Nil => 0.0
        case x => x.max
      }
    }

    // Solve LP to find minimum principal coordinate values
    val minCoordinates = negativeBasisVectors.map { v =>
      val linProg = new LinearProgram(3)
      linProg.setObjFun(v, LinearProgram.MAX)
      faces foreach { face =>
        val coeffs = Array(face.a, face.b, face.c)
        val rhs = face.d
        linProg.addConstraint(coeffs, LinearProgram.LE, rhs)
      }
      val results = linProg.solve().get._1
      val resultsSeq = Seq[Double](results(0), results(1), results(2))
      resultsSeq match {
        case Nil => 0.0
        case x => x.min
      }
    }

    val pairedCoords = maxCoordinates.zip(minCoordinates)
    val center = pairedCoords.map { case (x,y) => 0.5 * (x+y) }
    val diffVector = pairedCoords.map { case (x,y) => x - y }
    val radius = 0.5 * linalg.norm(DenseVector[Double](diffVector))

    // Shift from Block local coordinates to global coordinates
    (Array(center(0) + centerX, center(1) + centerY, center(2) + centerZ), radius)
  }

  private def findMaxInscribableRadius: Double = {
    /*
     * Equation (18) in Boon et. al., 2015 expresses this as a minimization.
     * We use a maximization LP to deal with negative distances explicitly
     * rather than falling back on absolute value.
     */
    val linProg = new LinearProgram(4)
    linProg.setObjFun(Array(0.0, 0.0, 0.0, 1.0), LinearProgram.MAX)
    faces foreach { face =>
      val coeffs = Array(face.a, face.b, face.c, 1.0)
      linProg.addConstraint(coeffs, LinearProgram.LE,
                            face.d)
    }
    linProg.solve().get._2
  }

  /**
    * Determine whether or not a joint intersects this rock block.
    *
    * @param joint The joint to check for intersection.
    * @return None if the joint does not intersect this block, or Some((x,y,z))
    * where (x,y,z) is the point of intersection.
    */
  def intersects(joint: Joint): Option[Array[Double]] = {
    val sphereJoint = joint.updateJoint(Array(sphereCenterX, sphereCenterY, sphereCenterZ))
    sphereJoint.boundingSphere match {
      case None =>
        // The joint is persistent
        if (math.abs(sphereJoint.d) > sphereRadius) None else bruteForceIntersects(joint)
      case Some((cent,r)) =>
        // The joint is not persistent
        val jointOrigin = DenseVector[Double](cent)
        val blockOrigin = DenseVector[Double](sphereCenterX, sphereCenterY, sphereCenterZ)
        val distance = linalg.norm(jointOrigin - blockOrigin)
        if (distance > (sphereRadius + r)) None else bruteForceIntersects(joint)
    }
  }

  // This computes intersection without any sort of intelligence -- just solves an LP
  private def bruteForceIntersects(joint: Joint): Option[Array[Double]] = {
    val linProg = new LinearProgram(4)
    // Minimize s
    linProg.setObjFun(Array(0.0, 0.0, 0.0, 1.0), LinearProgram.MIN)

    // Restrict our attention to plane of joint
    val coeffs = Array(joint.a, joint.b, joint.c, 0.0)
    val rhs = joint.d
    linProg.addConstraint(coeffs, LinearProgram.EQ, rhs)

    // Require s to be within planes defined by faces of block
    faces.foreach { face =>
      val faceCoeffs = Array(face.a, face.b, face.c, -1.0)
      val rhs = face.d
      linProg.addConstraint(faceCoeffs, LinearProgram.LE, rhs)
    }

    // Require s to be within planes defining shape of joint
    joint.globalCoordinates.foreach { case (normal, d) =>
      val a = normal(0)
      val b = normal(1)
      val c = normal(2)
      val jointCoeffs = Array(a, b, c, -1.0)
      val rhs = d
      linProg.addConstraint(jointCoeffs, LinearProgram.LE, rhs)
    }

    linProg.solve() match {
      case None => None
      case Some((_, opt)) if opt >= -NumericUtils.EPSILON => None
      case Some((vars, _)) => Some(vars)
    }
  }

  /**
    * Divide this block into two child blocks if a joint intersects this block.
    *
    * @param joint A joint that may or may not divide this block.
    * @param minSize The minimum radius of a sphere that can be inscribed in the child blocks.
    *                If either child block falls below this minimum, no cut is performed.
    * @param maxAspectRatio The maximum ratio of a child block's bounding sphere to the radius
    *                       of the largest sphere that can be inscribed in the block. If either
    *                       child falls above this minimum, no cut is performed.
    * @param generation The generation of any child blocks cut from this joint (see Block constructor
    *                   documentation for a description of the generation field).
    * @return A Seq of Block objects, containing the two child blocks divided by
    * the joint if it intersects this block and any minimum requirements for radius
    * or aspect ratio are met. Otherwise, returns a one-item Seq containing
    * only this block.
    */
  def cut(joint: Joint, minSize: Double, maxAspectRatio: Double, generation: Int=0): Seq[Block] = {
    val translatedJoint = joint.updateJoint(Array(centerX, centerY, centerZ))
    this.intersects(translatedJoint) match {
      case None => Vector(this)
      case Some(coord) =>
        val x = coord(0)
        val y = coord(1)
        val z = coord(2)
        val newX = NumericUtils.roundToTolerance(centerX + x)
        val newY = NumericUtils.roundToTolerance(centerY + y)
        val newZ = NumericUtils.roundToTolerance(centerZ + z)
        val updatedFaces = updateFaces(Array(newX, newY, newZ))

        // New origin is guaranteed to lie within joint, so initial d = 0 for all child blocks
        val childBlockA = if (translatedJoint.d < 0.0) {
          Block(Array(newX,newY,newZ), Face(Array(-translatedJoint.a, -translatedJoint.b, -translatedJoint.c), 0.0,
            translatedJoint.phi, translatedJoint.cohesion)+:updatedFaces, generation)
        } else {
          Block(Array(newX,newY,newZ), Face(Array(translatedJoint.a, translatedJoint.b, translatedJoint.c), 0.0,
            translatedJoint.phi, translatedJoint.cohesion)+:updatedFaces, generation)
        }
        val childBlockB = if (translatedJoint.d < 0.0) {
          Block(Array(newX,newY,newZ), Face(Array(translatedJoint.a,translatedJoint.b,translatedJoint.c), 0.0,
            translatedJoint.phi, translatedJoint.cohesion)+:updatedFaces, generation)
        } else {
          Block(Array(newX,newY,newZ), Face(Array(-translatedJoint.a,-translatedJoint.b,-translatedJoint.c), 0.0,
            translatedJoint.phi, translatedJoint.cohesion)+:updatedFaces, generation)
        }

        var childBlocks = Vector(childBlockA, childBlockB)
        // Check maximum radius of inscribable sphere for both children
        if (minSize > 0.0) {
          val inscribedRadiusA = childBlockA.maxInscribableRadius
          val inscribedRadiusB = childBlockB.maxInscribableRadius
          if (inscribedRadiusA < minSize || inscribedRadiusB < minSize) {
            childBlocks = Vector(this)
          }
        }
        // If necessary, also check the aspect ratio of both children
        if (maxAspectRatio != Double.PositiveInfinity && childBlocks.length != 1) {
          val aspectRatioA = childBlockA.sphereRadius / childBlockA.maxInscribableRadius
          val aspectRatioB = childBlockB.sphereRadius / childBlockB.maxInscribableRadius
          if (aspectRatioA > maxAspectRatio || aspectRatioB > maxAspectRatio) {
            childBlocks = Vector(this)
          }
        }

        childBlocks
    }
  }
  /**
    * Compute the faces of the rock block that are not geometrically redundant.
    *
    * @return A list of faces that uniquely determine this rock block and are not
    * geometrically redundant.
    */
  def nonRedundantFaces: Seq[Face] =
    faces.distinct.filter { face =>
      val linProg = new LinearProgram(3)
      val objCoeffs = Array(face.a, face.b, face.c)
      linProg.setObjFun(objCoeffs, LinearProgram.MAX)
      faces.foreach { f =>
        val faceCoeffs = Array(f.a, f.b, f.c)
        val rhs = f.d
        linProg.addConstraint(faceCoeffs, LinearProgram.LE, rhs)
      }
      val s = linProg.solve().get._2
      math.abs(s - face.d) < NumericUtils.EPSILON / 10.0
    }

  /**
    * Calculate the vertices of each face of the block
    *
    * @return A mapping from each face of the block to a Seq of vertices for that face
    * This function should only be called once all redundant faces have been removed.
    */
  def calcVertices: Map[Face, Seq[Array[Double]]] = {
    val relevantFaces = nonRedundantFaces

    relevantFaces.zip (
      relevantFaces map { f1 =>
        val n1 = DenseVector[Double](f1.a, f1.b, f1.c)
        relevantFaces flatMap { f2 =>
          val n2 = DenseVector[Double](f2.a, f2.b, f2.c)
          relevantFaces flatMap { f3 =>
            val n3 = DenseVector[Double](f3.a, f3.b, f3.c)
            // Check if normals of faces are coplanar, if not find intersection
            if (math.abs(n1 dot linalg.cross(n2, n3)) > NumericUtils.EPSILON) {
              val A_matx = DenseMatrix.zeros[Double](3, 3)
              val b_vect = DenseVector[Double](f1.d, f2.d, f3.d)
              A_matx(0, ::) := n1.t
              A_matx(1, ::) := n2.t
              A_matx(2, ::) := n3.t
              val p_vect = A_matx \ b_vect
              // Check if point is within block, otherwise discard it
              if (!pointOutsideBlock(Array(p_vect(0), p_vect(1), p_vect(2)))) {
                Some((p_vect(0) + centerX,
                      p_vect(1) + centerY,
                      p_vect(2) + centerZ))
              } else None
            } else None
          }
        }
      } map (_.distinct) map {seq => seq map { triple => Array(triple._1, triple._2, triple._3) } }
    ).toMap
  }

  /**
    * Get the counter-clockwise oriented vertices of each face of the block
    *
    * @return A mapping from each face of the block to a Seq of vertices for that face
    *         in a counter-clockwise orientation.
    *         This function should only be called once all redundant faces have been removed.
    */
  def orientedVertices: Map[Face, Seq[Array[Double]]] = {
    val faceVertices = calcVertices
    faceVertices.transform { case (face, vertices) =>
      // Rotate vertices to all be in x-y plane
      val R = Block.rotationMatrix(face.normalVec, Array(0.0, 0.0, 1.0))
      val rotatedVerts = vertices map { vertex =>
        val rotatedVertex = R * DenseVector[Double](vertex)
        Array(rotatedVertex(0), rotatedVertex(1), rotatedVertex(2))
      }
      // Order vertices in counter-clockwise orientation
      val center = Block.findCenter(rotatedVerts)
      val orderedVerts = if (face.normalVec(2) + 1.0 < NumericUtils.EPSILON) {
        // If z-component of normal vector points in negative z-direction, orientation
        // needs to be reversed otherwise points will be ordered clockwise
        rotatedVerts.sortWith(Block.ccwCompare(_, _, center)).reverse
      } else {
        rotatedVerts.sortWith(Block.ccwCompare(_, _, center))
      }

      // Rotate vertices back to original orientation
      val invR = R.t // Inverse of rotation matrix is equal to its transpose
      orderedVerts map { vertex =>
        val orderedVertex = (invR * DenseVector[Double](vertex)).map(NumericUtils.roundToTolerance(_))
        Array(orderedVertex(0), orderedVertex(1), orderedVertex(2))
      }
    }
  }

  /**
    * Calculates the centroid of the block using simplex integration.
    *
    * @return The centroid of the block, (centerX, centerY, centerZ).
    */
  def centroid: Array[Double] = {
    val faceVertexMap = orientedVertices

    // Check if block is extremely small
    if (volume <= NumericUtils.EPSILON) {
      // If volume is essentially 0.0, return average of all vertices as centroid
      val allVertices = faceVertexMap.values.flatten
      Array[Double](
        allVertices.map(_(0)).sum / allVertices.size,
        allVertices.map(_(1)).sum / allVertices.size,
        allVertices.map(_(2)).sum / allVertices.size
      )
    } else {
      val increments = faceVertexMap.flatMap { case (face, faceVertices) =>
        // First vertex of face used in all iterations
        val anchorVertex = faceVertices(0)

        // Tetrahedra are constructed using three vertices at a time
        faceVertices.drop(1).sliding(2).map { case Seq(firstVertex, secondVertex) =>
          // Translate vertex coordinates from global to local where center of mass is local origin
          val vertex1 = Array[Double](anchorVertex(0) - centerX,
            anchorVertex(1) - centerY,
            anchorVertex(2) - centerZ)
          val vertex2 = Array[Double](firstVertex(0) - centerX,
            firstVertex(1) - centerY,
            firstVertex(2) - centerZ)
          val vertex3 = Array[Double](secondVertex(0) - centerX,
            secondVertex(1) - centerY,
            secondVertex(2) - centerZ)

          // Initialize Jacobian
          val Jacobian = DenseMatrix(vertex1, vertex2, vertex3)
          val JacobianDet = linalg.det(Jacobian)

          // Calculate x, y and z centroid increments
          ( (vertex1(0) + vertex2(0) + vertex3(0)) * JacobianDet,
            (vertex1(1) + vertex2(1) + vertex3(1)) * JacobianDet,
            (vertex1(2) + vertex2(2) + vertex3(2)) * JacobianDet )
        }
      }

      val (centroidX, centroidY, centroidZ) = increments.fold (0.0, 0.0, 0.0) {
        case ((centX1, centY1, centZ1),  (centX2, centY2, centZ2)) =>
          (centX1 + centX2, centY1 + centY2, centZ1 + centZ2)
      }

      Array(centroidX / (volume * 24.0) + centerX,
        centroidY / (volume * 24.0) + centerY,
        centroidZ / (volume * 24.0) + centerZ)
    }
  }

  /**
    * Calculates the volume of the block using simplex integration
    *
    * @return The volume of the block
    */
  private def findVolume: Double = {
    val faceVertexMap = orientedVertices

    val volIncrements = faceVertexMap.flatMap { case (face, faceVertices) =>
      // Tetrahedra are constructed using three vertices at a time
      faceVertices.sliding(3).map { case Seq(vertex1, vertex2, vertex3) =>
        // Initialize Jacobian matrix
        val Jacobian = DenseMatrix(
          (1.0, centerX, centerY, centerZ),
          ( 1.0, vertex1(0), vertex1(1), vertex1(2) ),
          ( 1.0, vertex2(0), vertex2(1), vertex2(2) ),
          ( 1.0, vertex3(0), vertex3(1), vertex3(2) )
        )

        // Calculate determinant of Jacobian
        linalg.det(Jacobian)
      }
    }
    volIncrements.sum / 6.0
  }

  /**
    * Calculates the distances of the joints relative to a new origin
    *
    * @param localOrigin: new local origin
    * @return List of faces with updated distances
    */
  def updateFaces(localOrigin: Array[Double]): Seq[Face] = {
    assert(localOrigin.length == 3)

    faces.map { case Face(normal, d, phi, cohesion) =>
      val a = normal(0)
      val b = normal(1)
      val c = normal(2)

      val w = DenseVector.zeros[Double](3)
      if (math.abs(c) >= NumericUtils.EPSILON) {
        w(0) = localOrigin(0) - centerX
        w(1) = localOrigin(1) - centerY
        w(2) = localOrigin(2) - (d/c + centerZ)
      } else if (math.abs(b) >= NumericUtils.EPSILON) {
        w(0) = localOrigin(0) - centerX
        w(1) = localOrigin(1) - (d/b + centerY)
        w(2) = localOrigin(2) - centerZ
      } else if (math.abs(a) >= NumericUtils.EPSILON) {
        w(0) = localOrigin(0) - (d/a + centerX)
        w(1) = localOrigin(1) - centerY
        w(2) = localOrigin(2) - centerZ
      }

      val n = DenseVector[Double](a, b, c)
      val new_d = NumericUtils.roundToTolerance(-(n dot w) / linalg.norm(n))
      Face(Array(a, b, c), new_d, phi, cohesion)
    }
  }

  /**
    * Compare this block and input block for approximate equality within specified tolerance
    *
    * @param inputBlock Input block
    * @return True if blocks are equal, otherwise false
    */
  def approximateEquals(inputBlock: Block, tolerance: Double=NumericUtils.EPSILON):
                    Boolean = {
    if (faces.length != inputBlock.faces.length) {
      return false
    }
    val centroid = Array(centerX, centerY, centerZ)
    val updatedInputBlock = Block(centroid, inputBlock.updateFaces(centroid))

    val cleanFaces = faces.map(_.applyTolerance)
    val cleanInputFaces = updatedInputBlock.faces.map(_.applyTolerance)

    val sortedFaces1 = cleanFaces.sortBy(face => (face.d, face.a, face.b, face.c))
    val sortedFaces2 = cleanInputFaces.sortBy(face => (face.d, face.a, face.b, face.c))

    (math.abs(centerX - updatedInputBlock.centerX) < tolerance) &&
    (math.abs(centerY - updatedInputBlock.centerY) < tolerance) &&
    (math.abs(centerZ - updatedInputBlock.centerZ) < tolerance) &&
    sortedFaces1.zip(sortedFaces2).forall { case (face1, face2) =>
      face1.approximateEquals(face2, tolerance)
    }
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case b: Block =>
        this.centerX == b.centerX && this.centerY == b.centerY &&
        this.centerZ == b.centerZ && this.faces == b.faces
      case _ => false
    }
  }

  override def hashCode: Int = {
    val hcBuilder = new HashCodeBuilder()
    hcBuilder.append(centerX)
    hcBuilder.append(centerY)
    hcBuilder.append(centerZ)
    faces foreach { face =>
      hcBuilder.append(face.hashCode)
    }

    hcBuilder.toHashCode
  }
}
