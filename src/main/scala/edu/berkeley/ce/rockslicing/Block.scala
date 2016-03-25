package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.{DenseVector, DenseMatrix}
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
  * @param processorJoint Parameter that identifies joint as being artificial joint introduced as part
  *                        of load balancing
  */
@SerialVersionUID(1L)
case class Face(normalVec: Array[Double], distance: Double, phi: Double, cohesion: Double,
                processorJoint: Boolean=false)
extends Serializable {
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
    Face(Array(newA, newB, newC), newD, newPhi, newCohesion, processorJoint)
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
    * @return True if faces are shared, false otherwise
    */
  def isSharedWith(inputFace: Face, tolerance: Double = NumericUtils.EPSILON):
      Boolean = {
    (math.abs(a + inputFace.a) < tolerance) &&
    (math.abs(b + inputFace.b) < tolerance) &&
    (math.abs(c + inputFace.c) < tolerance) &&
    (math.abs(d + inputFace.d) < tolerance)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case f: Face =>
        this.a == f.a && this.b == f.b && this.c == f.c &&
        this.distance == f.distance && this.phi == f.phi &&
        this.cohesion == f.cohesion &&
        this.processorJoint == f.processorJoint

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
        .append(processorJoint)
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
case class Block(center: Array[Double], faces: Seq[Face], generation: Int=0)
extends Serializable {
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
      linProg.setObjFun(v.toArray, LinearProgram.MAX)
      faces foreach { face =>
        val coeffs = Array[Double](face.a, face.b, face.c).map(NumericUtils.applyTolerance)
        val rhs = NumericUtils.applyTolerance(face.d)
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
        val coeffs = Array(face.a, face.b, face.c).map(NumericUtils.applyTolerance)
        val rhs = NumericUtils.applyTolerance(face.d)
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
      linProg.addConstraint(NumericUtils.applyTolerance(coeffs), LinearProgram.LE,
                            NumericUtils.applyTolerance(face.d))
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
    val coeffs = Array(joint.a, joint.b, joint.c, 0.0).
                    map(NumericUtils.applyTolerance)
    val rhs = NumericUtils.applyTolerance(joint.d)
    linProg.addConstraint(coeffs, LinearProgram.EQ, rhs)

    // Require s to be within planes defined by faces of block
    faces.foreach { face =>
      val faceCoeffs = NumericUtils.applyTolerance(Array(face.a, face.b, face.c, -1.0))
      val rhs = NumericUtils.applyTolerance(face.d)
      linProg.addConstraint(faceCoeffs, LinearProgram.LE, rhs)
    }

    // Require s to be within planes defining shape of joint
    joint.globalCoordinates.foreach { case (normal, d) =>
      val a = normal(0)
      val b = normal(1)
      val c = normal(2)
      val jointCoeffs = NumericUtils.applyTolerance(Array(a, b, c, -1.0))
      val rhs = NumericUtils.applyTolerance(d)
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
  def cut(joint: Joint, minSize: Double=0.0, maxAspectRatio: Double=Double.PositiveInfinity,
          generation: Int=0): Seq[Block] = {
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
            translatedJoint.phi, translatedJoint.cohesion, translatedJoint.processorJoint)+:updatedFaces, generation)
        } else {
          Block(Array(newX,newY,newZ), Face(Array(translatedJoint.a, translatedJoint.b, translatedJoint.c), 0.0,
            translatedJoint.phi, translatedJoint.cohesion, translatedJoint.processorJoint)+:updatedFaces, generation)
        }
        val childBlockB = if (translatedJoint.d < 0.0) {
          Block(Array(newX,newY,newZ), Face(Array(translatedJoint.a,translatedJoint.b,translatedJoint.c), 0.0,
            translatedJoint.phi, translatedJoint.cohesion, translatedJoint.processorJoint)+:updatedFaces, generation)
        } else {
          Block(Array(newX,newY,newZ), Face(Array(-translatedJoint.a,-translatedJoint.b,-translatedJoint.c), 0.0,
            translatedJoint.phi, translatedJoint.cohesion, translatedJoint.processorJoint)+:updatedFaces, generation)
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
    faces.map(_.applyTolerance).distinct.filter { face =>
      val linProg = new LinearProgram(3)
      val objCoeffs = Array(face.a, face.b, face.c)
      linProg.setObjFun(objCoeffs, LinearProgram.MAX)
      faces.foreach { f =>
        val faceCoeffs = Array(f.a, f.b, f.c)
        val rhs = f.d
        linProg.addConstraint(faceCoeffs, LinearProgram.LE, rhs)
      }
      val s = linProg.solve().get._2
      math.abs(s - face.d) <= NumericUtils.EPSILON
    }

  /**
    * Calculate the vertices of each face of the block
    *
    * @return A mapping from each face of the block to a Seq of vertices for that face
    * This function should only be called once all redundant faces have been removed.
    */
  def findVertices: Map[Face, Seq[Array[Double]]] =
    faces.zip (
      faces map { f1 =>
        val n1 = DenseVector[Double](f1.a, f1.b, f1.c)
        faces flatMap { f2 =>
          val n2 = DenseVector[Double](f2.a, f2.b, f2.c)
          faces flatMap { f3 =>
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

  /**
    * Mesh the faces using Delaunay triangulation. This meshing is done
    * in order to calculate the volume and centroid of the block
    *
    * @return A Seq of seqs that give the local to global mapping for the
    * triangulation of each of the faces.
    */
  def meshFaces(vertices: Map[Face, Seq[Array[Double]]]): Map[Face, Seq[Array[Int]]] =
    // Determine rotation matrix to rotate faces perpendicular to z-axis. This way all vertices
    // are only in the x-y plane which makes triangulation easier.
    faces.zip (
      faces.map { face =>
        val R = Block.rotationMatrix(face.normalVec, Array(0.0, 0.0, 1.0))
        val rotatedVertices = vertices(face).map { vertex =>
          val rotatedVertex = R * DenseVector(vertex)
          Delaunay.Vector2(rotatedVertex(0), rotatedVertex(1))
        }.distinct.toList
        if (rotatedVertices.isEmpty) {
          println("This is the block with empty vertices")
          println(this)
          println("And these are the vertices")
          vertices.foreach{ case (currentFace, faceVertices) =>
              println("\n"+currentFace)
              println(faceVertices)
          }
        }
        assert(rotatedVertices.nonEmpty)
        // If normal is -z-axis, order needs to be reversed to maintain clockwise orientation since
        // rotation matrix is identity matrix in this case - vectors are parallel. Rotation matrix
        // takes care of all other cases.
        if (math.abs(face.c + 1.0) < NumericUtils.EPSILON) {
          Delaunay.Triangulation(rotatedVertices).map { vert =>
            Array(vert._3, vert._2, vert._1)
          }
        } else {
          Delaunay.Triangulation(rotatedVertices).map { vert =>
            Array(vert._1, vert._2, vert._3)
          }
        }
      }
    ).toMap

  /**
    * Calculates the centroid of the block.
    * See http://wwwf.imperial.ac.uk/~rn/centroid.pdf for theoretical background.
    *
    * @return The centroid of the block, (centerX, centerY, centerZ).
    */
  def centroid: Array[Double] = {
    val vertices = findVertices
    val mesh = meshFaces(vertices)

    val increments = faces.flatMap { face =>
      mesh(face).map { m =>
        // Access triangulation entries backwards so they are in counterclockwise order
        val a_vals = vertices(face)(m(2))
        val b_vals = vertices(face)(m(1))
        val c_vals = vertices(face)(m(0))
        val a = DenseVector[Double](a_vals(0), a_vals(1), a_vals(2))
        val b = DenseVector[Double](b_vals(0), b_vals(1), b_vals(2))
        val c = DenseVector[Double](c_vals(0), c_vals(1), c_vals(2))

        val n = linalg.cross(b - a, c - a)
        val volIncrement = a dot n

        val basisVectors = Vector(
          DenseVector[Double](1.0, 0.0, 0.0),
          DenseVector[Double](0.0, 1.0, 0.0),
          DenseVector[Double](0.0, 0.0, 1.0)
        )
        val centroidIncrement = basisVectors map { vec =>
          1 / 24.0 * ((n dot vec) * (
            math.pow((a + b) dot vec, 2) +
            math.pow((b + c) dot vec, 2) +
            math.pow((c + a) dot vec, 2)
          ))
        }
        (volIncrement, (centroidIncrement(0), centroidIncrement(1), centroidIncrement(2)))
      }
    }

    val (totalVolume, (centroidX, centroidY, centroidZ)) = increments.fold (0.0, (0.0,0.0,0.0)) {
      case ((volInc1, (centX1, centY1, centZ1)),  (volInc2, (centX2, centY2, centZ2))) =>
        (volInc1 + volInc2, (centX1 + centX2, centY1 + centY2, centZ1 + centZ2))
    }
    Array(
      // Factor of 3 comes from: centroid / (2.0 * (volume/6.0))
      3.0 * centroidX / totalVolume,
      3.0 * centroidY / totalVolume,
      3.0 * centroidZ / totalVolume
    )
  }

  /**
    * Calculates the volume of the block
    * See http://wwwf.imperial.ac.uk/~rn/centroid.pdf for theoretical background.
    *
    * @return The volume of the block
    */
  def volume: Double = {
    val vertices = findVertices
    val mesh = meshFaces(vertices)

    val volIncrements = faces.flatMap { face =>
      mesh(face).map { m =>
        // Access triangulation entries backwards so they are in counterclockwise order
        val a_vals = vertices(face)(m(2))
        val b_vals = vertices(face)(m(1))
        val c_vals = vertices(face)(m(0))
        val a = DenseVector[Double](a_vals(0), a_vals(1), a_vals(2))
        val b = DenseVector[Double](b_vals(0), b_vals(1), b_vals(2))
        val c = DenseVector[Double](c_vals(0), c_vals(1), c_vals(2))

        val n = linalg.cross(b - a, c - a)
        a dot n
      }
    }

    volIncrements.sum/6.0
  }

  /**
    * Calculates the distances of the joints relative to a new origin
    *
    * @param localOrigin: new local origin
    * @return List of faces with updated distances
    */
  def updateFaces(localOrigin: Array[Double]): Seq[Face] = {
    assert(localOrigin.length == 3)

    faces.map { case Face(normal, d, phi, cohesion, processorJoint) =>
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
      Face(Array(a, b, c), new_d, phi, cohesion, processorJoint)
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

    val zippedFaces = sortedFaces1.zip(sortedFaces2)
    val faceMatches = zippedFaces forall { case (face1, face2) =>
      face1.approximateEquals(face2, tolerance)
    }
    (math.abs(centerX - updatedInputBlock.centerX) < tolerance) &&
    (math.abs(centerY - updatedInputBlock.centerY) < tolerance) &&
    (math.abs(centerZ - updatedInputBlock.centerZ) < tolerance) &&
    faceMatches
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
