package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.{DenseVector, DenseMatrix}

/** A simple data structure to represent the face of a rock block.
  *
  * @constructor Create a new rock face.
  * @param normalVec The normal vector to the face. The individual vector components can
  * be accessed as 'a', 'b', and 'c'.
  * @param distance The distance from the face to the center of the rock block.
  * Accessed as 'd'.
  * @param phi The friction angle (phi) of the face.
  * @param cohesion The cohesion of the face.
  */
case class Face(normalVec: (Double,Double,Double), distance: Double, phi: Double, cohesion: Double) {
  val (a,b,c) = normalVec
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
    Face((newA,newB,newC), newD, newPhi, newCohesion)
  }
}

object Block {
  /**
   * Find a bounding sphere for a rock block.
   * @param centerX The x coordinate of the block's center
   * @param centerY The y coordinate of the block's center
   * @param centerZ The z coordinate of the block's center
   * @param faces A sequence of faces specifying the block's boundaries
   * @return A pair where the first element is a triple giving the center of
   *         the bounding sphere and the second element is the radius of the
   *         bounding sphere.
   */
  private def findBoundingSphere(centerX: Double, centerY: Double, centerZ: Double, faces: Seq[Face]):
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
      faces foreach { face =>
        val coeffs = Array[Double](face.a, face.b, face.c).map(NumericUtils.applyTolerance)
        val rhs = NumericUtils.applyTolerance(face.d)
        linProg.addConstraint(coeffs, LinearProgram.LE, rhs)
      }
      val results = linProg.solve().get._1
      val tester = Seq[Double](results(0), results(1), results(2))
      // Values of principal axes vectors set to 0.0 exacly, so okay to check for equality of Double
      if (tester.exists(x => x != 0.0)) tester.filter(_ != 0.0).head else 0.0 
    }

    val pairedCoords = maxCoordinates.take(3).zip(maxCoordinates.takeRight(3))
    val center = pairedCoords.map { case (x,y) => 0.5 * (x+y) }
    val diffVector = pairedCoords.map { case (x,y) => x - y }
    val radius = 0.5 * linalg.norm(DenseVector[Double](diffVector))

    // Shift from Block local coordinates to global coordinates
    ((center(0) + centerX, center(1) + centerY, center(2) + centerZ), radius)
  }
}

/**
  * A rock block.
  * @constructor Create a new rock block
  * @param center Cartesian coordinates for the center of the rock block. The individual
  * components can be accessed as 'centerX', 'centerY', and 'centerZ'.
  * @param faces: The faces that define the boundaries of the rock block.
  */
case class Block(center: (Double,Double,Double), faces: Seq[Face]) {
  val (centerX, centerY, centerZ) = center
  val ((sphereCenterX, sphereCenterY, sphereCenterZ), sphereRadius) =
      Block.findBoundingSphere(centerX, centerY, centerZ, faces)

  /**
    * Determine whether or not a joint intersects this rock block.
    * @param joint The joint to check for intersection.
    * @return None if the joint does not intersect this block, or Some((x,y,z))
    * where (x,y,z) is the point of intersection.
    */
  def intersects(joint: Joint): Option[(Double,Double,Double)] = {
    val sphereJoint = joint.updateJoint(sphereCenterX, sphereCenterY, sphereCenterZ)
    sphereJoint.boundingSphere match {
      case None =>
        // The joint is persistent
        if (math.abs(sphereJoint.d) > sphereRadius) None else bruteForceIntersects(joint)
      case Some(((x,y,z),r)) =>
        // The joint is not persistent
        val jointOrigin = DenseVector[Double](x,y,z)
        val blockOrigin = DenseVector[Double](sphereCenterX, sphereCenterY, sphereCenterZ)
        val distance = linalg.norm(jointOrigin - blockOrigin)
        if (distance > (sphereRadius + r)) None else bruteForceIntersects(joint)
    }
  }

  // This computes intersection without any sort of intelligence -- just solves an LP
  private def bruteForceIntersects(joint: Joint): Option[(Double,Double,Double)] = {
    val linProg = new LinearProgram(4)
    // Minimize s
    linProg.setObjFun(Vector[Double](0.0, 0.0, 0.0, 1.0), LinearProgram.MIN)

    // Restrict our attention to plane of joint
    val translatedJoint = joint.updateJoint(centerX, centerY, centerZ)
    val coeffs = Vector[Double](translatedJoint.a, translatedJoint.b, translatedJoint.c, 0.0).
                    map(NumericUtils.applyTolerance)
    val rhs = NumericUtils.applyTolerance(translatedJoint.d)
    linProg.addConstraint(coeffs, LinearProgram.EQ, rhs)

    // Require s to be within planes defined by faces of block
    faces.foreach { face =>
      val faceCoeffs = Vector[Double](face.a, face.b, face.c, -1.0).map(NumericUtils.applyTolerance)
      val rhs = NumericUtils.applyTolerance(face.d)
      linProg.addConstraint(faceCoeffs, LinearProgram.LE, rhs)
    }

    // Require s to be within planes defining shape of joint
    translatedJoint.globalCoordinates.foreach { case ((a,b,c),d) =>
      val jointCoeffs = Vector[Double](a, b, c, -1.0).map(NumericUtils.applyTolerance)
      val rhs = NumericUtils.applyTolerance(d)
      linProg.addConstraint(jointCoeffs, LinearProgram.LE, rhs)
    }

    linProg.solve() match {
      case None => None
      case Some((_, opt)) if opt >= -NumericUtils.EPSILON => None
      case Some((vars, _)) => Some((vars(0), vars(1), vars(2)))
    }
  }

  /**
    * Divide this block into two child blocks if a joint intersects this block.
    * @param joint A joint that may or may not divide this block.
    * @return A Seq of Block objects, containing the two child blocks divided by
    * the joint if it intersects this block. Otherwise, returns a one-item Seq
    * containing only this block.
    */
  def cut(joint: Joint): Seq[Block] =
    this.intersects(joint) match {
      case None => Vector(this)
      case Some((x,y,z)) =>
        val newX = NumericUtils.roundToTolerance(centerX + x)
        val newY = NumericUtils.roundToTolerance(centerY + y)
        val newZ = NumericUtils.roundToTolerance(centerZ + z)
        val updatedFaces = updateFaces(newX, newY, newZ)
        Vector(
          // New origin is guaranteed to lie within joint, so initial d = 0
          Block((newX,newY,newZ), Face((joint.a, joint.b, joint.c), 0.0, joint.phi, joint.cohesion)+:updatedFaces),
          Block((newX,newY,newZ), Face((-joint.a,-joint.b,-joint.c), 0.0, joint.phi, joint.cohesion)+:updatedFaces)
        )
    }

  /**
    * Compute the faces of the rock block that are not geometrically redundant.
    * @return A list of faces that uniquely determine this rock block and are not
    * geometrically redundant.
    */
  def nonRedundantFaces: Seq[Face] =
    faces.distinct.filter { face =>
      val linProg = new LinearProgram(3)
      val objCoeffs = Vector(face.a, face.b, face.c).map(NumericUtils.applyTolerance)
      linProg.setObjFun(objCoeffs, LinearProgram.MAX)
      faces.foreach { f =>
        val faceCoeffs = Vector(f.a, f.b, f.c).map(NumericUtils.applyTolerance)
        val rhs = NumericUtils.applyTolerance(f.d)
        linProg.addConstraint(faceCoeffs, LinearProgram.LE, rhs)
      }
      val s = linProg.solve().get._2
      math.abs(s - face.d) <= NumericUtils.EPSILON
    }

  /**
    * Calculate the vertices of each face of the block
    * @return A mapping from each face of the block to a Seq of vertices for that face
    * This function should only be called once all redundant faces have been removed.
    */
  def findVertices: Map[Face, Seq[(Double, Double, Double)]] =
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
              Some((NumericUtils.roundToTolerance(p_vect(0) + centerX),
                    NumericUtils.roundToTolerance(p_vect(1) + centerY),
                    NumericUtils.roundToTolerance(p_vect(2) + centerZ)))
            } else None
          }
        }
      } map (_.distinct)
    ).toMap                              

  /**
    * Calculates the rotation matrix to rotate the input plane (specified by its normal)
    * to the desired orientation (specified by the desired normal)
    * @param n_current: Current normal of plane
    * @param n_desired: Desired new normal
    * @return 3*3 rotation matrix
    */
  private def rotationMatrix(n_current: (Double, Double, Double), n_desired: (Double, Double, Double)):
                             DenseMatrix[Double] = {
    val n_c = DenseVector[Double](n_current._1, n_current._2, n_current._3)
    val n_d = DenseVector[Double](n_desired._1, n_desired._2, n_desired._3)
    if (math.abs(linalg.norm(linalg.cross(n_c,n_d))) > NumericUtils.EPSILON) {
      val v = linalg.cross(n_c, n_d)
      val s = linalg.norm(v)
      val c = n_c dot n_d

      val v_skew = DenseMatrix.zeros[Double](3,3)
      v_skew(0,1) = -v(2)
      v_skew(0,2) = v(1)
      v_skew(1,0) = v(2)
      v_skew(1,2) = -v(0)
      v_skew(2,0) = -v(1)
      v_skew(2,1) = v(0)

      DenseMatrix.eye[Double](3) + v_skew + (v_skew * v_skew) * (1-c)/(s*s)
//      val (u, v, w) = n_current
//
//      // Rotation matrix to rotate into x-z plane
//      val Txz = DenseMatrix.zeros[Double](3, 3)
//      Txz(0,0) = u / math.sqrt(u*u + v*v)
//      Txz(1,0) = -v /math.sqrt(u*u + v*v)
//      Txz(0,1) = v / math.sqrt(u*u + v*v)
//      Txz(1,1) = u / math.sqrt(u*u + v*v)
//      Txz(2,2) = 1.0
//
//      // Rotation matrix to rotate from x-z plane on z-axis
//      val Tz = DenseMatrix.zeros[Double](3, 3)
//      Tz(0,0) = w / math.sqrt(u*u + v*v + w*w)
//      Tz(2,0) = math.sqrt(u*u + v*v) / math.sqrt(u*u + v*v + w*w)
//      Tz(0,2) = -math.sqrt(u*u + v*v)/math.sqrt(u*u + v*v + w*w)
//      Tz(2,2) = w / math.sqrt(u*u + v*v + w*w)
//      Tz(1,1) = 1.0
//      Tz * Txz
    } else {
      DenseMatrix.eye[Double](3)
    }
  }

  /**
    * Mesh the faces using Delaunay triangulation. This meshing is done
    * in order to calculate the volume and centroid of the block
    * @return A Seq of seqs that give the local to global mapping for the
    * triangulation of each of the faces.
    */
  def meshFaces(vertices: Map[Face, Seq[(Double, Double, Double)]]): Map[Face, Seq[(Int, Int, Int)]] =
    // Determine rotation matrix to rotate faces perpendicular to z-axis. This way all vertices
    // are only in the x-y plane which makes triangulation easier.
    faces.zip (
      faces.map { face =>
        val R = rotationMatrix(face.normalVec, (0.0, 0.0, 1.0))
        val rotatedVertices = vertices(face).map { vertex =>
          val rotatedVertex = R * DenseVector(vertex._1, vertex._2, vertex._3)
          Delaunay.Vector2(rotatedVertex(0), rotatedVertex(1))
        }.distinct.toList

        // If z-component of normal is negative, order needs to be reversed to maintain clockwise orientation
        if (face.c < -NumericUtils.EPSILON) {
          Delaunay.Triangulation(rotatedVertices).map { vert =>
            (vert._3, vert._2, vert._1)
          }
        } else {
          Delaunay.Triangulation(rotatedVertices)
        }
      }
    ).toMap

  /**
    * Calculates the centroid of the block.
    * @return The centroid of the block, (centerX, centerY, centerZ).
    */
  def centroid: (Double, Double, Double) = {
    val vertices = findVertices
    val mesh = meshFaces(vertices)

    val increments = faces.flatMap { face =>
      mesh(face).map { m =>
        // Access triangulation entries backwards so they are in counterclockwise order
        val a_vals = vertices(face)(m._3)
        val b_vals = vertices(face)(m._2)
        val c_vals = vertices(face)(m._1)
        val a = DenseVector[Double](a_vals._1, a_vals._2, a_vals._3)
        val b = DenseVector[Double](b_vals._1, b_vals._2, b_vals._3)
        val c = DenseVector[Double](c_vals._1, c_vals._2, c_vals._3)

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
    (
      // Factor of 3 comes from: centroid / (2.0 * (volume/6.0))
      3.0 * centroidX / totalVolume,
      3.0 * centroidY / totalVolume,
      3.0 * centroidZ / totalVolume
    )
  }

  /**
    * Calculates the distances of the joints relative to a new origin
    * @param localOrigin: new local origin
    * @return List of faces with updated distances
    */
  def updateFaces(localOrigin: (Double, Double,Double)): Seq[Face] = {
    faces.map { case Face((a,b,c), d, phi, cohesion) =>
      val w = DenseVector.zeros[Double](3)
      if (math.abs(c) >= NumericUtils.EPSILON) {
        w(0) = localOrigin._1 - centerX
        w(1) = localOrigin._2 - centerY
        w(2) = localOrigin._3 - (d/c + centerZ)
      } else if (math.abs(b) >= NumericUtils.EPSILON) {
        w(0) = localOrigin._1 - centerX
        w(1) = localOrigin._2 - (d/b + centerY)
        w(2) = localOrigin._3 - centerZ
      } else if (math.abs(a) >= NumericUtils.EPSILON) {
        w(0) = localOrigin._1 - (d/a + centerX)
        w(1) = localOrigin._2 - centerY
        w(2) = localOrigin._3 - centerZ
      }
      val n = DenseVector[Double](a, b, c)
      val new_d = NumericUtils.roundToTolerance(-(n dot w) / linalg.norm(n))
      Face((a, b, c), new_d, phi, cohesion)
    }
  }
}
