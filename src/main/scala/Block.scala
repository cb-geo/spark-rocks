package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.{DenseVector, DenseMatrix}

object Face {
  private val EPSILON = 1.0e-6
}

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
    val newA = if (math.abs(a) > Face.EPSILON) a else 0.0
    val newB = if (math.abs(b) > Face.EPSILON) b else 0.0
    val newC = if (math.abs(c) > Face.EPSILON) c else 0.0
    val newD = if (math.abs(d) > Face.EPSILON) d else 0.0
    val newPhi = if (math.abs(phi) > Face.EPSILON) phi else 0.0
    val newCohesion = if (math.abs(cohesion) > Face.EPSILON) cohesion else 0.0
    Face((newA,newB,newC), newD, newPhi, newCohesion)
  }
}

object Block {
  private val EPSILON = 1e-6

  private def applyTolerance(d: Double): Double =
    if (math.abs(d) >= EPSILON) d else 0.0
}

/** A rock block.
  *
  * @constructor Create a new rock block
  * @param center Cartesian coordinates for the center of the rock block. The individual
  * components can be accessed as 'centerX', 'centerY', and 'centerZ'.
  * @param faces: The faces that define the boundaries of the rock block.
  */
case class Block(center: (Double,Double,Double), faces: Seq[Face]) {
  val (centerX, centerY, centerZ) = center

  /**
    * Determine whether or not a joint intersects this rock block.
    * @param joint The joint to check for intersection.
    * @return None if the joint does not intersect this block, or Some((x,y,z))
    * where (x,y,z) is the point of intersection.
    */
  def intersects(joint: Joint): Option[(Double,Double,Double)] = {
    val linProg = new LinearProgram(4)
    // Minimize s
    linProg.setObjFun(Vector[Double](0.0, 0.0, 0.0, 1.0), LinearProgram.MIN)

    // Restrict our attention to plane of joint
    val translatedJoint = joint.updateJoint(centerX, centerY, centerZ)
    val coeffs = Vector[Double](translatedJoint.a, translatedJoint.b, translatedJoint.c, 0.0).map(Block.applyTolerance)
    val rhs = Block.applyTolerance(translatedJoint.d)
    linProg.addConstraint(coeffs, LinearProgram.EQ, rhs)

    // Require s to be within planes defined by faces of block
    faces.foreach { face =>
      val faceCoeffs = Vector[Double](face.a, face.b, face.c, -1.0).map(Block.applyTolerance)
      val rhs = Block.applyTolerance(face.d)
      linProg.addConstraint(faceCoeffs, LinearProgram.LE, rhs)
    }

    // Require s to be within planes defining shape of joint
    translatedJoint.globalCoordinates.foreach { case ((a,b,c),d) =>
      val jointCoeffs = Vector[Double](a, b, c, -1.0).map(Block.applyTolerance)
      val rhs = Block.applyTolerance(d)
      linProg.addConstraint(jointCoeffs, LinearProgram.LE, rhs)
    }

    linProg.solve() match {
      case None => None
      case Some((_, opt)) if opt >= -Block.EPSILON => None
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
      case Some((x,y,z)) => {
        val newX = centerX + x
        val newY = centerY + y
        val newZ = centerZ + z
        val updatedFaces = updateFaces(newX, newY, newZ)
        Vector(
          // New origin is guaranteed to lie within joint, so initial d = 0
          Block((newX,newY,newZ), Face((joint.a, joint.b, joint.c), 0.0, joint.phi, joint.cohesion)+:updatedFaces),
          Block((newX,newY,newZ), Face((-joint.a,-joint.b,-joint.c), 0.0, joint.phi, joint.cohesion)+:updatedFaces)
        )
      }
    }

  /**
    * Compute the faces of the rock block that are not geometrically redundant.
    * @return A list of faces that uniquely determine this rock block and are not
    * geometrically redundant.
    */
  def nonRedundantFaces: Seq[Face] =
    faces.distinct.filter { face =>
      val linProg = new LinearProgram(3)
      val objCoeffs = Vector(face.a, face.b, face.c).map(Block.applyTolerance)
      linProg.setObjFun(objCoeffs, LinearProgram.MAX)
      faces.foreach { f =>
        val faceCoeffs = Vector(f.a, f.b, f.c).map(Block.applyTolerance)
        val rhs = Block.applyTolerance(f.d)
        linProg.addConstraint(faceCoeffs, LinearProgram.LE, rhs)
      }
      val s = linProg.solve().get._2
      math.abs(s - face.d) <= Block.EPSILON
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
            if (math.abs(n1 dot linalg.cross(n2, n3)) > Block.EPSILON) {
              val A_matx = DenseMatrix.zeros[Double](3, 3)
              val b_vect = DenseVector[Double](f1.d, f2.d, f3.d)
              A_matx(0, ::) := n1.t
              A_matx(1, ::) := n2.t
              A_matx(2, ::) := n3.t
              val p_vect = A_matx \ b_vect
              Some((p_vect(0), p_vect(1), p_vect(2)))
            } else None
          }
        }
        // Reverse is needed to keep the same ordering as faces list since elements are added to the front
      } map (_.distinct) map (_.reverse)
    ).toMap                              // of the list as they are created.

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
    if (math.abs(linalg.norm(linalg.cross(n_c,n_d))) > Block.EPSILON) {
      val (u, v, w) = n_current

      // Rotation matrix to rotate into x-z plane
      val Txz = DenseMatrix.zeros[Double](3, 3)
      Txz(0,0) = u / math.sqrt(u*u + v*v)
      Txz(1,0) = -v /math.sqrt(u*u + v*v)
      Txz(0,1) = v / math.sqrt(u*u + v*v)
      Txz(1,1) = u / math.sqrt(u*u + v*v)
      Txz(2,2) = 1.0

      // Rotation matrix to rotate from x-z plane on z-axis
      val Tz = DenseMatrix.zeros[Double](3, 3)
      Tz(0,0) = w / math.sqrt(u*u + v*v + w*w)
      Tz(2,0) = math.sqrt(u*u + v*v) / math.sqrt(u*u + v*v + w*w)
      Tz(0,2) = -math.sqrt(u*u + v*v)/math.sqrt(u*u + v*v + w*w)
      Tz(2,2) = w / math.sqrt(u*u + v*v + w*w)
      Tz(1,1) = 1.0
      Tz * Txz
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
        if (face.c < -Block.EPSILON) {
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
      3.0 * centroidX / totalVolume + centerX,
      3.0 * centroidY / totalVolume + centerY,
      3.0 * centroidZ / totalVolume + centerZ
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
      if (math.abs(c) >= Block.EPSILON) {
        w(0) = localOrigin._1
        w(1) = localOrigin._2
        w(2) = localOrigin._3 - (d/c + centerZ)
      } else if (math.abs(b) >= Block.EPSILON) {
        w(0) = localOrigin._1
        w(1) = localOrigin._2 - (d/b + centerY)
        w(2) = localOrigin._3
      } else if (math.abs(a) >= Block.EPSILON) {
        w(0) = localOrigin._1 - (d/a + centerX)
        w(1) = localOrigin._2
        w(2) = localOrigin._3
      }
      val n = DenseVector[Double](a, b, c)
      val new_d = -(n dot w) / linalg.norm(n)
      Face((a, b, c), new_d, phi, cohesion)
    }
  }
}
