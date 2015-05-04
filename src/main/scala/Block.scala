package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.{DenseVector, DenseMatrix}
import scala.language.postfixOps

/** A simple data structure to represent the face of a rock block.
  *
  * @constructor Create a new rock face.
  * @param normalVec The normal vector to the face. The individual vector components can
  * be accessed as 'a', 'b', and 'c'.
  * @param distance The distance from the normal vector to the center of the rock block.
  * Accessed as 'd'.
  * @param phi The friction angle (phi) of the face.
  * @param cohesion The cohesion of the face.
  * @param vertices: List of vertices that define the extent of the face. This list is 
  * initialized empty and only set once the face is part of a list of faces that define
  * a block. The list gets set by the findVertices method within the Block class
  * @param triangles: Triangulation of vertices to form triangles that are used to 
  * calculate the centroid of the block. This list gets set by the meshFaces method
  * within the Block class.
  */
case class Face(normalVec: (Double,Double,Double), distance: Double,
                val phi: Double, val cohesion: Double) {
  val (a,b,c) = normalVec
  val d = distance
  var vertices = List.empty[(Double, Double, Double)]
  var triangles = List.empty[(Int, Int, Int)]
}

object Block {
  private val EPSILON = 10e-12

  private def applyTolerance(d: Double): Double =
    if (math.abs(d) >= EPSILON) d else 0.0
}

/** A rock block.
  *
  * @constructor Create a new rock block
  * @param center Cartesian coordinates for the center of the rock block. The individual
  * components can be accessed as 'centerX', 'centerY', and 'centerZ'. 
  * @param faces: The faces that define the boundaries of the rock block.
  * @param vertices: List of vertices based on face intersections. This list is initialized
  * empty and only filled once findVertices is called by the user. This method should only
  * be called after redundant joints have been removed
  * @param centroid: Updates values of center when called. The centroid, assumed to be the 
  * same as the center of mass for uniform density, will change as new fractures are added
  * to the block. This function should be called once all joints have been added to the block 
  * @param volume: Volume of rock based on bounding faces. Should be called only after all
  * joints have been added to the block. Initialized as zero.
  */
case class Block(center: (Double,Double,Double), val faces: List[Face]) {
  var (centerX, centerY, centerZ) = center
  var vertices = List.empty[(Double, Double, Double)]
  var volume = 0.0

  /**
    * Determine whether or not a joint intersects this rock block.
    * @param joint The joint to check for intersection.
    * @return true if there is an intersection between the block and this joint,
    * false otherwise.
    */
  def intersects(joint: Joint): Boolean = {
    val linProg = new LinearProgram(4)
    // Minimize s
    linProg.setObjFun(Array[Double](0.0, 0.0, 0.0, 1.0), LinearProgram.MIN)

    // Restrict our attention to plane of joint
    val coeffs = Array[Double](joint.a, joint.b, joint.c, 0.0).map(Block.applyTolerance)
    val rhs = Block.applyTolerance(joint.d)
    linProg.addConstraint(coeffs, LinearProgram.EQ, rhs)

    // Require s to be within planes defined by faces of block
    faces.foreach { face =>
        val faceCoeffs = Array[Double](face.a, face.b, face.c, -1.0).map(Block.applyTolerance)
        val rhs = Block.applyTolerance(face.d)
        linProg.addConstraint(faceCoeffs, LinearProgram.LE, rhs)
    }

    // Require s to be within planes defining shape of joint
    joint.globalCoordinates.foreach { case ((a,b,c),d) =>
        val jointCoeffs = Array[Double](a, b, c, -1.0).map(Block.applyTolerance)
        val rhs = Block.applyTolerance(d)
        linProg.addConstraint(jointCoeffs, LinearProgram.LE, rhs)
    }

    linProg.solve() match {
      case None => false
      case Some(s: Double) => s < 0
    }
  }

  /**
    * Divide this block into two child blocks if a joint intersects this block.
    * @param joint A joint that may or may not divide this block.
    * @return A List of Block objects, containing the two child blocks divided by
    * the joint if it intersects this block. Otherwise, returns a one-item list
    * containing only this block.
    */
  def cut(joint: Joint): List[Block] =
    if (this intersects joint) {
      List(Block(center, Face((joint.a, joint.b, joint.c), joint.d, joint.phi, joint.cohesion)::faces),
           Block(center, Face((-joint.a,-joint.b,-joint.c), joint.d, joint.phi, joint.cohesion)::faces))
    } else {
      List(this)
    }

  /**
    * Compute the faces of the rock block that are not geometrically redundant.
    * @return A list of faces that uniquely determine this rock block and are not
    * geometrically redundant.
    */
  def nonRedundantFaces: List[Face] =
    faces.filter { face =>
      val linProg = new LinearProgram(3)
      val objCoeffs = Array[Double](face.a, face.b, face.c).map(Block.applyTolerance)
      linProg.setObjFun(objCoeffs, LinearProgram.MAX)
      faces.foreach { f =>
        val faceCoeffs = Array[Double](f.a, f.b, f.c).map(Block.applyTolerance)
        val rhs = Block.applyTolerance(f.d)
        linProg.addConstraint(faceCoeffs, LinearProgram.LE, rhs)
      }
      val s = linProg.solve().get
      math.abs(s - face.d) <= Block.EPSILON
    }

  /** 
    * Calculate the vertices of the block
    * @return Modifies the list of vertices based on the current list of Faces. This 
    * function should only be called once all redundant faces have been removed.
    */
  def findVertices: Unit = {
    // Iterate through list to check for intersection
    val tolerance = 1e-12
    for (i <- 0 until faces.length) {
      val ni = DenseVector[Double](faces(i).a, faces(i).b, faces(i).c)
      val di = faces(i).d
      for (j <- 0 until faces.length) {
        val nj = DenseVector[Double](faces(j).a, faces(j).b, faces(j).c)
        val dj = faces(j).d
        for (k <- 0 until faces.length) { 
          val nk = DenseVector[Double](faces(k).a, faces(k).b, faces(k).c)
          val dk = faces(k).d
          // check if normals of faces are coplanar, if not find intersection
          if (tolerance < math.abs((ni dot linalg.cross(nj, nk)))) {
            val A_matx = DenseMatrix.zeros[Double](3,3)
            val b_vect = DenseVector[Double](di, dj, dk)
            A_matx(0, ::) := ni.t
            A_matx(1, ::) := nj.t
            A_matx(2, ::) := nk.t
            val p_vect = A_matx \ b_vect
            vertices :::= List((p_vect(0), p_vect(1), p_vect(2)))
            faces(i).vertices :::= List((p_vect(0), p_vect(1), p_vect(2)))
          }
          faces(i).vertices = faces(i).vertices.distinct
        }
      }
    }
    vertices = vertices.distinct // Remove any duplicates from list
  }

  /**
    * Calculates the rotation matrix to rotate the input plane (specified by it's normal)
    * to the desired orientation (specified by the desired normal)
    * @param n_current: Current normal of plane
    * @param n_desired: Desired new normal
    * @return rmat: 3*3 rotation matrix
    */
  def rotationMatrix(n_current: (Double, Double, Double), n_desired: (Double, Double, Double)) :
                    DenseMatrix[Double] = {
    val tolerance = 1e-12
    val n_c = DenseVector[Double](n_current._1, n_current._2, n_current._3)
    val n_d = DenseVector[Double](n_desired._1, n_desired._2, n_desired._3)
    if (math.abs(linalg.norm(linalg.cross(n_c,n_d))) > tolerance) {
      val Txz = DenseMatrix.zeros[Double](3,3)
      val Tz = DenseMatrix.zeros[Double](3,3)
      val u = n_c(0)
      val v = n_c(1)
      val w = n_c(2)
      // Rotation matrix to rotate into x-z plane
      Txz(0,0) = u/math.sqrt(u*u + v*v)
      Txz(1,0) = -v/math.sqrt(u*u + v*v)
      Txz(0,1) = v/math.sqrt(u*u + v*v)
      Txz(1,1) = u/math.sqrt(u*u + v*v)
      Txz(2,2) = 1.0

      // Rotation matrix to rotate from x-z plane on z-axis
      Tz(0,0) = w/math.sqrt(u*u + v*v + w*w)
      Tz(2,0) = math.sqrt(u*u + v*v)/math.sqrt(u*u + v*v + w*w)
      Tz(0,2) = -math.sqrt(u*u + v*v)/math.sqrt(u*u + v*v + w*w)
      Tz(2,2) = w/math.sqrt(u*u + v*v + w*w)
      Tz(1,1) = 1.0
      val rmat = Tz * Txz
      return rmat
    } else {
      val rmat = DenseMatrix.eye[Double](3)
      return rmat
    }
  }
 
  /**
    * Mesh the faces using Delaunay triangulation. This meshing is done
    * in order to calculate the volume and centroid of the block
    * @return Modifies the triangles list in each of the faces in the list of faces
    * that define the block
    */
  def meshFaces: Unit = {
    // Determine rotation matrix to rotate faces perpendicular to z-axis. This way all vertices
    // are only in the x-y plane which makes triangulation easier.
    assert(vertices.length > 0)
    val ek = (0.0, 0.0, 1.0)
    for (i <- 0 until faces.length) {
      val nPlane = (faces(i).a, faces(i).b, faces(i).c)
      val R = rotationMatrix(nPlane, ek)
      var transformed_vertices = List.empty[(Double, Double, Double)]
      var temp_vertices = List.empty[Delaunay.Vector2]
      for (j <- 0 until faces(i).vertices.length) { // Iterate through vertices and rotate
        val temp_vector = R * DenseVector(faces(i).vertices(j)_1, faces(i).vertices(j)_2, faces(i).vertices(j)_3)
        transformed_vertices :::= List((temp_vector(0), temp_vector(1), temp_vector(2)))
        temp_vertices :::= List(Delaunay.Vector2(temp_vector(0), temp_vector(1)))
      }
      transformed_vertices = transformed_vertices.reverse
      temp_vertices = temp_vertices.reverse 
      // Vertices are returned in CLOCKWISE order
      faces(i).triangles = (Delaunay.Triangulation(temp_vertices)).toList
      // If z-component of normal is negative, order needs to be reversed to maintain clock-wise
      // orientation
      if (faces(i).c < 0.0) {
        var temp_triangles = List.empty[(Int, Int, Int)]
        for (k <- 0 until faces(i).triangles.length) {
          temp_triangles :::= List((faces(i).triangles(k)._3, faces(i).triangles(k)._2, faces(i).triangles(k)._1))
        }
        faces(i).triangles = temp_triangles.reverse
      }
    }
  }

  /**
    * Calculates the centroid and volume of the block. This function uses the mesh generated
    * by meshFaces as well as the vertices generated by findVertices, so there functions should
    * be invoked before calling this function.
    */
  def calcCentroidVolume: Unit = {
    // Calculate volume, this necessary before centroid calcs can be done
    volume = 0.0
    for (i <- 0 until faces.length) {
      assert(faces(i).triangles.length > 0)
      for (j <- 0 until faces(i).triangles.length) {
        // Access entries of triangulation in reverse so they are in ANTI-CLOCKWISE order
        val a_vals = faces(i).vertices(faces(i).triangles(j)._3)
        val b_vals = faces(i).vertices(faces(i).triangles(j)._2)
        val c_vals = faces(i).vertices(faces(i).triangles(j)._1)
        val a = DenseVector[Double](a_vals._1, a_vals._2, a_vals._3)
        val b = DenseVector[Double](b_vals._1, b_vals._2, b_vals._3)
        val c = DenseVector[Double](c_vals._1, c_vals._2, c_vals._3)
        val ni = linalg.cross(b-a,c-a)
        volume += a dot ni
      }
    }
    volume = volume/6.0
    // Calculate centroid
    var temp_centroid = DenseVector.zeros[Double](3)
    for (i <- 0 until faces.length) {
      for (j <- 0 until faces(i).triangles.length) {
        val a_vals = faces(i).vertices(faces(i).triangles(j)._3)
        val b_vals = faces(i).vertices(faces(i).triangles(j)._2)
        val c_vals = faces(i).vertices(faces(i).triangles(j)._1)
        val a = DenseVector[Double](a_vals._1, a_vals._2, a_vals._3)
        val b = DenseVector[Double](b_vals._1, b_vals._2, b_vals._3)
        val c = DenseVector[Double](c_vals._1, c_vals._2, c_vals._3)
        val ni = linalg.cross(b-a,c-a)/linalg.norm(linalg.cross(b-a,c-a))
        // Unit normals for each axis
        val ei = DenseVector[Double](1.0, 0.0, 0.0)
        val ej = DenseVector[Double](0.0, 1.0, 0.0)
        val ek = DenseVector[Double](0.0, 0.0, 1.0)
        val normal_list = List(ei, ej, ek)
        // Loop over each Cartesian axis
        for (k <- 0 until 3) {
          temp_centroid(k) += 1/24.0 * (ni dot normal_list(k) 
                                     * (math.pow((a+b) dot normal_list(k), 2) 
                                      + math.pow((b+c) dot normal_list(k), 2) 
                                      + math.pow((c+a) dot normal_list(k), 2)))          
        }
      } 
    }
    temp_centroid :*= 1/(2.0*volume)
    centerX = temp_centroid(0)
    centerY = temp_centroid(1)
    centerZ = temp_centroid(2)
  }
}
