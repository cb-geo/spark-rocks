package edu.berkeley.ce.rockslicing

import breeze.linalg.DenseVector

object BlockVTK {

  /**
    * Finds the center of a list of vertices - defined as the average coordinate of all the vertices in the list
    * @param vertices List of vertices
    * @return Coordinate of center of vertices
    */
  private def findCenter(vertices: Seq[Array[Double]]): Array[Double] = {
    Array(vertices.map(_(0)).sum / vertices.length.toDouble,
     vertices.map(_(1)).sum / vertices.length.toDouble,
     vertices.map(_(2)).sum / vertices.length.toDouble)
  }

  /**
    * Compares pointA to pointB. If pointA is first relative to pointB rotating counter-clockwise about center,
    * the method returns true. Input vectors should have same z-coordinate - comparison is based in 2-D
    * @param pointA Point of interest
    * @param pointB Point of comparison
    * @param center Reference point for comparison
    * @return Returns TRUE if pointA is first relative to pointB, FALSE otherwise
    */
  private def ccwCompare(pointA: Array[Double], pointB: Array[Double],
                         center: Array[Double]): Boolean = {
    // Check that points are in the same x-y plane
    if (math.abs(NumericUtils.roundToTolerance(pointA(2), 5) -
        NumericUtils.roundToTolerance(pointB(2), 5)) > NumericUtils.EPSILON) {
      throw new IllegalArgumentException("ERROR: Input to BlockVTK.ccwCompare: "+
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
    * Arranges the vertices of each of the faces of the input block such that they are in a counter-
    * clockwise orientation relative their face's unit normal
    * @param block A rock block
    * @return A mapping from each face of the block to a Seq of vertices for that face arranged in counter-
    *         clockwise order relative to its unit normal
    */
  private def orientVertices(block: Block): Map[Face, Seq[Array[Double]]] = {
    val faceVertices = block.findVertices
    faceVertices.keys.zip(
      faceVertices.map { case (face, vertices) =>
        // Rotate vertices to all be in x-y plane
        val R = Block.rotationMatrix(face.normalVec, Array(0.0, 0.0, 1.0))
        val rotatedVerts = vertices map { vertex =>
          val rotatedVertex = R * DenseVector[Double](vertex)
          Array(rotatedVertex(0), rotatedVertex(1), rotatedVertex(2))
        }
        // Order vertices in counter-clockwise orientation
        val center = BlockVTK.findCenter(rotatedVerts)
        val orderedVerts = {
          if (face.normalVec(2) < -NumericUtils.EPSILON) {
            // If z-component of normal vector points in negative z-direction, orientation
            // needs to be reversed otherwise points will be ordered clockwise
            rotatedVerts.sortWith(BlockVTK.ccwCompare(_, _, center)).reverse
          }
          else {
            rotatedVerts.sortWith(BlockVTK.ccwCompare(_, _, center))
          }
        }
        // Rotate vertices back to original orientation
        val invR = R.t // Inverse of rotation matrix is equal to its transpose
        orderedVerts map { vertex =>
          val orderedVertex = (invR * DenseVector[Double](vertex)).map {
            NumericUtils.roundToTolerance(_) }
          Array(orderedVertex(0), orderedVertex(1), orderedVertex(2))
        }
      }
    ).toMap
  }

  /**
    * Finds the indices of the face vertices in the global list of vertices
    * @param faceOrientedVerts Mapping from each face to a Seq of vertices for that face
    * @param vertices Seq of distinct vertices for the rock block
    * @return A mapping from each face of the block to a Seq of integers that represent
    *         the indices of the vertices in the global vertex list
    */
  private def connectivity(faceOrientedVerts: Map[Face, Seq[Array[Double]]],
                           vertices: Seq[Array[Double]]): Map[Face, Seq[Int]] = {
    faceOrientedVerts map { case (face, orientedVertices) =>
      (face, orientedVertices map { vertex => vertices.indexWhere(v => v sameElements vertex) })
    }
  }

  /**
    * Creates a sequence of all the normals of all the faces for the list of input blocks
    * @param faces A mapping from each face of the block to a Seq of vertices for that face arranged in counter-
    *        clockwise order relative to its unit normal
    * @return Sequence of tuples representing normals of all the block faces
    */
  private def normals(faces: Seq[Face]): Seq[(Double, Double, Double)] = {
    faces map { case face => (face.a, face.b, face.c) }
  }

  /**
    * Determines the offset that defines each face in the connectivity list
    * @param connectivities Mapping from each face of a block to a Seq of integers that represent
    *        the indices of the vertices in the global vertex list
    * @return A Seq of integers that represent the offset of each face in the
    *         connectivity list
    */
  private def faceOffsets(connectivities: Map[Face, Seq[Int]]): Seq[Int] = {
    val localOffsets = connectivities map { case(_, connections) => connections.length }
    val offsets = Seq[Int]()

    def offsetIterator(globalOS: Seq[Int], localOS: Seq[Int]): Seq[Int] = {
      localOS match {
        case Nil => globalOS.reverse
        case offset +: rest => if (globalOS.isEmpty) {
          offsetIterator(Seq[Int](offset), rest)
        } else {
          offsetIterator((offset + globalOS.head) +: globalOS, rest)
        }
      }
    }
    offsetIterator(offsets, localOffsets.toSeq)
  }

  private def distinctArrays(vertices: Seq[Array[Double]], previous: Seq[Array[Double]]=Nil): Seq[Array[Double]] = {
    // Use a naive n^2 algorithm to avoid boxing. We may need to revisit this later.
    vertices match {
      case Nil => previous
      case vert+:verts =>
        if (previous.exists(previousVertex => previousVertex sameElements vert)) {
          distinctArrays(verts, previous)
        } else {
          distinctArrays(verts, vert+:previous)
        }
    }
  }
}

/**
  * Simple data structure to contain data that represents a rock block that can be turned into vtk format
  * @constructor Create a new rock block in format that can be turned into vtk by rockProcessor
  */
@SerialVersionUID(1L)
case class BlockVTK(block: Block) extends Serializable {
  val orientedVertices = BlockVTK.orientVertices(block)
  val arrayVertices = BlockVTK.distinctArrays(orientedVertices.values.flatten.toSeq)
  val connectivityMap = BlockVTK.connectivity(orientedVertices, arrayVertices)
  val vertices = arrayVertices flatMap { t =>
    List(t(0), t(1), t(2))
  }
  val vertexIDs = arrayVertices.indices
  val connectivity = connectivityMap.values.flatten
  val faceCount = orientedVertices.size
  val offsets = BlockVTK.faceOffsets(connectivityMap)
  val normals = BlockVTK.normals(orientedVertices.keys.toSeq) flatMap { t =>
    List(t._1, t._2, t._3)
  }
}