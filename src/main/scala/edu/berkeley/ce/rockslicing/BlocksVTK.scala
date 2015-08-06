package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.{DenseVector, DenseMatrix}

object BlocksVTK {

  private val EPSILON = 1.0e-6

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
    if (math.abs(linalg.norm(linalg.cross(n_c,n_d))) > BlocksVTK.EPSILON) {
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
   * Finds the center of a list of vertices - defined as the average coordinate of all the vertices in the list
   * @param vertices List of vertices
   * @return Coordinate of center of vertices
   */
  private def findCenter(vertices: Seq[(Double, Double, Double)]): (Double, Double, Double) = {
    (vertices.map(_._1).sum / vertices.length.toDouble,
      vertices.map(_._2).sum / vertices.length.toDouble,
      vertices.map(_._3).sum / vertices.length.toDouble)
  }

  /**
   * Compares pointA to pointB. If pointA is first relative to pointB rotating counter-clockwise about center,
   * the method returns true. Input vectors should have same z-coordinate - comparison is based in 2-D
   * @param pointA Point of interest
   * @param pointB Point of comparison
   * @param center Reference point for comparison
   * @return Returns TRUE if pointA is first relative to pointB, FALSE otherwise
   */
  private def ccwCompare(pointA: (Double, Double, Double), pointB: (Double, Double, Double),
                           center: (Double, Double, Double)): Boolean = {
    assert(math.abs(pointA._3 - pointB._3) < BlocksVTK.EPSILON)
    if ((pointA._1 - center._1 < BlocksVTK.EPSILON) && (pointB._1 - center._1 >= -BlocksVTK.EPSILON)) {
      return true
    }
    if ((pointA._1 - center._1 >= -BlocksVTK.EPSILON) && (pointB._1 - center._1 < BlocksVTK.EPSILON)) {
      return false
    }
    if ((math.abs(pointA._1 - center._1) < BlocksVTK.EPSILON) &&
        (math.abs(pointB._1 - center._1) < BlocksVTK.EPSILON)) {
      if ((pointA._2 - center._2 >= BlocksVTK.EPSILON) || (pointB._2 - center._2 >= BlocksVTK.EPSILON)) {
        return pointA._2 > pointB._2
      }
      return pointB._2 > pointA._2
    }
    // the cross product of vectors (pointA - center) and (pointB - center)
    val det = ((pointA._1 - center._1) * (pointB._2 - center._2) -
               (pointB._1 - center._1) * (pointA._2 - center._2)).toInt
    if (det > 0) {
      return true
    }
    if (det < 0) {
      return false
    }
    // pointA and pointB are on the same line from the center, so check which one is closer to the center
    val d1 = (pointA._1 - center._1) * (pointA._1 - center._1) + (pointA._2 - center._2) * (pointA._2 - center._2)
    val d2 = (pointB._1 - center._1) * (pointB._1 - center._1) + (pointB._2 - center._2) * (pointB._2 - center._2)
    return d1 > d2
  }
}

/**
 * Simple data structure to contain data that represents list of blocks that can be turned into vtk format
 */
case class BlocksVTK(blocks: Seq[Block]) {
  val blockID = blocks.indices
  val faceVertices = blocks map { _.findVertices } // Sequence of maps of faces and their corresponding vertices

  /**
   * Finds the list of unique vertices for the entire list of input faces
   * @param faceSeq Sequence of maps of faces and their vertices
   * @return List of unique vertices
   */
  def vertices(faceSeq: Seq[Map[Face, Seq[(Double, Double, Double)]]]): Seq[(Double, Double, Double)] = {
    (faceSeq flatMap { _.values }).flatten.distinct.toList
  }

  /**
   * Rotates all the vertices of all the faces in the input such that they lie in the x-y plane
   * @param faceSeq Sequence of maps of faces and their vertices
   * @return Sequence of maps of faces and their vertices rotated such they lie in the x-y plane
   */
  def rotatedVerts(faceSeq: Seq[Map[Face, Seq[(Double, Double, Double)]]]):
  Seq[Map[Face, Seq[(Double, Double, Double)]]] = {
    faceSeq map {
      case face =>
        face.keys.zip(
          face.map { kv =>
            val R = BlocksVTK.rotationMatrix(kv._1.normalVec, (0.0, 0.0, 1.0))
            kv._2 map { vertex =>
              val rotatedVert = R * DenseVector[Double](vertex._1, vertex._2, vertex._3)
              (rotatedVert(0), rotatedVert(1), rotatedVert(2))
            }
          }
        ).toMap
    }
  }

  /**
   * Orders vertices in a counter-clockwise orientation relative to unit normal of face
   * @param faceSeq Sequence of maps of faces and their vertices
   * @return Sequence of maps of faces and their vertices sorted in a counter-clockwise orientation
   */
  def orderedVerts(faceSeq: Seq[Map[Face, Seq[(Double, Double, Double)]]]):
                            Seq[Map[Face, Seq[(Double, Double, Double)]]] = {
    faceSeq map {
      case face =>
        face.keys.zip(
          face.map { kv =>
            val center = BlocksVTK.findCenter(kv._2)
            if (kv._1.normalVec._3 < -BlocksVTK.EPSILON) {
              kv._2.sortWith(BlocksVTK.ccwCompare(_, _, center)).reverse
            }
            else {
              kv._2.sortWith(BlocksVTK.ccwCompare(_, _, center))
            }
          }
        ).toMap
    }
  }

  /**
   * Finds the indices of the face vertices in the global list of vertices
   * @param faceSeq Sequence of maps of faces and their vertices
   * @return Sequence of maps of faces and the indices of their vertices in the global vertex list
   */
  def connectivity(faceSeq: Seq[Map[Face, Seq[(Double, Double, Double)]]],
                   verts: Seq[(Double, Double, Double)]): Seq[Map[Face, Seq[Int]]] = {
    faceSeq map {
      case face =>
        face.keys.zip(
          face.map { kv =>
            kv._2 map { case vertex =>
              verts.indexOf(vertex)
            }
          }
        ).toMap
    }
  }

  /**
   * Creates a sequence of all the normals of all the faces for the list of input blocks
   * @return Sequence of tuples representing normals of all the block faces
   */
  def normals: Seq[(Double, Double, Double)] = {
    blocks flatMap { case block =>
      block.faces map { case face =>
        (face.a, face.b, face.c)
      }
    }
  }
}
