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
}

/**
 * Simple data structure to contain data that represents list of blocks that can be turned into vtk format
 */
case class BlocksVTK(blocks: Seq[Block]) {
  val blockID = blocks.indices
  val faceVertices = blocks map { // Sequence of maps of faces and their corresponding vertices
    _.findVertices
  }

  /**
   * Finds the list of unique vertices for the entire list of input faces
   * @param faceSeq Sequence of maps of faces and their vertices
   * @return List of unique vertices
   */
  def vertices(faceSeq: Seq[Map[Face, Seq[(Double, Double, Double)]]]): Seq[(Double, Double, Double)] = {
    ((faceSeq flatMap {
      _.values
    }) flatMap (_.distinct)).distinct.toList
  }

  def orderedVerts(faceSeq: Seq[Map[Face, Seq[(Double, Double, Double)]]]):
                            Seq[Map[Face, Seq[(Double, Double, Double)]]] = {
    faceSeq map {
      case face =>
        face.keys.zip(
          face.map { kv =>
            kv._2 map { vertex =>
              // CONTINUE HERE: ALSO NEED TO CHANGE THIS SO ROTATION MATRIX ISN'T RECALCULATED FOR EACH VERTEX
              val rotatedVertex = BlocksVTK.rotationMatrix(kv._1.normalVec, (0.0, 0.0, 1.0)) * vertex
            }
          }
        )
    }
  }

  /**
   * Finds the indices of the face vertices in the global list of vertices
   * @param faceSeq Sequence of maps of faces and their vertices
   * @return Sequence of maps of faces and the indices of their vertices in the global vertex list
   */
  def connectivity(faceSeq: Seq[Map[Face, Seq[(Double, Double, Double)]]]): Seq[Map[Face, Seq[Int]]] = {
    faceSeq map {
      case face =>
        face.keys.zip(
          face.map { kv =>
            kv._2 map { case vertex =>
              vertices(faceSeq).indexOf(vertex)
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

//  val vertices = {                                 // List of all unique vertices
//    ((faceVertices flatMap {
//      _.values
//    }) flatMap (_.distinct)).distinct.toList
//  }
//  val connectivity = faceVertices map {
//    case face =>
//      face.keys.zip (
//        face.map { kv =>
//          kv._2 map { case vertex =>
//            vertices.indexOf(vertex)
//          }
//        }
//      ).toMap
//  }
