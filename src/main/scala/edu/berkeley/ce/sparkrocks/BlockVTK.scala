package edu.berkeley.ce.sparkrocks

import breeze.linalg.DenseVector

object BlockVTK {

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
  val orientedVertices = block.orientedVertices
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