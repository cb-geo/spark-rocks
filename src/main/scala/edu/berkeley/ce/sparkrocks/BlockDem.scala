package edu.berkeley.ce.sparkrocks

/** Simple data structure to contain data that represents a rock block face to read as input into DEM codes
  *
  * @constructor Create a new rock face for DEM block
  * @param face Face to create DEM face from
  * @param vertices Seq of Arrays containing coordinates of face vertices oriented counter-clockwise relative
  *                 to face normal
  */
@SerialVersionUID(1L)
case class FaceDem(face: Face, vertices: Seq[Array[Double]]) extends Serializable {
  val a = face.a
  val b = face.b
  val c = face.c
  val d = face.d
  val phi = face.phi
  val cohesion = face.cohesion
}

/**
  * Simple data structure to contain data that represents a rock block that can be used in a DEM analysis
  *
  * @param block Input block to create DEM block from
  * @constructor Create a new rock block in format that can be turned into vtk by rockProcessor
  */
@SerialVersionUID(1L)
case class BlockDem(block: Block) {
  val centerX = block.centerX
  val centerY = block.centerY
  val centerZ = block.centerZ
  val sphereCenterX = block.sphereCenterX
  val sphereCenterY = block.sphereCenterY
  val sphereCenterZ = block.sphereCenterZ
  val sphereRadius = block.sphereRadius
  val (vertices, connectivity) = initializeVertices(block)
  val faces = initializeFaces(block.orientedVertices)

  /**
    * Initialize DEM-specific faces for DEM block
    *
    * @param orientedFaces Map of faces and face vertices, oriented in counter-clockwise order
    * @return Seq of DEM-specific faces
    */
  private def initializeFaces(orientedFaces: Map[Face, Seq[Array[Double]]]): Seq[FaceDem] = {
    (orientedFaces map { case (face, vertices) =>
        FaceDem(face, vertices)
    }).toSeq
  }

  /**
    * Initialize vertices for DEM block
    *
    * @param block Input block to create DEM block from
    * @return Tuple containing two entries. First is Seq of vertices in block and second is Seq
    *         of Seqs containing connectivities of block faces based on vertex indices
    */
  private def initializeVertices(block: Block): (Seq[Array[Double]], Seq[Seq[Int]]) = {
    val orientedVertices = block.orientedVertices
    val arrayVertices = BlockVTK.distinctArrays(orientedVertices.values.flatten.toSeq)
    val connectivityMap = BlockVTK.connectivity(orientedVertices, arrayVertices)
    (arrayVertices, connectivityMap.values.toSeq)
  }
}