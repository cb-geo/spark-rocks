package edu.berkeley.ce.rockslicing

/**
 * Simple data structure to contain data that represents list of blocks that can be turned into vtk format
 */
case class BlocksVTK(blocks: Seq[Block]) {
  val faceVertices = blocks map { _.findVertices } // map of faces and their corresponding vertices
  val vertices = { // sequence of all unique vertices
    ((faceVertices flatMap {
      _.values
    }) flatMap (_.distinct)).distinct
  }

}
