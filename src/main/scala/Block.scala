package edu.berkeley.ce.rockslicing

/** A simple data structure to represent the face of a rock block.
  *
  * @constructor Create a new rock face.
  * @param normalVec The normal vector to the face. The individual vector components can
  * be accessed as 'a', 'b', and 'c'.
  * @param distance The distance from the normal vector to the center of the rock block.
  * Accessed as 'd'.
  * @param phi The friction angle (phi) of the face.
  * @param cohesion The cohesion of the face.
  */
case class Face(normalVec: (Double,Double,Double), distance: Double,
                val phi: Double, val cohesion: Double) {
  val (a,b,c) = normalVec
  val d = distance
}

/** A rock block.
  *
  * @constructor Create a new rock block
  * @param center Cartesian coordinates for the center of the rock block. The individual
  * components can be accessed as 'centerX', 'centerY', and 'centerZ'.
  * @param faces: The faces that define the boundaries of the rock block.
  */
case class Block(center: (Double,Double,Double), val faces: List[Face]) {
  val (centerX, centerY, centerZ) = center

  def intersects(joint: Joint): Boolean = {
    val linProg = new LinearProgram(4)
    // Minimize s
    linProg.setObjFun(Array[Double](0.0, 0.0, 0.0, 1.0), LinearProgram.MIN)
    // Restrict our attention to plane of joint
    linProg.addConstraint(Array[Double](joint.a, joint.b, joint.c, 0.0),
                          LinearProgram.EQ, joint.d)
    // Require s to be within planes defined by faces of block
    faces.foreach { face => linProg.addConstraint(
        Array[Double](face.a, face.b, face.c, -1.0), LinearProgram.LE, face.d) }
    // Require s to be within planes defining shape of joint
    joint.globalCoordinates.foreach { case ((a,b,c),d) =>
        linProg.addConstraint(Array[Double](a, b, c, -1.0), LinearProgram.LE, d) }

    linProg.solve() match {
      case None => false
      case Some(s: Double) => s > 0
    }
  }

  def nonRedundantFaces: List[Face] =
    faces.filter { face =>
      val linProg = new LinearProgram(3)
      linProg.setObjFun(Array[Double](face.a, face.b, face.c), LinearProgram.MAX)
      faces.foreach { f => linProg.addConstraint(Array[Double](f.a, f.b, f.c),
                                                 LinearProgram.LE, f.d) }
      val s = linProg.solve().get
      s == face.d // TODO Add tolerance here
    }
}
