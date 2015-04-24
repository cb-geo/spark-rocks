package edu.berkeley.ce.rockslicing

import lpsolve._

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
    val solver = LpSolve.makeLp(0, 4) // 0 constraints (so far), 4 Variables: x, y, z, and s
    solver.setMinim()
    solver.setObjFn(Array[Double](0.0, 0.0, 0.0, 1.0)) // Minimize s
    (1 to 4).foreach {solver.setUnbounded(_)} // All variables are free

    // Restrict our attention to plane of joint
    solver.addConstraint(Array[Double](joint.a, joint.b, joint.c, 0.0),
                         LpSolve.EQ, joint.d)
    // Require s to be within planes defined by faces of block
    faces.foreach {face => solver.addConstraint(
        Array[Double](face.a, face.b, face.c, -1.0), LpSolve.LE, face.d)}
    // Require s to be within planes defining shape of joint
    joint.globalCoordinates.foreach {case ((a,b,c),d) =>
        solver.addConstraint(Array[Double](a, b, c, -1.0), LpSolve.LE, d)}
    solver.solve()
    val s = solver.getObjective()
    solver.deleteLp()
    s < 0
  }
}
