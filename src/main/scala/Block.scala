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
  */
case class Block(center: (Double,Double,Double), val faces: List[Face]) {
  val (centerX, centerY, centerZ) = center

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
}
