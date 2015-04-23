package edu.berkeley.ce.rockslicing.types

/** A simple data structure to represent a discontinuity.
  *
  * @constructor Create a new discontinuity.
  * @param normalVec The normal vector to the discontinuity. The individual vector components
  * can be accessed as 'a', 'b', and 'c'.
  * @param center Cartesian coordinates for the center of the discontinuity. The individual
           components can be accessed as 'centerX', 'centerY', and 'centerZ'.
  * @param distance The distance of the discontinuity from the local center, accessed as 'd'.
  * @param phi The discontinuity's friction angle (phi).
  * @param cohesion The cohesion along the discontinuity
  * @param shape A list of lines specifying the shape of the discontinuity. Each item is a
  * 3-tuple. The first two items specify the line, while the last gives the distance
  * of the line from the discontinuity's center in the local coordinate system.
 */
case class Discontinuity(normalVec: (Double, Double, Double), center: (Double, Double, Double),
                         val dipAngle: Double, val dipDirection: Double, distance: Double,
                         val phi: Double, val cohesion: Double,
                         val shape: List[(Double, Double, Double)]) {
  val (a, b, c) = normalVec
  val (centerX, centerY, centerZ) = center
  val d = distance
}

/** A simple data structure to represent the face of a rock block.
  *
  * @constructor Create a new rock face.
  * @param normalVec The normal vector to the face. The individual vector components can
  * be accessed as 'a', 'b', and 'c'.
  * @param distance The distance from the normal vector to the center of the rock block.
  * Accessed as 'd'.
  * @param phi The friction angle (phi) of the face.
  * @cohesion: The cohesion of the face.
  */
case class Face(normalVec: (Double,Double,Double), distance: Double,
                val phi: Double, val cohesion: Double) {
  val (a,b,c) = normalVec
  val d = distance
}

/** A simple data structure to represent a rock block.
  *
  * @constructor Create a new rock block
  * @param center Cartesian coordinates for the center of the rock block. The individual
  * components can be accessed as 'centerX', 'centerY', and 'centerZ'.
  * @param faces: The faces that define the boundaries of the rock block.
  */
case class Block(center: (Double,Double,Double), val faces: List[Face]) {
  val (centerX, centerY, centerZ) = center
}
