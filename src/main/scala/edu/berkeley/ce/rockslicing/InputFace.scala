package edu.berkeley.ce.rockslicing

/**
  * Represents bounding face of initial rock volume - allows for pattern matching to easily and clearly
  * extract face fields
  *
  * @param strike Strike of bounding face
  * @param dip Dip of bounding face
  * @param pointInPlane Point in plane that defines bounding face
  * @param phi Friction angle along bounding face
  * @param cohesion Cohesion along bounding face
  */
case class InputFace(strike: Double, dip: Double, pointInPlane: Array[Double], phi: Double, cohesion: Double) {
  assert(pointInPlane.length == 3)
}
