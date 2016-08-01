package edu.berkeley.ce.rockslicing

import org.apache.commons.lang3.builder.HashCodeBuilder

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

  override def equals(obj: Any): Boolean = {
    obj match {
      case b: InputFace =>
        this.strike == b.strike && this.dip == b.dip &&
          (this.pointInPlane sameElements b.pointInPlane) &&
          this.phi == b.phi && this.cohesion == b.cohesion
      case _ => false
    }
  }

  override def hashCode: Int = {
    new HashCodeBuilder()
      .append(strike)
      .append(dip)
      .append(pointInPlane)
      .append(phi)
      .append(cohesion)
      .toHashCode
  }
}
