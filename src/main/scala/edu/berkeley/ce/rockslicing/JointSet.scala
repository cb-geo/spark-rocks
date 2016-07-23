package edu.berkeley.ce.rockslicing

/**
  * Represents input joint set - allows for pattern matching to easily and clearly extract joint set fields
  *
  * @param parameters Array containing the input data representing the joint set. The inputs in each array
  *                   are strike, dip, joint spacing, persistence, phi, cohesion and optional stochastic parameters
  */
case class JointSet(parameters: Array[Double]) {
  val strike = parameters(0)
  val dip = parameters(1)
  val jointSpacing = parameters(2)
  val persistence = parameters(3)
  val phi = parameters(4)
  val cohesion = parameters(5)

  // Indicates whether joint set is deterministic or stochastic
  val stochasticFlag = if (parameters.length > 6) {
    true
  } else {
    false
  }

  // Optional stochastic parameters
  val strikeStDev = if (parameters.length > 6) {
    parameters(6)
  } else {
    0.0
  }
  val dipStDev = if (parameters.length > 6) {
    parameters(7)
  } else {
    0.0
  }
  val jointSpacingStDev = if (parameters.length > 6) {
    parameters(8)
  } else {
    0.0
  }
  val persistenceStDev = if (parameters.length == 10) {
    parameters(9)
  } else {
    0.0
  }
}
