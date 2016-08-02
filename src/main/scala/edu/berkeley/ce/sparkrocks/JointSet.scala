package edu.berkeley.ce.sparkrocks

/**
  * Represents input joint set - allows for pattern matching to easily and clearly extract joint set fields
  *
  * @param strike Joint set strike (mean if stochastic)
  * @param dip Joint set dip (mean if stochastic)
  * @param jointSpacing Joint set spacing (mean if stochastic)
  * @param persistence Joint set persistence (mean if non-persistent)
  * @param phi Joint set friction angle
  * @param cohesion Joint set cohesion
  * @param strikeStDev Strike standard deviation
  * @param dipStDev Dip standard deviation
  * @param jointSpacingStDev Joint spacing standard deviation
  * @param persistenceStDev Persistence standard deviation
  */
case class JointSet(strike: Double, dip: Double, jointSpacing: Double, persistence: Double,
                    phi: Double, cohesion: Double, strikeStDev: Double = 0.0,
                    dipStDev: Double = 0.0, jointSpacingStDev: Double = 0.0,
                    persistenceStDev: Double = 0.0) {
  // Indicates whether joint set is deterministic or stochastic
  val isStochastic = strikeStDev != 0.0 || dipStDev != 0.0 || jointSpacingStDev != 0.0 || persistenceStDev != 0.0
}
