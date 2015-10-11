package edu.berkeley.ce.rockslicing

import scala.math.BigDecimal
/**
 * Numerical utilities for implementation and test cases.
 */
object NumericUtils {
  val EPSILON = 1.0e-6

  /**
   * Converts a value to 0.0 if it is arbitrarily close to it.
   * @param d A double value.
   * @return If the value is within EPSILON of 0.0, then 0.0. Otherwise, the
   *         value is returned unchanged.
   */
  def applyTolerance(d: Double): Double =
    if (math.abs(d) <= EPSILON) 0.0 else d

  /**
   * Checks if each double in a sequence is arbitrarily close to 0.0 and if so,
   * converts it to 0.0.
   * @param values A sequence of double values.
   * @return A new sequence of doubles where any element in the original sequence
   *         that is within ESPILON of 0.0 is converted to 0.0. All other elements
   *         of the sequence are unchanged.
   */
  def applyTolerance(values: Seq[Double]): Seq[Double] =
    values map applyTolerance

  /**
    * Rounds to the specified number of decimal places. If not specified, default
    * is 6 decimal places
    * @param d A Double value
    * @param decimals Number of decimal places to round to
    * @return Value with 6 decimal places
    */
  def roundToTolerance(d: Double, decimals: Option[Int]=None): Double = {
    decimals match {
      case None => BigDecimal(d).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble
      case Some(dec) => BigDecimal(d).setScale(dec, BigDecimal.RoundingMode.HALF_UP).toDouble
    }
  }
}
