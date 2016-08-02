package edu.berkeley.ce.sparkrocks

import lpsolve._

object LinearProgram {
  // Scala's enumerations suck, so we have to do this
  sealed trait Operator { def value: String }
  case object LE extends Operator { val value = "<=" }
  case object EQ extends Operator { val value = "==" }
  case object GE extends Operator { val value = ">=" }

  sealed trait ObjectiveType { def value: String }
  case object MIN extends ObjectiveType { val value = "Minimize" }
  case object MAX extends ObjectiveType { val value = "Maximize" }
}

/**
  * A linear program. Maximizes or minimizes the function <c,x> subject to the
  * constraint Ax {<.=,>} b, where b, c, and x are vectors and A is a matrix.
  *
  * @constructor Initialize a new linear program.
  * @param numVars The number of variables in the linear program.
  */
class LinearProgram(val numVars: Int) {
  val solver = LpSolve.makeLp(0, numVars)
  solver.setVerbose(LpSolve.IMPORTANT)

  private def sanitizeCoefficients(coeffs: Array[Double]): Array[Double] =
    if (coeffs.length > numVars) {
      coeffs.slice(0, numVars)
    } else if (coeffs.length < numVars) {
      coeffs.padTo(numVars, 0.0)
    } else {
      coeffs
    }

  /**
    * Set the objective function of the linear program.
    * @param coeffs The coefficients of the variables in the objective function,
    * i.e. the entries of c. If the number of entries exceeds the number of
    * variables, the extra entries are ignored. If the number of entries is less
    * than the number of variables, coefficients of 0.0 are added by default.
    * @param objType One of either MIN (to minimize) or MAX (to maximize).
    */
  def setObjFun(coeffs: Array[Double], objType: LinearProgram.ObjectiveType): Unit = {
    val sanitizedCoeffs = sanitizeCoefficients(coeffs)
    solver.strSetObjFn(sanitizedCoeffs.mkString(" "))
    objType match {
      case LinearProgram.MIN => solver.setMinim()
      case LinearProgram.MAX => solver.setMaxim()
    }

     // Variables are always unbounded for our purposes. Users can always
     // express variable bounds as constraints with one coefficient anyways.
    (1 to numVars) foreach solver.setUnbounded
  }

  /**
    * Add a new constraint to the linear program.
    * @param coeffs The coefficients of the variables for the constraint.
    * If the number of entries exceeds the number of variables in the LP, the
    * extra entries are ignored. If the number of entries is less than the
    * number of variables, coefficients of 0.0 are added by default.
    * @param operator One of LE, for <=, EQ for ==, or GE for =>.
    * @param rhs The right-hand side of the constraint.
    */
  def addConstraint(coeffs: Array[Double], operator: LinearProgram.Operator, rhs: Double): Unit = {
    val sanitizedCoeffs = sanitizeCoefficients(coeffs)
    val op = operator match {
      case LinearProgram.LE => LpSolve.LE
      case LinearProgram.EQ => LpSolve.EQ
      case LinearProgram.GE => LpSolve.GE
    }

    solver.strAddConstraint(sanitizedCoeffs.mkString(" "), op, rhs)
  }

  /**
    * Solve the linear program to compute the optimal objective value.
    * Note that this instance becomes unuseable after this method completes.
    * @return None if an error occurred, otherwise Some((varSettings, opt)),
    * where varSettings is the value assigned to each value to optimize the
    * objective function and opt is the optimal objective value itself.
    */
  def solve(): Option[(Array[Double], Double)] = {
    try {
      solver.solve()
      val objectiveValue = solver.getObjective
      val variableSettings = solver.getPtrVariables
      Some((variableSettings, objectiveValue))
    } catch {
      case _: LpSolveException => None
    } finally {
      solver.deleteLp()
    }
  }
}
