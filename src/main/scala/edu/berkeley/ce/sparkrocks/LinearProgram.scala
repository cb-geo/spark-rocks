package edu.berkeley.ce.sparkrocks

import org.apache.commons.math3.exception.TooManyIterationsException
import org.apache.commons.math3.optim.linear._
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType

import scala.collection.JavaConverters._
import scala.collection.mutable

object LinearProgram {
  // Scala's enumerations suck, so we have to do this
  sealed trait Operator
  case object LE extends Operator
  case object EQ extends Operator
  case object GE extends Operator

  sealed trait ObjectiveType
  case object MIN extends ObjectiveType
  case object MAX extends ObjectiveType
}

/**
  * A linear program. Maximizes or minimizes the function <c,x> subject to the
  * constraint Ax {<.=,>} b, where b, c, and x are vectors and A is a matrix.
  *
  * @constructor Initialize a new linear program.
  * @param numVars The number of variables in the linear program.
  */
class LinearProgram(numVars: Int) {
  val constraints = mutable.ArrayBuffer.empty[LinearConstraint]
  var objectiveFunction: Option[LinearObjectiveFunction] = None
  var goalType: Option[GoalType] = None

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
    objectiveFunction = Some(new LinearObjectiveFunction(sanitizedCoeffs, 0))
    goalType = objType match {
      case LinearProgram.MIN => Some(GoalType.MINIMIZE)
      case LinearProgram.MAX => Some(GoalType.MAXIMIZE)
    }
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
    val rel = operator match {
      case LinearProgram.LE => Relationship.LEQ
      case LinearProgram.EQ => Relationship.EQ
      case LinearProgram.GE => Relationship.GEQ
    }

    constraints.append(new LinearConstraint(sanitizedCoeffs, rel, rhs))
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
      val solver = new SimplexSolver()
      if (objectiveFunction.isEmpty || constraints.isEmpty) {
        throw new IllegalStateException("Must specify both objective and constraints for LP")
      }

      val soln = solver.optimize(objectiveFunction.get, new LinearConstraintSet(constraints.asJava), goalType.get)
      Some((soln.getFirst, soln.getSecond))
    } catch {
      case _: TooManyIterationsException => None
    }
  }
}
