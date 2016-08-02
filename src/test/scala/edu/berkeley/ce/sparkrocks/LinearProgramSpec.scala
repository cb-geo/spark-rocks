package edu.berkeley.ce.sparkrocks

import org.scalatest._

class LinearProgramSpec extends FunSuite {
  test("Maximizing x subject to x = 5 should produce result 5") {
    val solver = new LinearProgram(1)
    solver.setObjFun(Array(1.0), LinearProgram.MAX)
    solver.addConstraint(Array(1.0), LinearProgram.EQ, 5.0)

    val (varSettings, opt) = solver.solve().get
    assert(varSettings.length == 1)
    assert(math.abs(varSettings(0) - 5.0) <= NumericUtils.EPSILON)
    assert(math.abs(opt - 5.0) <= NumericUtils.EPSILON)
  }

  test("Minimizing x subject to x = 5 should produce result 5") {
    val solver = new LinearProgram(1)
    solver.setObjFun(Array(1.0), LinearProgram.MIN)
    solver.addConstraint(Array(1.0), LinearProgram.EQ, 5.0)

    val (varSettings, opt) = solver.solve().get
    assert(varSettings.length == 1)
    assert(math.abs(varSettings(0) - 5.0) <= NumericUtils.EPSILON)
    assert(math.abs(opt - 5.0) <= NumericUtils.EPSILON)
  }

  test("Maximizing x + y subject to x <= 5 and y <= 4 should produce result 9") {
    val solver = new LinearProgram(2)
    solver.setObjFun(Array(1.0, 1.0), LinearProgram.MAX)
    solver.addConstraint(Array(1.0, 0.0), LinearProgram.LE, 5.0)
    solver.addConstraint(Array(0.0, 1.0), LinearProgram.LE, 4.0)

    val (varSettings, opt) = solver.solve().get
    assert(varSettings.length == 2)
    assert(math.abs(varSettings(0) - 5.0) <= NumericUtils.EPSILON)
    assert(math.abs(varSettings(1) - 4.0) <= NumericUtils.EPSILON)
    assert(math.abs(opt - 9.0) <= NumericUtils.EPSILON)
  }

  test("Minimizing x + y subject to x >= 5 and y >= 4 should produce result 9") {
    val solver = new LinearProgram(2)
    solver.setObjFun(Array(1.0, 1.0), LinearProgram.MIN)
    solver.addConstraint(Array(1.0, 0.0), LinearProgram.GE, 5.0)
    solver.addConstraint(Array(0.0, 1.0), LinearProgram.GE, 4.0)

    val (varSettings, opt) = solver.solve().get
    assert(varSettings.length == 2)
    assert(math.abs(varSettings(0) - 5.0) <= NumericUtils.EPSILON)
    assert(math.abs(varSettings(1) - 4.0) <= NumericUtils.EPSILON)
    assert(math.abs(opt - 9.0) <= NumericUtils.EPSILON)
  }

  test("Minimizing x - y subject to 5 <= x <= 6, 7 <= y <= 11, should produce result -6") {
    val solver = new LinearProgram(2)
    solver.setObjFun(Array(1.0, -1.0), LinearProgram.MIN)
    solver.addConstraint(Array(1.0, 0.0), LinearProgram.LE, 6.0)
    solver.addConstraint(Array(0.0, 1.0), LinearProgram.GE, 7.0)
    solver.addConstraint(Array(1.0, 0.0), LinearProgram.GE, 5.0)
    solver.addConstraint(Array(0.0, 1.0), LinearProgram.LE, 11.0)

    val (varSettings, opt) = solver.solve().get
    assert(varSettings.length == 2)
    assert(math.abs(varSettings(0) - 5.0) <= NumericUtils.EPSILON)
    assert(math.abs(varSettings(1) - 11.0) <= NumericUtils.EPSILON)
    assert(math.abs(opt + 6.0) <= NumericUtils.EPSILON)
  }

  test("Maximizing sum of coordinates in unit cube should produce result 3") {
    val solver = new LinearProgram(3)
    solver.setObjFun(Array(1.0, 1.0, 1.0), LinearProgram.MAX)
    solver.addConstraint(Array(1.0, 0.0, 0.0), LinearProgram.GE, 0.0)
    solver.addConstraint(Array(0.0, 1.0, 0.0), LinearProgram.GE, 0.0)
    solver.addConstraint(Array(0.0, 0.0, 1.0), LinearProgram.GE, 0.0)
    solver.addConstraint(Array(1.0, 0.0, 0.0), LinearProgram.LE, 1.0)
    solver.addConstraint(Array(0.0, 1.0, 0.0), LinearProgram.LE, 1.0)
    solver.addConstraint(Array(0.0, 0.0, 1.0), LinearProgram.LE, 1.0)

    val (varSettings, opt) = solver.solve().get
    assert(varSettings.length == 3)
    assert(math.abs(varSettings(0) - 1.0) <= NumericUtils.EPSILON)
    assert(math.abs(varSettings(1) - 1.0) <= NumericUtils.EPSILON)
    assert(math.abs(varSettings(2) - 1.0) <= NumericUtils.EPSILON)
    assert(math.abs(opt - 3.0) <= NumericUtils.EPSILON)
  }

  test("Minimum z coordinate within plane -x + z = 1 within 2-cube should be 1") {
    val solver = new LinearProgram(3)
    solver.setObjFun(Array(0.0, 0.0, 1.0), LinearProgram.MIN)
    solver.addConstraint(Array(1.0, 0.0, 0.0), LinearProgram.GE, 0.0)
    solver.addConstraint(Array(0.0, 1.0, 0.0), LinearProgram.GE, 0.0)
    solver.addConstraint(Array(0.0, 0.0, 1.0), LinearProgram.GE, 0.0)
    solver.addConstraint(Array(1.0, 0.0, 0.0), LinearProgram.LE, 2.0)
    solver.addConstraint(Array(0.0, 1.0, 0.0), LinearProgram.LE, 2.0)
    solver.addConstraint(Array(0.0, 0.0, 1.0), LinearProgram.LE, 2.0)
    solver.addConstraint(Array(-1.0, 0.0, 1.0), LinearProgram.EQ, 1.0)

    val (varSettings, _) = solver.solve().get
    assert(varSettings.length == 3)
    assert(math.abs(varSettings(0)) <= NumericUtils.EPSILON)
    assert(math.abs(varSettings(1)) <= NumericUtils.EPSILON)
    assert(math.abs(varSettings(2) - 1.0) <= NumericUtils.EPSILON)
  }
}