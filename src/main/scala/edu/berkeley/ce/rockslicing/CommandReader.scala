package edu.berkeley.ce.rockslicing

/** A simple tool for parsing command line inputs
  * @constructor Create a new command reader
  */
case class CommandReader(inputs: Array[String]) {
  assert(inputs.length >= 2)
  val inputFile = inputs(0)
  val numberSeedJoints = inputs(1).toInt
  val toInequalities = (inputs.length == 2) || (inputs contains "inequalities")
  val toVTK = inputs contains "toVTK"
}

