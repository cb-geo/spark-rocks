package edu.berkeley.ce.rockslicing

/** A simple tool for parsing command line inputs
  * @constructor Create a new command reader
  */
case class commandReader(inputs: Array[String]) {
  assert(inputs.length >= 3)
  val inputFile = inputs(0)
  val numberSeedJoints = inputs(1).toInt
  val toInequalities = inputs contains "inequalities"
  val toVertices = inputs contains "vertices"
  val toVTK = inputs contains "toVTK"
}

