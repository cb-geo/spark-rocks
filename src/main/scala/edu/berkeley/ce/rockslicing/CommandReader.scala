package edu.berkeley.ce.rockslicing

/**
 * A simple wrapper for parsing command line inputs
 */
object CommandReader {
  case class Config(
    inputFile: String = "",
    numSeedJoints: Int = 25,
    minRadius: Double = 0.0,
    maxAspectRatio: Double = Double.PositiveInfinity,
    toVTK: Boolean = false,
    toInequalities: Boolean = false,
    forceBalancer: Boolean = false
  )

  private val parser = new scopt.OptionParser[Config]("SparkRocks") {
    head("SparkRocks", "1.0")

    opt[String]('i', "inputFile") required() action { (x, c) =>
      c.copy(inputFile = x)
    } text "File that contains input information on the rock volume of interest and joint sets"

    opt[Int]('n', "numSeedJoints") required() action { (x, c) =>
      c.copy(numSeedJoints = x)
    } validate { x =>
      if (x >= 0) success else failure("Number of seed joints must be greater than or equal to 0")
    } text "Integer that specifies the number of joints to process before initiating parallel processing"

    opt[Double]("minRadius") action { (x, c) =>
      c.copy(minRadius = x)
    } validate { x =>
      if (x > 0.0) success else failure("Minimum inscribable radius must be positive")
    } text "The smallest acceptable maximum radius of a sphere inscribable inside child blocks"

    opt[Double]("maxAspectRatio") action { (x, c) =>
      c.copy(maxAspectRatio = x)
    } validate { x =>
      if (x > 0.0) success else failure("Maximum aspect ratio must be positive")
    } text "The maximum acceptable aspect ratio of child blocks"

    opt[Unit]("toVTK") action { (_, c) =>
      c.copy(toVTK = true)
    } text "Generate output that can be converted to VTK format by rockProcessor"

    opt[Unit]("toIE") action { (_, c) =>
      c.copy(toInequalities = true)
    } text "Generate JSON output that uses inequalities to represent rock blocks along with the blocks' centroids"

    opt[Unit]("f") action { (_, c) =>
      c.copy(forceBalancer = true)
    } text "Force the load balancer to provide seed joints regardless of load imbalance"

    help("help") text "Prints this usage text"

    checkConfig { c =>
      if (!c.toVTK && !c.toInequalities) failure("Must specify at least one of toVTK or toIE") else success
    }
  }

  def parseArguments(args: Seq[String]): Option[Config] = parser.parse(args, Config())
}
