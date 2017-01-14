package edu.berkeley.ce.sparkrocks

/**
 * A simple wrapper for parsing command line inputs
 */
object CommandReader {
  case class Config(
    inputFile: String = "",
    numPartitions: Int = 25,
    minRadius: Double = 0.0,
    maxAspectRatio: Double = Double.PositiveInfinity,
    vtkOut: String = "",
    jsonOut: String = "",
    demOut: String = "",
    forcePartition: Boolean = false
  )

  private val parser = new scopt.OptionParser[Config]("SparkRocks") {
    head("SparkRocks", "1.0")

    opt[String]('i', "inputFile") required() action { (x, c) =>
      c.copy(inputFile = x)
    } text "File that contains input information on the rock volume of interest and joint sets"

    opt[Int]('n', "numPartitions") required() action { (x, c) =>
      c.copy(numPartitions = x)
    } validate { x =>
      if (x >= 1) success else failure("Number of partitions must be greater than or equal to 1")
    } text "Integer that specifies the minimum number of partitions that will be used in the analysis."+
           " Used to maintain balanced load across nodes"

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

    opt[String]("vtkOut") action { (x, c) =>
      c.copy(vtkOut = x)
    } text "Output file for block VTK data"

    opt[String]("jsonOut") action { (x, c) =>
      c.copy(jsonOut = x)
    } text "Output file for block JSON data"

    opt[String]("demOut") action { (x, c) =>
      c.copy(demOut = x)
    } text "Output file for block DEM data"

    opt[Unit]('f', "forcePartition") action { (_, c) =>
      c.copy(forcePartition = true)
    } text "Flag to force analysis to continue if specified number of partitions is not found"

    help("help") text "Prints this usage text"

    checkConfig { c =>
      if (c.vtkOut == "" && c.jsonOut == "" && c.demOut == "")
        failure("Must specify at least one of vtkOut or jsonOut") else success
    }
  }

  def parseArguments(args: Seq[String]): Option[Config] = parser.parse(args, Config())
}
