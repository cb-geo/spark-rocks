package edu.berkeley.ce.rockslicing

/** A simple tool for parsing command line inputs
  * @constructor Create a new command reader
  */
object CommandReader {
  val usage =
    """
       Usage: <spark arguments> sparkRocks-assembly-1.0.jar -inputFile <inputFile> -numberSeedJoints <Int> <output opts>
         where:
              <spark arguments>    See www.spark.apache.org for details on how to submit Spark applications
              <inputFile>          File that contains input information on the rock volume of interest and joint sets
              <Int>                Integer that specifies the number of joints to process before initiating parallel
                                   instances
         and <output opts> is one or all of the following
              -toVTK               Generate output that can be converted to VTK format by rockProcessor.
                                   Output file format is JSON.
              -toIE                Generate output that uses inequalities to represent rock blocks along with the
                                   blocks' center of mass. Output file format is JSON.
    """.stripMargin

  type OptionMap = Map[Symbol, String]

  def parseArguments(inputs: Array[String]): OptionMap = {
    if (inputs.length == 0) {
      println(usage)
      sys.exit(1)
    }

    inputs.toList match {
      case "-inputFile" :: file :: "-numberSeedJoints" :: seeds :: Seq("-toVTK") => 
        Map('inputFile -> file, 'numberSeedJoints -> seeds, 'toIE -> "false", 'toVTK -> "true")
      case "-inputFile" :: file :: "-numberSeedJoints" :: seeds :: Seq("-toIE") => 
        Map('inputFile -> file, 'numberSeedJoints -> seeds, 'toIE -> "true", 'toVTK -> "false")
      case "-inputFile" :: file :: "-numberSeedJoints" :: seeds :: "-toIE" :: Seq("-toVTK") => 
        Map('inputFile -> file, 'numberSeedJoints -> seeds, 'toIE -> "true", 'toVTK -> "true")
      case "-inputFile" :: file :: "-numberSeedJoints" :: seeds :: "-toVTK" :: Seq("-toIE") => 
        Map('inputFile -> file, 'numberSeedJoints -> seeds, 'toIE -> "true", 'toVTK -> "true")
      case "-inputFile" :: file :: Nil =>
        println("ERROR: Number of seed joints not specified")
        sys.exit(1)
      case "-inputFile" :: file :: "-numberSeedJoints" :: seeds :: Nil  =>
        println("ERROR: Please specify the desired output format(s)")
        sys.exit(1)
      case _ =>
        println("ERROR: Please verify you are specifying command line inputs in correct format")
        sys.exit(1)
    }
  }
}
