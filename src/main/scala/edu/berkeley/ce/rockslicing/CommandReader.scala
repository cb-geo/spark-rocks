package edu.berkeley.ce.rockslicing

/** A simple tool for parsing command line inputs based on the following:
  * http://stackoverflow.com/questions/2315912/scala-best-way-to-parse-command-line-parameters-cli
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
              -toVTK               Generate output that can be converted to VTK format by rockProcessor
              -toIE                Generate output that uses inequalities to represent rock blocks along with the
                                   blocks' center of mass
    """.stripMargin

  type OptionMap = Map[Symbol, String]

  def parseArguments(inputs: Array[String]): OptionMap = {
    if (inputs.length == 0) {
      println(usage)
      sys.exit(0)
    }
    val inputList = inputs.toList

    def nextOption(inputMap: OptionMap, inputsString: List[String]): OptionMap = {
      inputsString match {
        case Nil => inputMap
        case "-inputFile" :: file :: Nil =>
          println("ERROR: Number of seed joints not specified")
          sys.exit(1)
        case "-inputFile" :: file :: tail =>
          nextOption(inputMap ++ Map('inputFile -> file), tail)
        case "-numberSeedJoints" :: seeds :: Nil =>
          println("ERROR: Please specify the desired output format(s)")
          sys.exit(1)
        case "-numberSeedJoints" :: seeds :: tail =>
          nextOption(inputMap ++ Map('numberSeedJoints -> seeds), tail)
        case "-toVTK" :: tail =>
          nextOption(inputMap ++ Map('toVTK -> "true"), tail)
        case "-toIE" :: tail =>
          nextOption(inputMap ++ Map('toIE -> "true"), tail)
        case option :: tail =>
          println("ERROR: Unknown option "+option)
          sys.exit(1)
      }
    }
    nextOption(Map(), inputList)
  }
}
