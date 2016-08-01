package edu.berkeley.ce.rockslicing

import scala.io.Source
import scala.util.Try

/**
  * Processes inputs provided in user input file
  */
object InputProcessor {
  // Processes input file: Add rock volume faces and joints to respective input list
  /**
    * Processes input file to extract global origin, bounding box,
    * initial rock volume and joint sets.
    *
    * @param inputSource Input file containing user inputs
    * @return If input is properly formatted, returns a tuple containing an array representing
    *         the global origin, an array representing the bounding box, a Seq of arrays
    *         representing the bounding faces of the initial rock volume and a Seq of JointSets
    *         representing the joint sets present in the rock volume. If input is not properly
    *         formatted, it returns None.
    */
  def readInput(inputSource: Source): Option[ (Array[Double], (Array[Double], Array[Double]),
    Seq[InputFace], Seq[JointSet]) ] = {
    val lines = inputSource.getLines().zipWithIndex.toVector
    val globalOriginLine = lines(0)._1
    val boundingBoxLine = lines(1)._1

    val globalOriginTokens = globalOriginLine.split(" ") map { x => Try(x.toDouble) }
    val errIndex = globalOriginTokens.indexWhere(_.isFailure)
    if (errIndex != -1) {
      println(s"Error, Line 1, Token $errIndex: Invalid double found in definition of global origin")
      return None
    } else if (globalOriginTokens.length != 3) {
      println("Error, Line 1: Input file must begin with definition of global origin as 3 double values")
      return None
    }
    val globalOrigin = globalOriginTokens map(_.get)
    val globalOriginArr = Array(globalOrigin(0), globalOrigin(1), globalOrigin(2))

    val boundingBoxTokens = boundingBoxLine.split(" ") map { x => Try(x.toDouble) }
    val bbErrIndex = boundingBoxTokens.indexWhere(_.isFailure)
    if (bbErrIndex != -1) {
      println(s"Error, Line 2, Token $bbErrIndex: Invalid double found in definition of bounding box")
      return None
    } else if (boundingBoxTokens.length !=  6) {
      println("Error, Line 2: Input file must specify domain as bounding box with 6 double values")
      return None
    }
    val boundingBox = boundingBoxTokens map(_.get)
    val boundingBoxArr = (Array(boundingBox(0), boundingBox(1), boundingBox(2)),
      Array(boundingBox(3), boundingBox(4), boundingBox(5)))

    val remainingLines = lines.drop(2)
    val transitionIndex = remainingLines.indexWhere(_._1.isEmpty)
    if (transitionIndex == -1) {
      println("Error: Input file must contain empty line to mark transition from rock volume to joints")
      return None
    }

    val rockVolumeData = remainingLines.take(transitionIndex)
    val jointData = remainingLines.drop(transitionIndex + 1)

    val rockVolume = rockVolumeData.map { case (line, index) =>
      val tokens = line.split(" ") map { x => Try(x.toDouble) }
      val errIndex = tokens.indexWhere(_.isFailure)
      if (errIndex != -1) {
        println(s"Error, Line $index, Token $errIndex: Invalid double found in definition of rock volume face")
        return None
      }
      val doubleVals = tokens map(_.get)
      if (doubleVals.length != 7) {
        println(s"Error, Line $index: Each face of input rock volume is defined by values: strike, dip, x, y, z,"+
        " phi and cohesion")
        return None
      }

      // These values will be passed to JointGenerator to find joints corresponding to rock volume
      InputFace(doubleVals(0), doubleVals(1), Array(doubleVals(2), doubleVals(3), doubleVals(4)),
        doubleVals(5), doubleVals(6))
    }

    val joints = jointData.map { case (line, index) =>
      val tokens = line.split(" ") map { x => Try(x.toDouble) }
      val errIndex = tokens.indexWhere(_.isFailure)
      if (errIndex != -1) {
        println(s"Error, Line $index, Token $errIndex: Invalid double found in definition of joint")
        return None
      }
      val doubleVals = tokens map(_.get)
      if (doubleVals.length < 6) {
        println(s"""Error, Line $index: Each input joint set is defined by at least 6 values:
                        Strike
                        Dip
                        Joint Spacing
                        Persistence (Input as percentage, 100 for persistent joints)
                        Phi
                        Cohesion
                        Optional input: Standard deviation of 4 geometric parameters. Standard deviation
                        should not be specified for persistence when set to 100%
                """
        )
        return None
      } else if (doubleVals.length != 6 && doubleVals.length != 9 && doubleVals.length != 10) {
        println(s"Error, Line $index: If specifying distributions for joints, they must be specified for strike, "+
          "dip, spacing and, if relevant, persistence.")
        return None
      }

      if (doubleVals.length == 6) {
        JointSet(doubleVals(0), doubleVals(1), doubleVals(2), doubleVals(3), doubleVals(4), doubleVals(5))
      } else if (doubleVals.length == 9) {
        JointSet(doubleVals(0), doubleVals(1), doubleVals(2), doubleVals(3), doubleVals(4), doubleVals(5),
          doubleVals(6), doubleVals(7), doubleVals(8))
      } else {
        JointSet(doubleVals(0), doubleVals(1), doubleVals(2), doubleVals(3), doubleVals(4), doubleVals(5),
          doubleVals(6), doubleVals(7), doubleVals(8), doubleVals(9))
      }
    }

    Some((globalOriginArr, boundingBoxArr, rockVolume, joints))
  }
}