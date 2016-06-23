package edu.berkeley.ce.rockslicing

import scala.io.Source
import scala.util.Try

object InputProcessor {
  // Processes input file: Add rock volume faces and joints to respective input list
  def readInput(inputSource: Source): Option[(Array[Double], Array[Double],
                                             Seq[Array[Double]], Seq[Array[Double]])] = {
    val lines = inputSource.getLines().zipWithIndex.toVector
    val globalOriginLine = lines.head._1
    val boundingBoxLine = lines.tail.head._1

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

    val boundingBoxTokens = boundingBoxLine.split(" ") map {x => Try(x.toDouble) }
    val bbErrIndex = boundingBoxTokens.indexWhere(_.isFailure)
    if (bbErrIndex != -1) {
      println(s"Error, Line 2, Token $errIndex: Invalid double found in definition of bounding box")
      return None
    } else if (boundingBoxTokens.length !=  6) {
      println("Error, Line 2: Input file must specify domain as bounding box with 6 double values")
      return None
    }
    val boundingBox = boundingBoxTokens map(_.get)
    val boundingBoxArr = Array(boundingBox(0), boundingBox(1), boundingBox(2),
                               boundingBox(3), boundingBox(4), boundingBox(5))

    val remainingLines = lines.tail.tail
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
      doubleVals
    }

    val joints = jointData.map { case (line, index) =>
      val tokens = line.split(" ") map { x => Try(x.toDouble) }
      val errIndex = tokens.indexWhere(_.isFailure)
      if (errIndex != -1) {
        println(s"Error, Line $index, Token $errIndex: Invalid double found in definition of joint")
        return None
      }
      val doubleVals = tokens map(_.get)
      if (doubleVals.length < 4) {
        println(s"""Error, Line $index: Each input joint is defined by at least 4 values:
                        Strike
                        Dip
                        Joint Spacing
                        Persistence (0 for persistent, 1 for non-persistent)
                        Mean and standard deviation of 4 primary parameters (Optional)
                """
        )
        return None
      }

      val optionalValues = doubleVals.drop(4)
      if (optionalValues.length != 0 && (optionalValues.length != 6 || optionalValues.length != 8)) {
        println(s"Error, Line $index: If specifying distributions for joints, they must be specified for all parameters")
        return None
      }

      doubleVals
    }

    Some((globalOriginArr, boundingBoxArr, rockVolume, joints))
  }
}
