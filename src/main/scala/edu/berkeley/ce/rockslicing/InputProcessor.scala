package edu.berkeley.ce.rockslicing

import breeze.linalg.DenseVector

import scala.io.Source
import scala.util.Try

object InputProcessor {
  // Processes input file: Add rock volume faces and joints to respective input list
  def readInput(inputSource: Source): Option[((Double, Double, Double),
                                             Seq[Face], Seq[Joint])] = {
    val lines = inputSource.getLines().zipWithIndex.toVector
    val globalOriginLine = lines.head._1

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
    val globalOriginTuple = (globalOrigin(0), globalOrigin(1), globalOrigin(2))

    // val allOtherLines = lines.tail
    // val boundingBoxLine = allOtherLines.head._1
    // val boundingBoxTokens = boundingBoxLine.split(" ") map { x => Try(x.toDouble) }
    // val errIndexBoundingBox = boundingBoxTokens.indexWhere(_.isFailure)
    // if (errIndexBoundingBox != -1) {
    //   println(s"Error, Line 2, Token $errIndexBoundingBox: "+
    //           "Invalid double found in definition of bounding box")
    //   return None
    // } else if (boundingBoxTokens.length != 6) {
    //   println("Error, Line 2: Input file must have definition of bounding box as 6 double values")
    //   return None
    // }
    // val boundingBox = boundingBoxTokens map(_.get)
    // val boundingBoxTuple = (boundingBox(0), boundingBox(1), boundingBox(2),
    //                         boundingBox(3), boundingBox(4), boundingBox(5))

    // val dataLines = allOtherLines.tail
    // val transitionLine = dataLines.head._1
    // if (!transitionLine.isEmpty) {
    //   println("Error: Input file must contain an empty line to mark transistion from global origin"+
    //           " and bounding box definitions to joints")
    //   return None
    // }

    val remainingLines = lines.tail
    val transitionIndex = remainingLines.indexWhere(_._1 == "%")
    if (transitionIndex == -1) {
      println("Error: Input file must contain \"%\" to mark transition from rock volume to joints")
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
      if (doubleVals.length != 6) {
        println(s"Error, Line $index: Each face of input rock volume is defined by values: a, b, c, d, phi and cohesion")
        return None
      }

      val a = doubleVals(0)
      val b = doubleVals(1)
      val c = doubleVals(2)
      val d = doubleVals(3)
      val phi = doubleVals(4)
      val cohesion = doubleVals(5)

      // Ensure that the normal vector is also a unit vector
      val normVec = DenseVector[Double](a, b, c)
      val unitNormVec = breeze.linalg.normalize(normVec)
      val aPrime = unitNormVec(0)
      val bPrime = unitNormVec(1)
      val cPrime = unitNormVec(2)

      // Eliminate any inward-pointing normals, and round extremely small distances to 0
      if (math.abs(d) >= NumericUtils.EPSILON) {
        Face((aPrime, bPrime, cPrime), d, phi, cohesion)
      } else if (d < -NumericUtils.EPSILON) {
        Face((-aPrime, -bPrime, -cPrime), -d, phi, cohesion)
      } else {
        Face((aPrime, bPrime, cPrime), 0, phi, cohesion)
      }
    }

    val joints = jointData.map { case (line, index) =>
      val tokens = line.split(" ") map { x => Try(x.toDouble) }
      val errIndex = tokens.indexWhere(_.isFailure)
      if (errIndex != -1) {
        println(s"Error, Line $index, Token $errIndex: Invalid double found in definition of joint")
        return None
      }
      val doubleVals = tokens map(_.get)
      if (doubleVals.length < 11) {
        println(s"""Error, Line $index: Each input joint is defined by at least 11 values:
                        3 For Normal Vector
                        3 For Local Origin
                        3 For Joint Center
                        Phi
                        Cohesion
                        4 For Each Bounding Face (Optional)
                """
        )
        return None
      }

      val (mandatoryValues, optionalValues) = doubleVals.splitAt(11)
      if (optionalValues.length % 4 != 0) {
        println(s"Error, Line $index: Bounding Faces for Block Require 4 Values Each")
        return None
      }

      // Ensure that the normal vector is also a unit vector
      val normVec = DenseVector[Double](mandatoryValues(0), mandatoryValues(1), mandatoryValues(2))
      val unitNormVec = breeze.linalg.normalize(normVec)

      val localOrigin = (mandatoryValues(3), mandatoryValues(4), mandatoryValues(5))
      val center = (mandatoryValues(6), mandatoryValues(7), mandatoryValues(8))
      val phi = mandatoryValues(9)
      val cohesion = mandatoryValues(10)

      val shape = optionalValues grouped(4) map { group =>
        // Again, we want to be sure the normal vector is also a unit vector
        val normVec = DenseVector[Double](group(0), group(1), group(2))
        val unitNormVec = breeze.linalg.normalize(normVec)
        val d = group(3)
        ((unitNormVec(0), unitNormVec(1), unitNormVec(2)), d)
      }

      Joint((unitNormVec(0), unitNormVec(1), unitNormVec(2)), localOrigin, center, phi, cohesion, shape.toSeq)
    }

    Some((globalOriginTuple, rockVolume, joints))
  }
}
