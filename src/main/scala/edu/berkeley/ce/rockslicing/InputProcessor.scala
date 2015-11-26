package edu.berkeley.ce.rockslicing

import breeze.linalg.DenseVector

import scala.io.Source
import scala.util.Try

object InputProcessor {
  // Processes input file: Add rock volume faces and joints to respective input list
  def readInput(inputSource: Source): Option[(Seq[Face], Seq[Joint])] = {
    val lines = inputSource.getLines().zipWithIndex.toVector
    val transitionIndex = lines.indexWhere(_._1 == "%")
    if (transitionIndex == -1) {
      println("Error: Input file must contain \"%\" to mark transition from rock volume to joints")
      return None
    }

    val rockVolumeData = lines.take(transitionIndex)
    val jointData = lines.drop(transitionIndex + 1)

    val rockVolume = rockVolumeData.map { case (line, index) =>
      // Attempt to convert each token to a Double, ignoring those that fail
      val tokens = line.split(" ") map { x => Try(x.toDouble) } filter(_.isSuccess)
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

      // Eliminate any inward-pointing normals
      if (d >= 0.0) {
        Face((aPrime, bPrime, cPrime), d, phi, cohesion)
      } else {
        Face((-aPrime, -bPrime, -cPrime), -d, phi, cohesion)
      }
    }

    val joints = jointData.map { case (line, index) =>
      // Again, convert each token to Double and ignore failing tokens
      val tokens = line.split(" ") map { x => Try(x.toDouble) } filter (_.isSuccess)
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
        // These don't have to be unit vectors, so we leave them alone
        val normalVec = (group(0), group(1), group(2))
        val d = group(3)
        (normalVec, d)
      }

      Joint((unitNormVec(0), unitNormVec(1), unitNormVec(2)), localOrigin, center, phi, cohesion, shape.toSeq)
    }

    Some((rockVolume, joints))
  }
}
