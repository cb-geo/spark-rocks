package edu.berkeley.ce.rockslicing

import scala.io.Source

object inputProcessor {
  // Processes input file: Add rock volume faces and joints to respective input list
  def readInput(inputSource: Source): (Seq[Face], Seq[Joint]) = {
    val lines = inputSource.getLines().toVector
    val transitionIndex = lines.indexWhere(_ == "%")
    assert(transitionIndex > -1)

    val rockVolumeData = lines.slice(0, transitionIndex)
    val jointData = lines.slice(transitionIndex + 1, lines.length)

    val rockVolume = rockVolumeData.map { line =>
      val tokens = line.split(" ") map(_.toDouble)
      assert(tokens.length == 6)

      val normalVec = (tokens(0), tokens(1), tokens(2))
      val d = tokens(3)
      val phi = tokens(4)
      val cohesion = tokens(5)
      Face(normalVec, d, phi, cohesion)
    }

    val joints = jointData.map { line =>
      val tokens = line.split(" ") map(_.toDouble)
      assert(tokens.length >= 13)
      val mandatoryTokens = tokens.take(13)
      val optionalTokens = tokens.slice(13, tokens.length)
      assert(optionalTokens.length % 4 == 0)

      val normalVec = (mandatoryTokens(0), mandatoryTokens(1), mandatoryTokens(2))
      val localOrigin = (mandatoryTokens(3), mandatoryTokens(4), mandatoryTokens(5))
      val center = (mandatoryTokens(6), mandatoryTokens(7), mandatoryTokens(8))
      val dipAngle = mandatoryTokens(9)
      val dipDirection = mandatoryTokens(10)
      val phi = mandatoryTokens(11)
      val cohesion = mandatoryTokens(12)

      val shape = optionalTokens.grouped(4).toVector.map { group =>
        val normalVec = (group(0), group(1), group(2))
        val d = group(3)
        (normalVec, d)
      }

      Joint(normalVec, localOrigin, center, dipAngle, dipDirection, phi, cohesion, shape)
    }

    (rockVolume, joints)
  }
}
