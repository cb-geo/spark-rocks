package edu.berkeley.ce.rockslicing

import scala.io.Source

object inputProcessor {
  // Processes input file: Add rock volume faces and joints to respective input list
  def readInput(inputSource: Source): (List[Face], List[Joint]) = {
    val lines = inputSource.getLines.toList
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
      assert(tokens.length >= 11)
      val mandatoryTokens = tokens.take(11)
      val optionalTokens = tokens.slice(11, tokens.length)
      assert(optionalTokens.length % 4 == 0)

      val normalVec = (mandatoryTokens(0), mandatoryTokens(1), mandatoryTokens(2))
      val d = mandatoryTokens(3)
      val center = (mandatoryTokens(4), mandatoryTokens(5), mandatoryTokens(6))
      val dipAngle = mandatoryTokens(7)
      val dipDirection = mandatoryTokens(8)
      val phi = mandatoryTokens(9)
      val cohesion = mandatoryTokens(10)

      val shape = optionalTokens.grouped(4).toList.map { group =>
        val normalVec = (group(0), group(1), group(2))
        val d = group(3)
        (normalVec, d)
      }

      Joint(normalVec, d, center, dipAngle, dipDirection, phi, cohesion, shape)
    }

    (rockVolume, joints)
  }

  // Finds the average x, y and z coordinates of the input joint set
  def averageCoords(joints: Seq[Joint]): (Double, Double, Double) = (
    joints.map(_.centerX).sum / joints.length,
    joints.map(_.centerY).sum / joints.length,
    joints.map(_.centerZ).sum / joints.length
  )
}
