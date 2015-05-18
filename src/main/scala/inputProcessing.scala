package edu.berkeley.ce.rockslicing

import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import scala.collection.mutable.ListBuffer

object inputProcessor {
  // Generates face object from read inputs and adds it to the beginning of the list faces
  var transition = false
  var shape = List.empty[((Double, Double, Double), Double)]
  def addFace(normalVec: (Double, Double, Double), distance: Double,
              phi: Double, cohesion: Double, faces: ListBuffer[Face]) : Unit = {
    val currentFace = Face(normalVec, distance, phi, cohesion)
    faces += currentFace
  }

  // Generates joint object from read inputs and adds it to the beginning of the list joints
  def addJoint(normalVec: (Double, Double, Double), center: (Double, Double, Double),
               dipAngle: Double, dipDirection: Double, distance: Double, phi: Double,
               cohesion: Double, shape: List[((Double, Double, Double), Double)],
               joints: ListBuffer[Joint]) : Unit = {
    val  currentJoint = Joint(normalVec, distance, center, dipAngle, dipDirection,
                              phi, cohesion, shape)
    joints += currentJoint
  }

  // Finds the average x, y and z coordinates if the input joint set
  def averageCoords(joints: ListBuffer[Joint]) : (Double, Double, Double) = {
    var x_avg = 0.0
    var y_avg = 0.0
    var z_avg = 0.0
    for (joint <- joints) {
      x_avg += joint.centerX
      y_avg += joint.centerY
      z_avg += joint.centerZ
    }
    (x_avg/joints.length, y_avg/joints.length, z_avg/joints.length)
  }

  // Processes input file: Add rock volume faces and joints to respective input list
  def readInput(inputSource: Source, rockVolume: ListBuffer[Face],
                joints: ListBuffer[Joint]) : Unit = {
    try {
      for (line <- inputSource.getLines) {
        // Check if transitioning to joint input: % in input indicates end of rock volume inputs
        if (line == "%") {
          transition = true
        } else if (transition == false) {
          // Process input for rock volume faces
          val inputLine = line.split(" ").map(_.toDouble)
          val (a,b,c) = (inputLine(0), inputLine(1), inputLine(2))
          val d = inputLine(3)
          val phi = inputLine(4)
          val cohesion = inputLine(5)
          addFace((a,b,c), d, phi, cohesion, rockVolume)
        } else {
          // When done with rock volume input, process joint input
          shape = List.empty[((Double, Double, Double), Double)]
          val inputLine = line.split(" ").map(_.toDouble)
          val inputLength = inputLine.size
          val (a,b,c) = (inputLine(0), inputLine(1), inputLine(2))
          val d = inputLine(3)
          val (centerX,centerY,centerZ) = (inputLine(4), inputLine(5), inputLine(6))
          val dipAngle = inputLine(7)
          val dipDirection = inputLine(8)
          val phi = inputLine(9)
          val cohesion = inputLine(10)
          if (inputLength > 11) {
            for (i <- 0 until (inputLength - 11)/4)  {
              shape :::= List(((inputLine(4*i+11), inputLine(4*i+12), inputLine(4*i+13)), inputLine(4*i+14)))
            }
          }
          shape = shape.reverse
          addJoint((a,b,c), (centerX, centerY, centerZ), dipAngle, dipDirection, d,
                    phi, cohesion, shape, joints)
        }
      }
    } catch {
      case e: FileNotFoundException => println("Couldn't find that file.")
      case e: IOException => println("Got an IOException!")
    }
  }
}
