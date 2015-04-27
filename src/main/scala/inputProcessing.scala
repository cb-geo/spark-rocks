package edu.berkeley.ce.rockslicing

import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import scala.collection.mutable.ListBuffer

object inputProcessor {
  // Generates face object from read inputs and adds it to the beginning of the list faces
  def addFace(normalVec: (Double, Double, Double), distance: Double,
              phi: Double, cohesion: Double, faces: ListBuffer[Face]) : Unit = {
    val currentFace = Face(normalVec, distance, phi, cohesion) 
    faces += currentFace
  }

  // Generates joint object from read inputs and adds it to the beginning of the list joints
  def addJoint(normalVec: (Double, Double, Double), center: (Double, Double, Double),
               dipAngle: Double, dipDirection: Double, distance: Double, phi: Double,
               cohesion: Double, shape: List[(Double, Double, Double)],
               joints: ListBuffer[Joint]) : Unit = {
    val  currentJoint = Joint(normalVec, center, dipAngle, dipDirection, 
                              distance, phi, cohesion, shape)
    joints += currentJoint
  }

  // Takes single line of input and returns it as a vector
  def makeVector(inputLine: String) : (Double, Double, Double) = {
    val inputArray = inputLine.split(" ").map(_.toDouble)
    val (a,b,c) = (inputArray(0), inputArray(1), inputArray(2))
    return (a,b,c)
  }

  // Processes input file: Add rock volume faces and joints to respective input list
  def readInput(fileName: String, rockVolume: ListBuffer[Face],
                joints: ListBuffer[Joint]) : Unit = {
    try { 
       // Rock volume data read first, will be set to true once all rock face data is read
      var transition: Boolean = false                                   
      var count: Int = 0 // Counter for determining which part of input is being processed
      // Declare variables that will be read for each face that defines
      // the volume of rock to be blocked.
      var a: Double = -1e12
      var b: Double = -1e12
      var c: Double = -1e12
      var d: Double = -1e12
      var phi: Double = -1e12
      var cohesion: Double = -1e12
      // Declare additional variables that are necessary for joints
      var dipAngle: Double = -1e12
      var dipDirection: Double = -1e12
      var centerX: Double = -1e12
      var centerY: Double = -1e12
      var centerZ: Double = -1e12
      var shape = List.empty[(Double, Double, Double)]

      val inputSource = Source.fromFile(fileName) // Open input file

      for (line <- inputSource.getLines) {
        // Check if transitioning to joint input: % in input indicates end of rock volume inputs
        if (line == '%') {
          transition = true
          count = 0
        }
        // Process input for rock volume faces
        if (transition == false) {
          count match {
            case 0 => val (a,b,c) = makeVector(line)            
            case 1 => d = line.toDouble
            case 2 => phi = line.toDouble
            case 3 => cohesion = line.toDouble
            // NOTE: Assumes blank line between faces
            case 4 => addFace((a, b, c), d, phi, cohesion, rockVolume) 
          }
          if (count < 4) {
            count += 1
          } else if (count == 4) {
            count = 0
          } else {
            println("ERROR IN ALGORITHM, CHECK CODE IN ROCK VOLUME PROCESSING")
          }
          // When done with rock volume input, process joint input
        } else {
          if (line == '#') { 
            // NOTE: Assumes # at the end of shape definition to indicate end of particular joint
            shape = shape.reverse
            addJoint((a,b,c), (centerX, centerY, centerZ), dipAngle, dipDirection, d,
                     phi, cohesion, shape, joints)
            // Reset shape list and counter
            shape = List.empty[(Double, Double, Double)]
            count = 0
          } else if (count < 7) {
            // Reads all input not associated with joint shape
            count match {
              case 0 => val (a,b,c) = makeVector(line)
              case 1 => val (centerX, centerY, centerZ) = makeVector(line)
              case 2 => dipAngle = line.toDouble
              case 3 => dipDirection = line.toDouble
              case 4 => d = line.toDouble
              case 5 => phi = line.toDouble
              case 6 => cohesion = line.toDouble
            }
          } else { 
            // Keeps adding shape lines to list until finding # in input file
            shape :::= List(makeVector(line))
          }
        }
      }
      inputSource.close
    } catch {
      case e: FileNotFoundException => println("Couldn't find that file.")
      case e: IOException => println("Got an IOException!")
    }
  }
}
