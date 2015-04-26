package edu.berkeley.ce.rockslicing

import scala.io.Source
import java.io.{FileNotFoundException, IOException}

object inputProcessor {
  // Generates face object from read inputs and adds to to the beginning of the list faces
  def addFace(normalVec: (Double, Double, Double), distance: Double,
              phi: Double, cohesion: Double, faces: List[Face]) : Unit = {
    val currentFace = Face(normalVec, distance, phi, cohesion) 
    faces :::= List(currentFace)
  }
 
  // Takes single line of input and returns it as a vector
  def makeVector(inputLine: String) : (Double, Double, Double) = {
    val inputArray = inputLine.split(" ").map(_.toDouble)
    val (a,b,c) = (inputArray(0), inputArray(1), inputArray(2))
  }

  // Processes input file: Add rock volume faces and discontinuities to respective input list
  def readInput(fileName: String, rockVolume: List[Face],
                discontinuities: List[Discontinuity]) : Unit = {
    try { 
       // Rock volume data read first, will be set to true once all rock face data is read
      var transition: Boolean = false                                   
      var count: Int = 0 // Counter that gets updated as reading text file - 6 lines per rock face
      // Declare variables that will be read for each face that defines
      // the volume of rock to be blocked.
      var a: Double = -1e12
      var b: Double = -1e12
      var c: Double = -1e12
      var d: Double = -1e12
      var phi: Double = -1e12
      var cohesion: Double = -1e12
      val inputSource = Source.fromFile(fileName) // Open input file

      for (line <- inputSource.getLines) {
        // Check if transitioning to discontinuity input:
        // % in input indicates end of rock volume inputs
        if (line == '%') {
          transition = true
        }
        // Process input for rock volume faces
        if (transition == false) {
          count match {
            case 0 => (a,b,c) => makeVector(line)            
            case 1 => d = line.toDouble
            case 2 => phi = line.toDouble
            case 3 => cohesion = line.toDouble
            // NOTE: Assumes blank line between faces
            case 4 => addFace((a, b, c), d, phi, cohesion, rockVolume) 
          }
          if (count < 4) {
            count += 1
          } else if (count == 6) {
            count = 0
          } else {
            println("ERROR IN ALGORITHM, CHECK CODE IN ROCK VOLUME PROCESSING")
          }
          // Process joint input
          // TODO: ADD JOINT DATA PROCESSING
        } else {
          println("This is where the joint data gets processed")
        }
      }
      inputSource.close
    } catch {
      case e: FileNotFoundException => println("Couldn't find that file.")
      case e: IOException => println("Got an IOException!")
    }
  }
}
