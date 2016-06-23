package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.DenseVector

/**
  * Provides functionality to generate joint sets for input joint parameters and faces
  * that represent faces of input rock volume.
  */
object JointGenerator {
  /**
    * Finds the normal to the joint specified by the input strike and dip
    *
    * @param strike Strike of the joint plane given in degrees
    * @param dip Dip of the joint plane given in degrees
    * @return Array that specifies the x, y and z components of the joint plane
    */
  def findJointNormal(strike: Double, dip: Double): Array[Double] = {
    val strikeRadians = strike * math.Pi / 180.0
    val dipRadians = dip * math.Pi / 180.0
    val strikeVector = DenseVector[Double](math.cos(-strikeRadians), math.sin(-strikeRadians), 0.0)
    // If joint is vertical, dip vector will point along negative z-axis
    if (math.abs(dip - 90.0) < NumericUtils.EPSILON) {
      val dipVector = DenseVector[Double](0.0, 0.0, -1.0)
      val jointNormal = linalg.normalize(linalg.cross(strikeVector, dipVector))
      Array(jointNormal(0), jointNormal(1), jointNormal(2))
    } else {
      val dipVector = DenseVector[Double](math.cos(-(strikeRadians + math.Pi / 2.0)),
        math.sin(-(strikeRadians + math.Pi / 2.0)),
        -math.sin(dipRadians))
      val jointNormal = linalg.normalize(linalg.cross(strikeVector, dipVector))
      Array(jointNormal(0), jointNormal(1), jointNormal(2))
    }
  }

  /**
    * Finds the faces that define the rock volume of interest
    *
    * @param globalOrigin Coordinates of global origin
    * @param boundingBox Coordinates specifying the lower left and upper right corners of the box that bounds
    *                    the domain of interest.
    * @param rockVolume Array of arrays describing each of the faces that define the boundaries of
    *                   the rock volume of interest. The inputs in each array are strike, dip and the x,
    *                   y and z coordinates of a point that lies within the plane containing the face.
    *                   Strike and dip should be specified in degrees.
    * @return A sequence of faces that represent the rock volume of interest
    */
  def findBoundingFaces(globalOrigin: Array[Double], boundingBox: Array[Double],
                        rockVolume: Array[Array[Double]]): Seq[Face] = {
    rockVolume map { parameters =>
      val faceCenter = Array(parameters(2), parameters(3), parameters(4))
      val normal = findJointNormal(parameters(0), parameters(1))
      val distance = Joint.findDistance(normal, globalOrigin, faceCenter)
      if (distance > 0.0) {
        Face(normal, distance, parameters(5), parameters(6))
      } else {
        // Ensures that all normals are outward pointing
        Face(Array(-normal(0), -normal(1), -normal(2)), -distance, parameters(5), parameters(6))
      }
    }
  }

  def findMasterJoints(globalOrigin: Array[Double], boundingBox: Array[Double],
                       joints: Array[Array[Double]]): Option[Seq[(Joint, Double)]] = {
    joints map { parameters =>
      if (parameters.length == 4) {
        val normal = findJointNormal(parameters(0), parameters(1))
        // CONTINUE HERE
      } else {
        println("Error, Joint Input Data: Stochastic joint generation not implemented yet")
        None
      }
    }
  }
}
