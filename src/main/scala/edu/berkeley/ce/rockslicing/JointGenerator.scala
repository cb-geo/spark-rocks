package edu.berkeley.ce.rockslicing

import breeze.linalg
import breeze.linalg.DenseVector
import scala.annotation.tailrec

object JointGenerator {
  /**
    * Finds the normal to the joint specified by the input strike and dip
    *
    * @param strike Strike of the joint plane given in degrees
    * @param dip Dip of the joint plane given in degrees
    * @return Array that specifies the x, y and z components of the joint plane
    */
  private def findJointNormal(strike: Double, dip: Double): Array[Double] = {
    val strikeRadians = strike.toRadians
    val dipRadians = dip.toRadians
    val strikeVector = DenseVector[Double](math.cos(-strikeRadians), math.sin(-strikeRadians), 0.0)
    // If joint is vertical, dip vector will point along negative z-axis
    val dipVector = if (math.abs(dip - 90.0) < NumericUtils.EPSILON) {
      DenseVector[Double](0.0, 0.0, -1.0)
    } else {
      DenseVector[Double](math.cos(dipRadians) * math.cos(-(strikeRadians + math.Pi / 2.0)),
        math.cos(dipRadians) * math.sin(-(strikeRadians + math.Pi / 2.0)),
        -math.sin(dipRadians))
    }
    val jointNormal = linalg.normalize(linalg.cross(strikeVector, dipVector))
    Array(jointNormal(0), jointNormal(1), jointNormal(2))
  }

  /**
    * Finds the faces that define the rock volume of interest
    *
    * @param globalOrigin Coordinates of global origin
    * @param rockVolumeData Array of arrays describing each of the faces that define the boundaries of
    *                       the rock volume of interest. The inputs in each array are strike, dip and the x,
    *                       y and z coordinates of a point that lies within the plane containing the face.
    *                       Strike and dip should be specified in degrees.
    * @return A sequence of faces that represent the rock volume of interest
    */
  private def findBoundingFaces(globalOrigin: Array[Double], rockVolumeData: Seq[Array[Double]]): Seq[Face] = {
    rockVolumeData map { parameters =>
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

  /**
    * Finds representative joint for each joint set, centered at the lower left corner of the
    * bounding box. For deterministic inputs, representative joint will have normal vector
    * corresponding to strike and dip input by user as well as the specified phi and cohesion.
    * Stochastic inputs are presently not supported, but will be implemented at a later time.
    *
    * @param globalOrigin Coordinates of the global origin
    * @param lowerLeftCorner Lower left corner of the bounding box
    * @param jointSets Array containing the input data representing joint sets
    * @return Seq of "master" joints that represent input joint sets. These will be used as a starting point
    *         to generate full joint set.
    */
  private def findMasterJoints(globalOrigin: Array[Double], lowerLeftCorner: Array[Double],
                               jointSets: Seq[Array[Double]]): Seq[Joint] = {
    val masterJoints = jointSets flatMap { parameters =>
      if (parameters.length == 6) {
        // TODO: Implement non-persistent joint generator
        val normal = findJointNormal(parameters(0), parameters(1))
        Some(Joint(normal, globalOrigin, center=Array(lowerLeftCorner(0), lowerLeftCorner(1), lowerLeftCorner(2)),
          phi=parameters(4), cohesion=parameters(5), shape=Vector.empty))
      } else {
        // TODO: Implement stochastic generator
        throw new UnsupportedOperationException("ERROR: Joint Input Data: Stochastic joint generation " +
          "not yet implemented")
      }
    }
    masterJoints
  }
}

/**
  * Provides functionality to generate joint sets for input joint parameters and faces
  * that represent faces of input rock volume.
  *
  * @param globalOrigin Coordinates of the global origin
  * @param boundingBox Coordinates specifying the lower left and upper right corners of the box that bounds
  *                    the domain of interest.
  * @param rockVolumeData Seq of arrays describing each of the faces that define the boundaries of
  *                       the rock volume of interest. The inputs in each array are strike, dip and the x,
  *                       y and z coordinates of a point that lies within the plane containing the face.
  *                       Strike and dip should be specified in degrees.
  * @param jointSetData Seq of arrays containing the input data representing joint sets. The inputs in each array
  *                     are strike, dip, joint spacing, persistence, phi, cohesion and optional stochastic parameters
  */
case class JointGenerator(globalOrigin: Array[Double], boundingBox: Array[Double], rockVolumeData: Seq[Array[Double]],
                          jointSetData: Seq[Array[Double]]) {
  val origin = globalOrigin
  val lowerLeftCorner = Array(boundingBox(0), boundingBox(1), boundingBox(2))
  val upperRightCorner = Array(boundingBox(3), boundingBox(4), boundingBox(5))
  val rockVolume = JointGenerator.findBoundingFaces(globalOrigin, rockVolumeData)
  val masterJoints = JointGenerator.findMasterJoints(globalOrigin, lowerLeftCorner, jointSetData)
  val jointSets = generateJointSets(jointSetData)

  /**
    * Generates joint sets for each of the input joint sets
    *
    * @param jointSetData Array of arrays containing the input data representing joint sets. The inputs in each array
    *                     are strike, dip, joint spacing, persistence, phi, cohesion and optional stochastic parameters
    * @return Seq of joints for each input joint set
    */
  def generateJointSets(jointSetData: Seq[Array[Double]]): Seq[Seq[Joint]] = {
    val jointTuples = masterJoints.zip(jointSetData)
    
    val deltaX = upperRightCorner(0) - lowerLeftCorner(0)
    val deltaY = upperRightCorner(1) - lowerLeftCorner(1)
    val deltaZ = upperRightCorner(2) - lowerLeftCorner(2)

    val lowerCorners = Array(lowerLeftCorner,
      Array(lowerLeftCorner(0) + deltaX, lowerLeftCorner(1), lowerLeftCorner(2)),
      Array(lowerLeftCorner(0) + deltaX, lowerLeftCorner(1) + deltaY, lowerLeftCorner(2)),
      Array(lowerLeftCorner(0), lowerLeftCorner(1) + deltaY, lowerLeftCorner(2))
    )

    val upperCorners = Array(
      Array(lowerCorners(2)(0), lowerCorners(2)(1), lowerCorners(2)(2) + deltaZ),
      Array(lowerCorners(3)(0), lowerCorners(3)(1), lowerCorners(3)(2) + deltaZ),
      Array(lowerCorners(0)(0), lowerCorners(0)(1), lowerCorners(0)(2) + deltaZ),
      Array(lowerCorners(1)(0), lowerCorners(1)(1), lowerCorners(1)(2) + deltaZ)
    )

    // Diagonal vectors of bounding box
    val diagonalVectors = upperCorners.zip(lowerCorners) map { case (upper, lower) =>
        linalg.normalize(DenseVector[Double](upper(0) - lower(0), upper(1) - lower(1), upper(2) - lower(2)))
    }

    jointTuples map { case (joint, jointData) =>
      if (jointData(3) == 100) {
        // Calculate dot product of each diagonal vector with joint normal
        val diagDotProducts = diagonalVectors map { diagonal =>
          math.abs(diagonal dot DenseVector[Double](joint.a, joint.b, joint.c))
        }
        // Use diagonal that is most parallel to joint normal - this ensures that joints generated will populate
        // entire bounding box
        val bestDiagonal = diagonalVectors(diagDotProducts.indexOf(diagDotProducts.max))
        val diagonalIndex = diagDotProducts.indexOf(diagDotProducts.max)
        val initialCorner = lowerCorners(diagonalIndex)
        val terminalCorner = upperCorners(diagonalIndex)
        makePersistentJointSet(joint, jointData, bestDiagonal, initialCorner, terminalCorner)
      } else {
        makeNonPersistentJointSet(joint, jointData)
      }
    }
  }

  /**
    * Generates a sequence of joints spanning the entire bounding box. The sequence of joints represents
    * a single joint set.
    *
    * @param joint Joint that is representative of the joint set - properties of the joint are the mean values.
    * @param jointData Properties of the joint set - strike, dip, joint spacing, persistence, phi, cohesion and
    *                  optional stochastic parameters
    * @param diagonalVector Diagonal vector of bounding box along which joints will be generated
    * @param center Center of joint
    * @param terminationPoint Point beyond which no further joints should be generated
    * @param jointSet Seq of joints representing the joint set. This will be an empty Seq when the function is
    *                 first called.
    * @return Seq of joints representing the joint set
    */
  @tailrec
  private def makePersistentJointSet(joint: Joint, jointData: Array[Double], diagonalVector: DenseVector[Double],
                                     center: Array[Double], terminationPoint: Array[Double],
                                     jointSet: Seq[Joint] = Seq.empty[Joint]): Seq[Joint] = {
    val terminationVector = DenseVector[Double](terminationPoint(0) - center(0),
      terminationPoint(1) - center(1),
      terminationPoint(2) - center(2)
    )
    // Check if center is still located within bounding box
    if (terminationVector.dot(diagonalVector) >= -NumericUtils.EPSILON) {
      // Create new joint with updated center
      val newJoint = Joint(Array(joint.a, joint.b, joint.c), origin, center, joint.phi, joint.cohesion,
        joint.shape, joint.dipAngleParam, joint.dipDirectionParam)
      // Update center based on joint spacing and diagonal vector
      val centerIncrement = jointData(2) / math.abs(diagonalVector dot DenseVector[Double](joint.a, joint.b, joint.c))
      val newCenter = Array(
        diagonalVector(0)*centerIncrement + center(0),
        diagonalVector(1)*centerIncrement + center(1),
        diagonalVector(2)*centerIncrement + center(2)
      )
      makePersistentJointSet(joint, jointData, diagonalVector, newCenter, terminationPoint, newJoint +: jointSet)
    } else {
      jointSet
    }
  }

  /**
    * This function is not yet implemented. Placeholder until verifying current load balance logic works.
    *
    * @param joint
    * @param jointData
    * @return
    */
  def makeNonPersistentJointSet(joint: Joint, jointData: Array[Double]): Seq[Joint] = {
    // Still needs to be implemented, but will require stochastic generator
    throw new UnsupportedOperationException("ERROR: Non-persistent joint generator not yet implemented")
  }
}