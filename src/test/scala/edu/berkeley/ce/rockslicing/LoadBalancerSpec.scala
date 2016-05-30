package edu.berkeley.ce.rockslicing

import org.scalatest._

class LoadBalancerSpec extends FunSuite {
  val boundingFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x = 0
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),  // x = 1
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y = 0
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),  // y = 1
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z = 0
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)   // z = 1
  )
  val unitCube = Block(Array(0.0, 0.0, 0.0), boundingFaces)

  val boundingFaces2 = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 0.666, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
  )
  val twothirdsCube = Block(Array(0.0, 0.0, 0.0), boundingFaces2)

  val boundingFaces3 = List(
    Face(Array(-1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, -1.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
  )
  val twoCube = Block(Array(0.0, 0.0, 0.0), boundingFaces3)

  test("18 seed joints should be generated for unit cube") {
    val numProcessors = 19
    val seedJoints =
      LoadBalancer.generateProcessorJoints(unitCube, numProcessors)
    assert(seedJoints.length + 1 == numProcessors)
  }

  test("Three seed joints should be generated for two thirds cube") {
    val numProcessors = 4
    val seedJoints =
      LoadBalancer.generateProcessorJoints(twothirdsCube, numProcessors)
    assert(seedJoints.length + 1 == numProcessors)
  }

  test("Five seed joints should be generated for two cube") {
    val numProcessors = 6
    val seedJoints =
      LoadBalancer.generateProcessorJoints(twoCube, numProcessors)
    assert(seedJoints.length + 1 == numProcessors)
  }

  val leftCubeFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0)
  )
  val leftCube = Block(Array(0.0, 0.0, 0.0), leftCubeFaces)

  val rightCubeFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0)
  )
  val rightCube = Block(Array(0.0, 1.0, 0.0), rightCubeFaces)

  val leftHalfUnitCubeFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.5, phi=0, cohesion=0),
    Face(Array(0.0, 1.0, 0.0), 0.0, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
  )
  val leftHalfUnitCube = Block(Array(0.0, 0.5, 0.0), leftHalfUnitCubeFaces)

  val rightHalfUCFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 1.0, 0.0), 0.5, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
  )
  val rightHalfUnitCube = Block(Array(0.0, 0.5, 0.0), rightHalfUCFaces)

  val leftQuarterUCFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.5, phi=0, cohesion=0),
    Face(Array(0.0, 1.0, 0.0), -0.25, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
  )
  val leftQuarterUnitCube = Block(Array(0.0, 0.5, 0.0), leftQuarterUCFaces)

  val centerPartUCFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.25, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 1.0, 0.0), 0.25, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
  )
  val centerPartUnitCube = Block(Array(0.0, 0.5, 0.0), centerPartUCFaces)

  val rightQuarterUCFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), -0.25, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 1.0, 0.0), 0.5, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
  )
  val rightQuarterUnitCube = Block(Array(0.0, 0.5, 0.0), rightQuarterUCFaces)

  val leftCenterQuarterUCFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.25, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 1.0, 0.0), 0.0, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
  )
  val leftCenterQuarterUnitCube = Block(Array(0.0, 0.5, 0.0), leftCenterQuarterUCFaces)

  val rightCenterQuarterUCFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 1.0, 0.0), 0.25, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
  )
  val rightCenterQuarterUnitCube = Block(Array(0.0, 0.5, 0.0), rightCenterQuarterUCFaces)

  val bottomHalfUCFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0)
  )
  val bottomHalfUnitCube = Block(Array(0.0, 0.0, 0.0), bottomHalfUCFaces)

  val topHalfUCFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0)
  )
  val topHalfUnitCube = Block(Array(0.0, 0.0, 0.5), topHalfUCFaces)

  /*
   * Divide unit cube into eight pieces with three processor joints present
   * Faces lists will be the same in top and bottom portions of unit cube divided into
   * eight blocks - only need to change center of block
   */
  val leftFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.5, phi=0, cohesion=0),
    Face(Array(0.0, 1.0, 0.0), -0.25, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0)
  )

  val rightFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), -0.25, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 1.0, 0.0), 0.5, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0)
  )

  val leftCenterFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.25, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 1.0, 0.0), 0.0, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0)
  )

  val rightCenterFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 1.0, 0.0), 0.25, phi=0, cohesion=0, isProcessorFace = true),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0)
  )

  val bottomLeftEighthUnitCube = Block(Array(0.0, 0.5, 0.0), leftFaces)

  val bottomLeftCenterEighthUnitCube = Block(Array(0.0, 0.5, 0.0), leftCenterFaces)

  val bottomRightCenterEighthUnitCube = Block(Array(0.0, 0.5, 0.0), rightCenterFaces)

  val bottomRightEighthUnitCube = Block(Array(0.0, 0.5, 0.0), rightFaces)

  val topLeftEighthUnitCube = Block(Array(0.0, 0.5, 0.5), leftFaces)

  val topLeftCenterEighthUnitCube = Block(Array(0.0, 0.5, 0.5), leftCenterFaces)

  val topRightCenterEighthUnitCube = Block(Array(0.0, 0.5, 0.5), rightCenterFaces)

  val topRightEighthUnitCube = Block(Array(0.0, 0.5, 0.5), rightFaces)

  test("Two blocks should be merged into unit cube") {
    val processorBlocks = Seq(leftHalfUnitCube, rightHalfUnitCube)
    val mergedBlocks = LoadBalancer.mergeProcessorBlocks(processorBlocks)
    val centroidMergedBlocks = mergedBlocks.map{ block =>
      val centroid = block.centroid
      Block(centroid, block.updateFaces(centroid))
    }

    assert(centroidMergedBlocks.length == 1)
    assert(centroidMergedBlocks.head.approximateEquals(unitCube))
  }

  test("Three blocks should be merged into unit cube") {
    val processorBlocks = Seq(leftQuarterUnitCube, centerPartUnitCube, rightQuarterUnitCube)
    val mergedBlocks = LoadBalancer.mergeProcessorBlocks(processorBlocks)
    val centroidMergedBlocks = mergedBlocks.map{ block =>
      val centroid = block.centroid
      Block(centroid, block.updateFaces(centroid))
    }

    assert(centroidMergedBlocks.length == 1)
    assert(centroidMergedBlocks.head.approximateEquals(unitCube))
  }

  test("Four blocks should be merged into unit cube") {
    val processorBlocks = Seq(leftQuarterUnitCube, leftCenterQuarterUnitCube,
      rightCenterQuarterUnitCube, rightQuarterUnitCube)
    val mergedBlocks = LoadBalancer.mergeProcessorBlocks(processorBlocks)
    val centroidMergedBlocks = mergedBlocks.map{ block =>
      val centroid = block.centroid
      Block(centroid, block.updateFaces(centroid))
    }

    assert(centroidMergedBlocks.length == 1)
    assert(centroidMergedBlocks.head.approximateEquals(unitCube))
  }

  test("Eight blocks should be merged into two blocks that comprise top and bottom halves "+
    "of unit cube") {
    val processorBlocks = Seq(bottomLeftEighthUnitCube, bottomLeftCenterEighthUnitCube,
      bottomRightCenterEighthUnitCube, bottomRightEighthUnitCube,
      topLeftEighthUnitCube, topLeftCenterEighthUnitCube,
      topRightCenterEighthUnitCube, topRightEighthUnitCube)

    val globalOrigin = Array(0.0, 0.0, 0.0)
    val mergedBlocks = LoadBalancer.mergeProcessorBlocks(processorBlocks).map { block =>
      Block(globalOrigin, block.updateFaces(globalOrigin))
    }
    val expectedBlocks = Seq(topHalfUnitCube, bottomHalfUnitCube).map { block =>
      Block(globalOrigin, block.updateFaces(globalOrigin))
    }

    assert(mergedBlocks.length == expectedBlocks.length)
    assert(mergedBlocks.forall(block1 => expectedBlocks.exists(block2 => block1.approximateEquals(block2))))
  }

  test("Processor joints should be removed and actual blocks restored") {
    val processorJoint1 = Joint(Array(0.0, 1.0, 0.0), Array(0.0, 0.0, 0.0),
      Array(0.0, 0.3, 0.0), phi = 0.0, cohesion = 0.0, shape = Vector.empty,
      processorJoint = true)
    val processorJoint2 = Joint(Array(0.0, 1.0, 0.0), Array(0.0, 0.0, 0.0),
      Array(0.0, 0.7, 0.0), phi = 0.0, cohesion = 0.0, shape = Vector.empty,
      processorJoint = true)
    val processorJoint3 = Joint(Array(0.0, 1.0, 0.0), Array(0.0, 0.0, 0.0),
      Array(0.0, 0.2, 0.0), phi = 0.0, cohesion = 0.0, shape = Vector.empty,
      processorJoint = true)
    val processorJoint4 = Joint(Array(0.0, 1.0, 0.0), Array(0.0, 0.0, 0.0),
      Array(0.0, 0.8, 0.0), phi = 0.0, cohesion = 0.0, shape = Vector.empty,
      processorJoint = true)

    val actualJoint1 = Joint(Array(0.0, 1.0, 0.0), Array(0.0, 0.0, 0.0), Array(0.0, 0.25, 0.0), phi = 0.0,
      cohesion = 0.0, shape = Vector.empty)
    val actualJoint2 = Joint(Array(0.0, 1.0, 0.0), Array(0.0, 0.0, 0.0), Array(0.0, 0.75, 0.0), phi = 0.0,
      cohesion = 0.0, shape = Vector.empty)

    val joints = Seq(processorJoint1, processorJoint2, processorJoint3, processorJoint4,
      actualJoint1, actualJoint2)

    val rawBlocks = joints.foldLeft(Seq(unitCube)) { case (currentBlocks, joint) =>
      currentBlocks.flatMap(_.cut(joint))
    }
    val nonRedundantBlocks = rawBlocks.map { case block @ Block(center, _, _) =>
      Block(center, block.nonRedundantFaces)
    }

    val (processorBlocks, nonProcessorBlocks) = nonRedundantBlocks.partition(_.faces.exists(_.isProcessorFace))
    val mergedBlocks = LoadBalancer.mergeProcessorBlocks(processorBlocks) ++ nonProcessorBlocks
    val expectedBlocks = Seq(rightQuarterUnitCube, centerPartUnitCube, leftQuarterUnitCube)

    assert(mergedBlocks.length == expectedBlocks.length)
    assert(mergedBlocks.forall(actualBlock => expectedBlocks.exists { expectedBlock =>
      actualBlock.approximateEquals(expectedBlock)
    }))
  }

  test("Oblique processor joints should be removed and actual blocks restored") {
    val processorJoint1 = Joint(Array(1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0)),
      Array(0.0, 0.0, 0.0), Array(0.0, 0.9, 0.0), phi = 0.0, cohesion = 0.0, shape = Vector.empty,
      processorJoint = true)
    val processorJoint2 = Joint(Array(1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0)),
      Array(0.0, 0.0, 0.0), Array(0.7, 0.7, 0.7), phi = 0.0, cohesion = 0.0, shape = Vector.empty,
      processorJoint = true)
    val processorJoint3 = Joint(Array(1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0)),
      Array(0.0, 0.0, 0.0), Array(0.5, 0.5, 0.5), phi = 0.0, cohesion = 0.0, shape = Vector.empty,
      processorJoint = true)
    val processorJoint4 = Joint(Array(1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0)),
      Array(0.0, 0.0, 0.0), Array(0.4, 0.4, 0.4), phi = 0.0, cohesion = 0.0, shape = Vector.empty,
      processorJoint = true)
    val processorJoint5 = Joint(Array(1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0)),
      Array(0.0, 0.0, 0.0), Array(0.6, 0.6, 0.6), phi = 0.0, cohesion = 0.0, shape = Vector.empty,
      processorJoint = true)

    val actualJoint1 = Joint(Array(0.0, 1.0, 0.0), Array(0.0, 0.0, 0.0), Array(0.0, 0.25, 0.0), phi = 0.0,
      cohesion = 0.0, shape = Vector.empty)
    val actualJoint2 = Joint(Array(0.0, 1.0, 0.0), Array(0.0, 0.0, 0.0), Array(0.0, 0.75, 0.0), phi = 0.0,
      cohesion = 0.0, shape = Vector.empty)

    val joints = Seq(processorJoint1, processorJoint2, processorJoint3, processorJoint4, processorJoint5,
      actualJoint1, actualJoint2)

    val blocks = joints.foldLeft(Seq(unitCube)) { case (currentBlocks, joint) =>
      currentBlocks.flatMap(_.cut(joint))
    }
    val nonRedundantBlocks = blocks map { case block @ Block(center, _, _) =>
      Block(center, block.nonRedundantFaces)
    }

    val (processorBlocks, nonProcessorBlocks) = nonRedundantBlocks.partition(_.faces.exists(_.isProcessorFace))
    val mergedBlocks = LoadBalancer.mergeProcessorBlocks(processorBlocks) ++ nonProcessorBlocks
    val expectedBlocks = Seq(leftQuarterUnitCube, rightQuarterUnitCube, centerPartUnitCube)

    assert(mergedBlocks.length == expectedBlocks.length)
    assert(mergedBlocks.forall(actualBlock => expectedBlocks.exists { expectedBlock =>
      actualBlock.approximateEquals(expectedBlock)
    }))
  }
}