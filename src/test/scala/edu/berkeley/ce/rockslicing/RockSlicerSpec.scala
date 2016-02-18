package edu.berkeley.ce.rockslicing

import org.scalatest._

class RockSlicerSpec extends FunSuite {
  val boundingFaces = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0),
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), 
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), 
    Face((0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0)   
  )
  val leftCube = Block((0.0, 0.0, 0.0), boundingFaces)

  val boundingFaces2 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0),
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), 
    Face((0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0) 
  )
  val rightCube = Block((0.0, 1.0, 0.0), boundingFaces2)

  val boundingFaces3 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), 
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), 
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), 
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0) 
  )
  val unitCube = Block((0.0, 0.0, 0.0), boundingFaces3)

  val boundingFaces4 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.5, phi=0, cohesion=0),
    Face((0.0, 1.0, 0.0), 0.0, phi=0, cohesion=0, processorJoint = true), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0) 
  )
  val leftHalfUnitCube = Block((0.0, 0.5, 0.0), boundingFaces4)

  val boundingFaces5 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 1.0, 0.0), 0.5, phi=0, cohesion=0), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0) 
  )
  val rightHalfUnitCube = Block((0.0, 0.5, 0.0), boundingFaces5)

  val boundingFaces6 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.5, phi=0, cohesion=0),
    Face((0.0, 1.0, 0.0), -0.25, phi=0, cohesion=0, processorJoint = true), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0) 
  )
  val leftQuarterUnitCube = Block((0.0, 0.5, 0.0), boundingFaces6)

  val boundingFaces7 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.25, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 1.0, 0.0), 0.25, phi=0, cohesion=0, processorJoint = true), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0) 
  )
  val centerPartUnitCube = Block((0.0, 0.5, 0.0), boundingFaces7)

  val boundingFaces8 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), -0.25, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 1.0, 0.0), 0.5, phi=0, cohesion=0), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0) 
  )
  val rightQuarterUnitCube = Block((0.0, 0.5, 0.0), boundingFaces8)

  val boundingFaces9 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.25, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 1.0, 0.0), 0.0, phi=0, cohesion=0, processorJoint = true), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0) 
  )
  val leftCenterQuarterUnitCube = Block((0.0, 0.5, 0.0), boundingFaces9)

  val boundingFaces10 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 1.0, 0.0), 0.25, phi=0, cohesion=0, processorJoint = true), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0) 
  )
  val rightCenterQuarterUnitCube = Block((0.0, 0.5, 0.0), boundingFaces10)

  val boundingFaces11 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0) 
  )
  val bottomHalfUnitCube = Block((0.0, 0.0, 0.0), boundingFaces11)

  val boundingFaces12 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0) 
  )
  val topHalfUnitCube = Block((0.0, 0.0, 0.5), boundingFaces12)

  // Divide unit cube into eight pieces with three processor joints present
  val boundingFaces13 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.5, phi=0, cohesion=0),
    Face((0.0, 1.0, 0.0), -0.25, phi=0, cohesion=0, processorJoint = true), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0) 
  )
  val bottomLeftEighthUnitCube = Block((0.0, 0.5, 0.0), boundingFaces13)

  val boundingFaces14 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.25, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 1.0, 0.0), 0.0, phi=0, cohesion=0, processorJoint = true), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0) 
  )
  val bottomLeftCenterEighthUnitCube = Block((0.0, 0.5, 0.0), boundingFaces14)

  val boundingFaces15 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 1.0, 0.0), 0.25, phi=0, cohesion=0, processorJoint = true), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0) 
  )
  val bottomRightCenterEighthUnitCube = Block((0.0, 0.5, 0.0), boundingFaces15)

  val boundingFaces16 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), -0.25, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 1.0, 0.0), 0.5, phi=0, cohesion=0), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0) 
  )
  val bottomRightEighthUnitCube = Block((0.0, 0.5, 0.0), boundingFaces16)

  val boundingFaces17 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.5, phi=0, cohesion=0),
    Face((0.0, 1.0, 0.0), -0.25, phi=0, cohesion=0, processorJoint = true), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0) 
  )
  val topLeftEighthUnitCube = Block((0.0, 0.5, 0.5), boundingFaces17)

  val boundingFaces18 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.25, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 1.0, 0.0), 0.0, phi=0, cohesion=0, processorJoint = true), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0) 
  )
  val topLeftCenterEighthUnitCube = Block((0.0, 0.5, 0.5), boundingFaces18)

  val boundingFaces19 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 1.0, 0.0), 0.25, phi=0, cohesion=0, processorJoint = true), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0) 
  )
  val topRightCenterEighthUnitCube = Block((0.0, 0.5, 0.5), boundingFaces19)

  val boundingFaces20 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), 
    Face((0.0, -1.0, 0.0), -0.25, phi=0, cohesion=0, processorJoint = true),
    Face((0.0, 1.0, 0.0), 0.5, phi=0, cohesion=0), 
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face((0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0) 
  )
  val topRightEighthUnitCube = Block((0.0, 0.5, 0.5), boundingFaces20)

// test("The two cubes should share one processor face") {
//   val leftCenter = (leftCube.centerX, leftCube.centerY, leftCube.centerZ)
//   val updatedRightCube = Block(leftCenter, rightCube.updateFaces(leftCenter))
//
//   val sharedFaces = RockSlicer.compareProcessorBlocks(leftCube, updatedRightCube)
//   assert(!sharedFaces.exists( face => math.abs(face.d) != 1.0))
//   assert(sharedFaces.length == 2)
// }
//
// test("The two blocks should share one processor face, but with distances reversed") {
//   val rightCenter = (rightCube.centerX, rightCube.centerY, rightCube.centerZ)
//   val updatedLeftCube = Block(rightCenter, leftCube.updateFaces(rightCenter))
//   val sharedFaces = RockSlicer.compareProcessorBlocks(rightCube, updatedLeftCube)
//   assert(!sharedFaces.exists( face => face.d != 0.0))
//   assert(sharedFaces.length == 2)
// }
//
// test("Two blocks should be merged into unit cube") {
//   val processorBlocks = Seq(leftHalfUnitCube, rightHalfUnitCube)
//   val (mergedBlocks, orphanBlocks) = RockSlicer.mergeBlocks(processorBlocks, Seq.empty[Block],
//                                                             Seq.empty[Block])
//   val centroidMergedBlocks = mergedBlocks.map{ block =>
//     val centroid = block.centroid
//     Block(centroid, block.updateFaces(centroid))
//   }
//
//   assert(orphanBlocks.isEmpty)
//   assert(centroidMergedBlocks.length == 1)
//   assert(centroidMergedBlocks.head.approximateEquals(unitCube))
// }
//
// test("Three blocks should be merged into unit cube") {
//   val processorBlocks = Seq(leftQuarterUnitCube, centerPartUnitCube, rightQuarterUnitCube)
//   val (mergedBlocks, orphanBlocks) = RockSlicer.mergeBlocks(processorBlocks, Seq.empty[Block],
//                                                             Seq.empty[Block])
//   val centroidMergedBlocks = mergedBlocks.map{ block =>
//     val centroid = block.centroid
//     Block(centroid, block.updateFaces(centroid))
//   }
//
//   assert(orphanBlocks.isEmpty)
//   assert(centroidMergedBlocks.length == 1)
//   assert(centroidMergedBlocks.head.approximateEquals(unitCube))
// }
//
// test("Four blocks should be merged into unit cube") {
//   val processorBlocks = Seq(leftQuarterUnitCube, leftCenterQuarterUnitCube,
//                             rightCenterQuarterUnitCube, rightQuarterUnitCube)
//   val (mergedBlocks, orphanBlocks) = RockSlicer.mergeBlocks(processorBlocks, Seq.empty[Block],
//                                                             Seq.empty[Block])
//   val centroidMergedBlocks = mergedBlocks.map{ block =>
//     val centroid = block.centroid
//     Block(centroid, block.updateFaces(centroid))
//   }
//
//   assert(orphanBlocks.isEmpty)
//   assert(centroidMergedBlocks.length == 1)
//   assert(centroidMergedBlocks.head.approximateEquals(unitCube))
// }
//
// test("Eight blocks should be merged into two blocks that comprise top and bottom halves "+
//      "of unit cube") {
//   val processorBlocks = Seq(bottomLeftEighthUnitCube, bottomLeftCenterEighthUnitCube,
//                             bottomRightCenterEighthUnitCube, bottomRightEighthUnitCube,
//                             topLeftEighthUnitCube, topLeftCenterEighthUnitCube,
//                             topRightCenterEighthUnitCube, topRightEighthUnitCube)
//   val (mergedBlocks, orphanBlocks) = RockSlicer.mergeBlocks(processorBlocks, Seq.empty[Block],
//                                                             Seq.empty[Block])
//   val centroidMergedBlocks = mergedBlocks.map{ block =>
//     val centroid = block.centroid
//     Block(centroid, block.updateFaces(centroid))
//   }
//
//   val expectedBlocks = Seq(topHalfUnitCube, bottomHalfUnitCube)
//   val expectedBlocksCentroid = expectedBlocks map { block =>
//     val centroid = block.centroid
//     Block(centroid, block.updateFaces(centroid))
//   }
//
//   val blockCheck = centroidMergedBlocks.zip(expectedBlocksCentroid) map { case (calc, expected) =>
//     calc.approximateEquals(expected)
//   }
//   assert(orphanBlocks.isEmpty)
//   assert(centroidMergedBlocks.length == 2)
//   assert(!blockCheck.contains(false))
// }
//
// test("Processor joints should be removed and actual blocks restored") {
//   val processorJoint1 = Joint((0.0, 1.0, 0.0), (0.0, 0.0, 0.0),
//                               (0.0, 0.3, 0.0), phi = 0.0, cohesion = 0.0, shape = Nil,
//                                processorJoint = true)
//   val processorJoint2 = Joint((0.0, 1.0, 0.0), (0.0, 0.0, 0.0),
//                               (0.0, 0.7, 0.0), phi = 0.0, cohesion = 0.0, shape = Nil,
//                                processorJoint = true)
//   val processorJoint3 = Joint((0.0, 1.0, 0.0), (0.0, 0.0, 0.0),
//                               (0.0, 0.2, 0.0), phi = 0.0, cohesion = 0.0, shape = Nil,
//                               processorJoint = true)
//   val processorJoint4 = Joint((0.0, 1.0, 0.0), (0.0, 0.0, 0.0),
//                               (0.0, 0.8, 0.0), phi = 0.0, cohesion = 0.0, shape = Nil,
//                               processorJoint = true)
//
//   val actualJoint1 = Joint((0.0, 1.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.25, 0.0), phi = 0.0,
//                            cohesion = 0.0, shape = Nil)
//   val actualJoint2 = Joint((0.0, 1.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.75, 0.0), phi = 0.0,
//                            cohesion = 0.0, shape = Nil)
//   val joints = Seq(processorJoint1, processorJoint2, processorJoint3, processorJoint4,
//                    actualJoint1, actualJoint2)
//   var blocks = Seq(unitCube)
//   for (joint <- joints) {
//     blocks = blocks.flatMap(_.cut(joint))
//   }
//
//   val nonRedundantBlocks = blocks.map { case block @ Block(center, _) =>
//     Block(center, block.nonRedundantFaces)
//   }
//
//   val processorBlocks = nonRedundantBlocks.filter { block =>
//     block.faces.exists { face => face.processorJoint }
//   }
//   val (mergedBlocks, orphanBlocks) = RockSlicer.mergeBlocks(processorBlocks, Seq.empty[Block],
//                                                             Seq.empty[Block])
//
//   val centroidMergedBlocks = mergedBlocks.map{ block =>
//     val centroid = block.centroid
//     Block(centroid, block.updateFaces(centroid))
//   }
//
//   val expectedBlocks = Seq(rightQuarterUnitCube, centerPartUnitCube, leftQuarterUnitCube)
//   val expectedBlocksCentroid = expectedBlocks map { block =>
//     val centroid = block.centroid
//     Block(centroid, block.updateFaces(centroid))
//   }
//
//   val blockCheck = centroidMergedBlocks.zip(expectedBlocksCentroid) forall { case (calc, expected) =>
//     calc.approximateEquals(expected)
//   }
//
//   assert(orphanBlocks.isEmpty)
//   assert(centroidMergedBlocks.length == 3)
//   assert(blockCheck)
// }

  test("Oblique processor joints should be removed and actual blocks restored") {
    val processorJoint1 = Joint((1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0)),
                                (0.0, 0.0, 0.0), (0.0, 0.9, 0.0), phi = 0.0, cohesion = 0.0, shape = Nil,
                                 processorJoint = true)
    val processorJoint2 = Joint((1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0)),
                                (0.0, 0.0, 0.0), (0.7, 0.7, 0.7), phi = 0.0, cohesion = 0.0, shape = Nil,
                                 processorJoint = true)
    val processorJoint3 = Joint((1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0)),
                                (0.0, 0.0, 0.0), (0.5, 0.5, 0.5), phi = 0.0, cohesion = 0.0, shape = Nil,
                                processorJoint = true)
    val processorJoint4 = Joint((1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0)),
                                (0.0, 0.0, 0.0), (0.4, 0.4, 0.4), phi = 0.0, cohesion = 0.0, shape = Nil,
                                processorJoint = true)
    val processorJoint5 = Joint((1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0), 1.0/math.sqrt(3.0)),
                                (0.0, 0.0, 0.0), (0.6, 0.6, 0.6), phi = 0.0, cohesion = 0.0, shape = Nil,
                                processorJoint = true)
    val actualJoint1 = Joint((0.0, 1.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.25, 0.0), phi = 0.0,
                             cohesion = 0.0, shape = Nil)
    val actualJoint2 = Joint((0.0, 1.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.75, 0.0), phi = 0.0,
                             cohesion = 0.0, shape = Nil)
    val actualJoint3 = Joint((1.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.5, 0.0, 0.0), phi = 0.0,
                             cohesion = 0.0, shape = Nil)

    val joints = Seq(processorJoint1, processorJoint2, processorJoint3, processorJoint4, processorJoint5,
                     actualJoint1, actualJoint2)
    var blocks = Seq(unitCube)
    for (joint <- joints) {
      blocks = blocks.flatMap(_.cut(joint))
    }

    val nonRedundantBlocks = blocks.map { case block @ Block(center, _) =>
        Block(center, block.nonRedundantFaces)
    }

    val processorBlocks = nonRedundantBlocks.filter { block =>
      block.faces.exists { face => face.processorJoint }
    }

    val (mergedBlocks, orphanBlocks) = RockSlicer.mergeBlocks(processorBlocks, Seq.empty[Block],
                                                              Seq.empty[Block])

    val centroidMergedBlocks = mergedBlocks.map{ block =>
      val centroid = block.centroid
      Block(centroid, block.updateFaces(centroid))
    }

    val expectedBlocks = Seq(leftQuarterUnitCube, rightQuarterUnitCube, centerPartUnitCube)
    val expectedBlocksCentroid = expectedBlocks map { block =>
      val centroid = block.centroid
      Block(centroid, block.updateFaces(centroid))
    }
    val blockCheck = centroidMergedBlocks.zip(expectedBlocksCentroid) forall { case (calc, expected) =>
      calc.approximateEquals(expected)
    }

    println("\nFINAL MERGED BLOCKS:")
    centroidMergedBlocks.foreach(println)
    println("FINAL ORPHAN BLOCKS:")
    orphanBlocks.foreach(println)
    assert(orphanBlocks.isEmpty)
    assert(centroidMergedBlocks.length == 3)
    assert(blockCheck)
  }
//
// test("Orphan blocks should be initial input list") {
//   val processorBlocks = Seq(leftQuarterUnitCube, rightQuarterUnitCube)
//
//   val (mergedBlocks, orphanBlocks) = RockSlicer.mergeBlocks(processorBlocks, Seq.empty[Block],
//                                                             Seq.empty[Block])
//   assert(mergedBlocks.isEmpty)
//   assert(processorBlocks.diff(orphanBlocks).isEmpty)
// }
}
