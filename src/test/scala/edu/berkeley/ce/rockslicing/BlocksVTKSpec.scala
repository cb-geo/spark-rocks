package edu.berkeley.ce.rockslicing

import org.scalatest._

import scala.math.sqrt

class BlocksVTKSpec extends FunSuite {
  val boundingFaces = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi = 0, cohesion = 0), // -x = 0
    Face((1.0, 0.0, 0.0), 1.0, phi = 0, cohesion = 0), // x = 1
    Face((0.0, -1.0, 0.0), 0.0, phi = 0, cohesion = 0), // -y = 0
    Face((0.0, 1.0, 0.0), 1.0, phi = 0, cohesion = 0), // y = 1
    Face((0.0, 0.0, -1.0), 0.0, phi = 0, cohesion = 0), // -z = 0
    Face((0.0, 0.0, 1.0), 1.0, phi = 0, cohesion = 0) // z = 1
  )
  val unitCube = Block((0.0, 0.0, 0.0), boundingFaces)

  val boundingFaces2 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi = 0, cohesion = 0), // -x = 0
    Face((1.0, 0.0, 0.0), 2.0, phi = 0, cohesion = 0), // x = 2
    Face((0.0, -1.0, 0.0), 0.0, phi = 0, cohesion = 0), // -y = 0
    Face((0.0, 1.0, 0.0), 2.0, phi = 0, cohesion = 0), // y = 2
    Face((0.0, 0.0, -1.0), 0.0, phi = 0, cohesion = 0), // -z = 0
    Face((0.0, 0.0, 1.0), 2.0, phi = 0, cohesion = 0) // z = 2
  )
  val twoCube = Block((0.0, 0.0, 0.0), boundingFaces2)

  val boundingFaces3 = List(
    Face((-1.0, 0.0, 0.0), 1.0, phi = 0, cohesion = 0), // -x = 1
    Face((1.0, 0.0, 0.0), 1.0, phi = 0, cohesion = 0), // x = 1
    Face((0.0, -1.0, 0.0), 1.0, phi = 0, cohesion = 0), // -y = 1
    Face((0.0, 1.0, 0.0), 1.0, phi = 0, cohesion = 0), // y = 1
    Face((0.0, 0.0, -1.0), 1.0, phi = 0, cohesion = 0), // -z = 1
    Face((0.0, 0.0, 1.0), 1.0, phi = 0, cohesion = 0) // z = 1
  )
  val twoCubeNonOrigin = Block((1.0, 1.0, 1.0), boundingFaces3)

  val jointBounds = Seq(
    ((1.0, 0.0, 0.0), 1.0),
    ((-1.0, 0.0, 0.0), 0.0),
    ((0.0, 1.0, 0.0), 1.0),
    ((0.0, -1.0, 0.0), 0.0)
  )

  val jointBounds2 = Seq(
    ((1.0, 0.0, 0.0), 1.0),
    ((-1.0, 0.0, 0.0), 1.0),
    ((0.0, 1.0, 0.0), 1.0),
    ((0.0, -1.0, 0.0), 1.0)
  )

  val jointBounds3 = Seq(
    ((1.0 / sqrt(2.0), 1.0 / sqrt(2.0), 0.0), 1.0),
    ((-1.0 / sqrt(2.0), 1.0 / sqrt(2.0), 0.0), 1.0),
    ((-1.0 / sqrt(2.0), -1.0 / sqrt(2.0), 0.0), 1.0),
    ((1.0 / sqrt(2.0), -1.0 / sqrt(2.0), 0.0), 1.0)
  )

  test("Cutting the two-cube with faces x=0 and z=0 should produce four blocks") {
    val xPlane = Joint((1.0, 0.0, 0.0), (1.0, 1.0, 1.0), (1.0, 1.0, 1.0),
      phi = 0, cohesion = 0, shape = Nil)
    val zPlane = Joint((0.0, 0.0, 1.0), (1.0, 1.0, 1.0), (1.0, 1.0, 1.0),
      phi = 0, cohesion = 0, shape = Nil)
    val xBlocks = twoCube.cut(xPlane)
    val xzBlocks = xBlocks.flatMap(_.cut(zPlane))

    val nonRedundantBlocks = xzBlocks.map { case block@Block(center, faces) =>
      val nonRedundantFaces = block.nonRedundantFaces
      Block(center, nonRedundantFaces)
    }

    val centroidBlocks = nonRedundantBlocks.map { case block@Block(center, _) =>
      val centroid = block.centroid
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, updatedFaces)
    }

    val cleanedBlocks = centroidBlocks.map { case Block(center, faces) =>
      Block(center, faces.map(_.applyTolerance))
    }

    val vtkblocks = BlocksVTK(cleanedBlocks)
//    println(vtkblocks.faceVertices)
//    println(vtkblocks.vertices)
  }

  test("vtkblocks of unit cube") {
    val vtkblocks = BlocksVTK(Seq(twoCubeNonOrigin))
    println(vtkblocks.faceVertices)
    println(vtkblocks.vertices)
  }
}
