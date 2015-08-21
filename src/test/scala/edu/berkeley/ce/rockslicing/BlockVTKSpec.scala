package edu.berkeley.ce.rockslicing

import org.scalatest._
import scala.math.sqrt

class BlockVTKSpec extends FunSuite {
  val boundingFaces = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x = 0
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),  // x = 1
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y = 0
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),  // y = 1
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z = 0
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)   // z = 1
  )
  val unitCube = Block((0.0, 0.0, 0.0), boundingFaces)

  val slantedJoint = Joint((1.0/sqrt(2.0), 1.0/sqrt(2.0), 0.0), (0.0, 0.0, 0.0),
                           (1.0, 0.0, 0.0), 0.0, 0.0, shape = Nil)

  test("Getting to the bottom of this bullshit") {
//    val vtkBlock = BlockVTK(unitCube)
//    println(vtkBlock.orientedVertices)
//    println(vtkBlock.vertices)
//    println(vtkBlock.connectivity)
//    println(vtkBlock.normals)
//    println(vtkBlock.vertexIDs)
//    println(vtkBlock.offsets)
    val blocks = unitCube.cut(slantedJoint)
    val nonRedudundantBlocks = blocks map { case block @ Block(center, _) =>
        Block(center, block.nonRedundantFaces)
    }
    val vtkBlocks = nonRedudundantBlocks.map(BlockVTK(_))
    vtkBlocks foreach { case block =>
//          println(block.orientedVertices)
      println(block.orientedVertices)
      println(block.tupleVertices)
      println(block.faceCount)
    }
  }
}

