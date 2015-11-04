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

  val boundingFaces2 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x <= 0
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),  // x <= 1
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y <= 0
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),  // y <= 1
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z <= 0
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0),  // z <= 1
    Face((0.707106781187, 0.707106781187, 0.0), 0.707106781187 - 0.5, phi=0, cohesion=0) // sqrt(2)*x + sqrt(2)*y <= 1
  )
  val sevenSidedBlock = Block((0.0, 0.0, 0.0), boundingFaces2)

  val face1 = Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0)
  val face2 = Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0)
  val face3 = Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0)
  val face4 = Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0)
  val face5 = Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0)
  val face6 = Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
  val face1Verts = List((0.0, 1.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.0, 1.0), (0.0, 1.0, 1.0))
  val face2Verts = List((1.0, 1.0, 1.0), (1.0, 0.0, 1.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0))
  val face3Verts = List((0.0, 0.0, 1.0), (0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 0.0, 1.0))
  val face4Verts = List((0.0, 1.0, 0.0), (0.0, 1.0, 1.0), (1.0, 1.0, 1.0), (1.0, 1.0, 0.0))
  val face5Verts = List((1.0, 1.0, 0.0), (1.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 1.0, 0.0))
  val face6Verts = List((0.0, 1.0, 1.0), (0.0, 0.0, 1.0), (1.0, 0.0, 1.0), (1.0, 1.0, 1.0))

  private def doubleListElementDiff(list1: Seq[Double], list2: Seq[Double]): Seq[Double] = {
    assert(list1.length == list2.length)
    list1.zip(list2) map { case (a,b) => a - b }
  }

  private def intListElementDiff(list1: Seq[Int], list2: Seq[Int]): Seq[Int] = {
    assert(list1.length == list2.length)
    list1.zip(list2) map { case (a,b) => a - b }
  }

  test("The vertices should be oriented counter-clockwise for each face") {
    val expectedVertices = Map(
      face1 -> face1Verts,
      face2 -> face2Verts,
      face3 -> face3Verts,
      face4 -> face4Verts,
      face5 -> face5Verts,
      face6 -> face6Verts
    )
    val vtkBlock = BlockVTK(unitCube)
    assert(expectedVertices == vtkBlock.orientedVertices)
  }

  test("The vertices should be oriented counter-clockwise for each face (seven-sided block)") {
    val expectedVertices = Map(
      face1 -> face1Verts,
      face2 -> face2Verts,
      face3 -> face3Verts,
      face4 -> face4Verts,
      face5 -> face5Verts,
      face6 -> face6Verts
    )
    val vtkBlock = BlockVTK(sevenSidedBlock)
    println(vtkBlock.tupleVertices)
    assert(expectedVertices == vtkBlock.orientedVertices)
  }

  test("List of vertices should contain only distinct vertices as tuples") {
    val vtkBlock = BlockVTK(unitCube)
    val expectedVertices = List((0.0, 1.0, 1.0), (0.0, 0.0, 1.0), (1.0, 0.0, 1.0), (1.0, 1.0, 1.0),
                                (0.0, 1.0, 0.0), (0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0))
    assert(expectedVertices == vtkBlock.tupleVertices)
  }

  test("List of tuple vertices should flatten into list of doubles") {
    val vtkBlock = BlockVTK(unitCube)
    val expectedVertices = List(0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
                                0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0)
    val verticesDiff = doubleListElementDiff(expectedVertices, vtkBlock.vertices)
    assert(verticesDiff.filter(_ > NumericUtils.EPSILON).isEmpty)
  }

  test("Vertex ID's should start from 0 and go up to 7") {
    val vtkBlock = BlockVTK(unitCube)
    val expectedIDs = Seq.range(0, 8)
    val idsDiff = intListElementDiff(expectedIDs.toSeq, vtkBlock.vertexIDs.toSeq)
    assert(idsDiff.filter(_ > NumericUtils.EPSILON).isEmpty)
  }

  test("Connectivities for each face should be in terms of vertex ID's") {
    val vtkBlock = BlockVTK(unitCube)
    val faceConnectivities = Map(
      face1 -> List[Int](4, 5, 1, 0),
      face2 -> List[Int](3, 2, 6, 7),
      face3 -> List[Int](1, 5, 6, 2),
      face4 -> List[Int](4, 0, 3, 7),
      face5 -> List[Int](7, 6, 5, 4),
      face6 -> List[Int](0, 1, 2, 3)
    )
    assert(faceConnectivities == vtkBlock.connectivityMap)
  }

  test("Connectivity map should flatten into list of integers") {
    val vtkBlock = BlockVTK(unitCube)
    val faceConnectivities = Seq[Int](0, 1, 2, 3, 4, 5, 1, 0, 3, 2, 6, 7,
                                      4, 0, 3, 7, 7, 6, 5, 4, 1, 5, 6, 2)
    val connectionsDiff = intListElementDiff(faceConnectivities.toSeq, vtkBlock.connectivity.toSeq)
    assert(connectionsDiff.filter(_ > NumericUtils.EPSILON).isEmpty)
  }

  test("Number of faces should be 6") {
    val vtkBlock = BlockVTK(unitCube)
    assert(vtkBlock.faceCount == 6)
  }

  test("Offsets should be incremented by number of vertices on each face when iterating through list of faces") {
    val vtkBlock = BlockVTK(unitCube)
    val expectedOffsets = List(4, 8, 12, 16, 20, 24)
    val offsetDiff = intListElementDiff(expectedOffsets, vtkBlock.offsets)
    assert(offsetDiff.filter(_ > NumericUtils.EPSILON).isEmpty)
  }

  test("Normals tuples should flatten into list of integers") {
    val vtkBlock = BlockVTK(unitCube)
    val expectedNormals = Seq[Double](
      face6.a, face6.b, face6.c,
      face1.a, face1.b, face1.c,
      face2.a, face2.b, face2.c,
      face4.a, face4.b, face4.c,
      face5.a, face5.b, face5.c,
      face3.a, face3.b, face3.c
    ) 
    val normals = vtkBlock.normals
    val normalsDiff = doubleListElementDiff(expectedNormals, normals)
    assert(normalsDiff.filter(_ > NumericUtils.EPSILON).isEmpty)
  }
}
