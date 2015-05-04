import org.scalatest._
import edu.berkeley.ce.rockslicing.{Face, Block, Joint, Delaunay}

class BlockSpec extends FunSuite {
  val boundingFaces = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x = 0
    Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),  // x = 1
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y = 0
    Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),  // y = 1
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z = 0
    Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)   // z = 1
  )
  val unitCube = Block((1/2.0, 1/2.0, 1/2.0), boundingFaces)

  val boundingFaces2 = List(
    Face((-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x = 0
    Face((1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0),  // x = 2
    Face((0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y = 0
    Face((0.0, 1.0, 0.0), 2.0, phi=0, cohesion=0),  // y = 2
    Face((0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z = 0
    Face((0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0)   // z = 2
  )
  val twoCube = Block((1.0, 1.0, 1.0), boundingFaces2)

  test("The plane z = 0.5 should intersect the unit cube") {
    val jointShape = List(
      ((1.0, 0.0, 0.0), 2.0),
      ((-1.0, 0.0, 0.0), 2.0),
      ((0.0, 1.0, 0.0), 2.0),
      ((0.0, -1.0, 0.0), 2.0)
    )
    val joint = Joint((0.0, 0.0, 1.0), 1/2.0, center=(0.0, 0.0, 1/2.0), dipAngle=0,
                      dipDirection=0, phi=0, cohesion=0, shape=jointShape)
    assert(unitCube intersects joint)
  }

  test("The plane z = 2 should not intersect the unit cube") {
    val jointShape = List(
      ((1.0, 0.0, 0.0), 2.0),
      ((-1.0, 0.0, 0.0), 2.0),
      ((0.0, 1.0, 0.0), 2.0),
      ((0.0, -1.0, 0.0), 2.0)
    )
    val joint = Joint((0.0, 0.0, 1.0), 2.0, center=(0.0, 0.0, 2.0), dipAngle=0,
                      dipDirection=0, phi=0, cohesion=0, shape=jointShape)
    assert(!(unitCube intersects joint))
  }

  test("The plane y = 1 should not intersect the unit cube") {
    val jointShape = List(
      ((1.0, 0.0, 0.0), 2.0),
      ((-1.0, 0.0, 0.0), 2.0),
      ((0.0, 0.0, 1.0), 2.0),
      ((0.0, 0.0, -1.0), 2.0)
    )
    val joint = Joint((0.0, 1.0, 0.0), 1.0, center=(0.0, 0.0, 2.0), dipAngle=0,
                      dipDirection=0, phi=0, cohesion=0, shape=jointShape)
    assert(!(unitCube intersects joint))
  }

  test("The plane y = 0.99 should intersect the unit cube") {
    val jointShape = List(
      ((1.0, 0.0, 0.0), 2.0),
      ((-1.0, 0.0, 0.0), 2.0),
      ((0.0, 0.0, 1.0), 2.0),
      ((0.0, 0.0, -1.0), 2.0)
    )
    val joint = Joint((0.0, 1.0, 0.0), 0.99, center=(0.0, 0.0, 2.0), dipAngle=0,
                      dipDirection=0, phi=0, cohesion=0, shape=jointShape)
    assert(unitCube intersects joint)
  }

  test("The plane -x + z = 1 should not intersect the unit cube") {
    val jointShape = List(
      ((1.0, 0.0, 0.0), 4.0),
      ((-1.0, 0.0, 0.0), 4.0),
      ((0.0, 0.0, 1.0), 4.0),
      ((0.0, 0.0, -1.0), 4.0)
    )
    val joint = Joint((-1.0, 0.0, 1.0), 1.0, center=(0.0, 0.0, 1.0), dipAngle=0,
                      dipDirection=0, phi=0, cohesion=0, shape=jointShape)
    assert(!(unitCube intersects joint))
  }

  test("The plane -x + z = 1 should intersect the two cube") {
    val jointShape = List(
      ((1.0, 0.0, 0.0), 4.0),
      ((-1.0, 0.0, 0.0), 4.0),
      ((0.0, 0.0, 1.0), 4.0),
      ((0.0, 0.0, -1.0), 4.0)
    )
    val joint = Joint((-1.0, 0.0, 1.0), 1.0, center=(0.0, 0.0, 1.0), dipAngle=0,
                      dipDirection=0, phi=0, cohesion=0, shape=jointShape)
    assert(twoCube intersects joint)
  }

  test("The unit cube should not contain any redundant faces") {
    assert(unitCube.nonRedundantFaces == boundingFaces)
  }

  test("Adding planes x,y,z = +- 2 to the unit cube should be considered redundant") {
    val redundantBoundingFaces = boundingFaces ++ List(
      Face((-1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0), // -x = 2
      Face((1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0),  // x = 2
      Face((0.0, -1.0, 0.0), 2.0, phi=0, cohesion=0), // -y = 2
      Face((0.0, 1.0, 0.0), 2.0, phi=0, cohesion=0),  // y = 2
      Face((0.0, 0.0, -1.0), 2.0, phi=0, cohesion=0), // -z = 2
      Face((0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0)   // z = 2
    )
    val redundantUnitCube = Block((1/2.0, 1/2.0, 1/2.0), redundantBoundingFaces)

    assert(redundantUnitCube.nonRedundantFaces == boundingFaces)
  }

  test("The two cube should not contain any redundant faces") {
    assert(twoCube.nonRedundantFaces == boundingFaces2)
  }

  test("Adding the boundary -x + 2z = 0 to the two cube should render z = 2 redundant") {
    val newFace = Face((-1.0, 0.0, 2.0), 0.0, phi=0, cohesion=0)
    val redundantBoundingFaces = newFace::boundingFaces2
    val redundant2Cube = Block((1.0, 1.0, 1.0), redundantBoundingFaces)
    val expectedFaces = redundantBoundingFaces.slice(0, redundantBoundingFaces.length - 1)
    assert(redundant2Cube.nonRedundantFaces == expectedFaces)
  }

  test("The points of intersection between the four planes should (0.0, 0.0, 0.0) & (0.0, 1.0, 0.0)") {
    val testFace1 = Face((1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0)
    val testFace2 = Face((0.0, 1.0, 0.0), 0.0, phi=0, cohesion=0)
    val testFace3 = Face((0.0, 0.0, 1.0), 0.0, phi=0, cohesion=0)
    val testFace4 = Face((0.0, 1.0, 0.0), 5.0, phi=0, cohesion=0)
    val testBlock = Block((1.0, 1.0, 1.0), List[Face](testFace1, testFace2, testFace3, testFace4))
    val expectedIntersection = List((0.0, 5.0, 0.0),(0.0, 0.0, 0.0))
    testBlock.findVertices
    assert(testBlock.vertices == expectedIntersection)
  }

  test("There should be no intersection between the planes") {
    val testFace1 = Face((1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0)
    val testFace2 = Face((1.0, 0.0, 0.0), 5.0, phi=0, cohesion=0)
    val testFace3 = Face((0.0, 0.0, 1.0), 0.0, phi=0, cohesion=0)
    val testBlock = Block((1.0, 1.0, 1.0), List[Face](testFace1, testFace2, testFace3))
    val expectedIntersection = List.empty[(Double, Double, Double)]
    testBlock.findVertices
    assert(testBlock.vertices == expectedIntersection)
  }

  test("There should three entries in the triangulation list ordered in a clockwise fashion") {
    val testFace1 = Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0)
    val testFace2 = Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0)
    val testFace3 = Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
    val testFace4 = Face((1/math.sqrt(2.0), 1/math.sqrt(2.0), 0.0), 1/math.sqrt(2.0), phi=0, cohesion=0) 
    val testBlock = Block((1.0, 1.0, 1.0), List[Face](testFace1, testFace2, testFace3, testFace4))
    testBlock.findVertices
    testBlock.meshFaces
    val testVertices = List(Delaunay.Vector2(0.0, 1.0), Delaunay.Vector2(1.0,0.0), Delaunay.Vector2(1.0,1.0))
    val expectedTriangulation = (Delaunay.Triangulation(testVertices)).toList
    assert(testBlock.faces(2).triangles == expectedTriangulation)
  }

  test("Volume should be 8.0") {
    val testFace1 = Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0)
    val testFace2 = Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0)
    val testFace3 = Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
    val testFace4 = Face((-1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0)
    val testFace5 = Face((0.0, -1.0, 0.0), 1.0, phi=0, cohesion=0)
    val testFace6 = Face((0.0, 0.0, -1.0), 1.0, phi=0, cohesion=0)
    val testBlock = Block((1.0, 1.0, 1.0), List[Face](testFace1, testFace2, testFace3, testFace4, testFace5, testFace6))
    testBlock.findVertices
    testBlock.meshFaces
    testBlock.calcCentroidVolume
    val expectedVolume = 8.0
    assert(testBlock.volume == expectedVolume) 
  }

  test("Centroid should be at (0.0, 0.0, 0.0)") {
    val testFace1 = Face((1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0)
    val testFace2 = Face((0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0)
    val testFace3 = Face((0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
    val testFace4 = Face((-1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0)
    val testFace5 = Face((0.0, -1.0, 0.0), 1.0, phi=0, cohesion=0)
    val testFace6 = Face((0.0, 0.0, -1.0), 1.0, phi=0, cohesion=0)
    val testBlock = Block((1.0, 1.0, 1.0), List[Face](testFace1, testFace2, testFace3, testFace4, testFace5, testFace6))
    testBlock.findVertices
    testBlock.meshFaces
    testBlock.calcCentroidVolume
    val expectedCentroid = (0.0, 0.0, 0.0)
    assert((testBlock.centerX, testBlock.centerY, testBlock.centerZ) == expectedCentroid)
  }
}
