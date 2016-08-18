package edu.berkeley.ce.sparkrocks

import org.scalatest._

import scala.math.sqrt

class BlockSpec extends FunSuite {
  val unitCubeFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x = 0
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),  // x = 1
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y = 0
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),  // y = 1
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z = 0
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)   // z = 1
  )
  val unitCube = Block(Array(0.0, 0.0, 0.0), unitCubeFaces)

  val twoCubeFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x = 0
    Face(Array(1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0), // x = 2
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y = 0
    Face(Array(0.0, 1.0, 0.0), 2.0, phi=0, cohesion=0), // y = 2
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z = 0
    Face(Array(0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0) // z = 2
  )
  val twoCube = Block(Array(0.0, 0.0, 0.0), twoCubeFaces)

  val twoCubeNonOriginFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0), // -x = 1
    Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),  // x = 1
    Face(Array(0.0, -1.0, 0.0), 1.0, phi=0, cohesion=0), // -y = 1
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),  // y = 1
    Face(Array(0.0, 0.0, -1.0), 1.0, phi=0, cohesion=0), // -z = 1
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)   // z = 1
  )
  val twoCubeNonOrigin = Block(Array(1.0, 1.0, 1.0), twoCubeNonOriginFaces)

  val unitCubeNonOriginFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.5, phi=0, cohesion=0), // -x = 0
    Face(Array(1.0, 0.0, 0.0), 0.5, phi=0, cohesion=0),  // x = 1
    Face(Array(0.0, -1.0, 0.0), 0.5, phi=0, cohesion=0), // -y = 0
    Face(Array(0.0, 1.0, 0.0), 0.5, phi=0, cohesion=0),  // y = 1
    Face(Array(0.0, 0.0, -1.0), 0.5, phi=0, cohesion=0), // -z = 0
    Face(Array(0.0, 0.0, 1.0), 0.5, phi=0, cohesion=0)   // z = 1
  )
  val unitCubeNonOrigin = Block(Array(0.5, 0.5, 0.5), unitCubeNonOriginFaces)

  val rectPrismFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0), // -x = 0
    Face(Array(1.0, 0.0, 0.0), 0.5, phi=0, cohesion=0), // x = 0.5
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0), // -y = 0
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0), // y = 1
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0), // -z = 0
    Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0) // z = 1
  )
  val rectPrism = Block(Array(0.0, 0.0, 0.0), rectPrismFaces)

  val tetrahedralFaces = List(
    Face(Array(1/math.sqrt(3.0), 1/math.sqrt(3.0), 1/math.sqrt(3.0)), 0.0, phi=0, cohesion=0),
    Face(Array(-1.0, 0.0, 0.0), 0.3, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.3, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, -1.0), 0.3, phi=0, cohesion=0)
  )
  val tetrahedralBlock = Block(Array(0.3, 0.3, 0.3), tetrahedralFaces)

  val tinyBlockFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi = 0.0, cohesion = 0.0),
    Face(Array(0.0, -1.0, 0.0), 0.0, phi = 0.0, cohesion = 0.0),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi = 0.0, cohesion = 0.0),
    Face(Array(1.0, 0.0, 0.0), 1e-6, phi = 0.0, cohesion = 0.0),
    Face(Array(0.0, 1.0, 0.0), 1e-6, phi = 0.0, cohesion = 0.0),
    Face(Array(0.0, 0.0, 1.0), 1e-6, phi = 0.0, cohesion = 0.0)
  )
  val tinyBlock = Block(Array(0.0, 0.0, 0.0), tinyBlockFaces)

  val lessTinyBlockFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi = 0.0, cohesion = 0.0),
    Face(Array(0.0, -1.0, 0.0), 0.0, phi = 0.0, cohesion = 0.0),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi = 0.0, cohesion = 0.0),
    Face(Array(1.0, 0.0, 0.0), 0.009, phi = 0.0, cohesion = 0.0),
    Face(Array(0.0, 1.0, 0.0), 0.009, phi = 0.0, cohesion = 0.0),
    Face(Array(0.0, 0.0, 1.0), 0.009, phi = 0.0, cohesion = 0.0)
  )
  val lessTinyBlock = Block(Array(0.0, 0.0, 0.0), lessTinyBlockFaces)

  val jointBounds = Vector(
    (Array(1.0, 0.0, 0.0), 1.0),
    (Array(-1.0, 0.0, 0.0), 0.0),
    (Array(0.0, 1.0, 0.0), 1.0),
    (Array(0.0, -1.0, 0.0), 0.0)
  )

  val jointBounds2 = Vector(
    (Array(1.0, 0.0, 0.0), 1.0),
    (Array(-1.0, 0.0, 0.0), 1.0),
    (Array(0.0, 1.0, 0.0), 1.0),
    (Array(0.0, -1.0, 0.0), 1.0)
  )

  val jointBounds3 = Vector(
    (Array(1.0/sqrt(2.0), 1.0/sqrt(2.0), 0.0), 1.0),
    (Array(-1.0/sqrt(2.0), 1.0/sqrt(2.0), 0.0), 1.0),
    (Array(-1.0/sqrt(2.0), -1.0/sqrt(2.0), 0.0), 1.0),
    (Array(1.0/sqrt(2.0), -1.0/sqrt(2.0), 0.0), 1.0)
  )

  val halfUnitCubeFaces = List(
    Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
    Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),
    Face(Array(1 / sqrt(2.0), 0.0, 1 / sqrt(2.0)), 1 / sqrt(2.0), phi=0, cohesion=0)
  )
  val halfUnitCube = Block(Array(0.0, 0.0, 0.0), halfUnitCubeFaces)

  def centroidDifference(c1: Array[Double], c2: Array[Double]): Double = {
    assert(c1.length == 3 && c2.length == 3)
    math.abs(c1(0) - c2(0)) + math.abs(c1(1) - c2(1)) + math.abs(c1(2) - c2(2))
  }

  test("The plane z = 0.5 should intersect the unit cube") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0, 0.0, 1/2.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isDefined)
  }

  test("The plane z = 0.5 with non-zero origin should intersect the unit cube") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,-1/4), center=Array(0.0, 0.0, 1/4.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isDefined)
  }

  test("The plane -z = 0.5 should intersect the unit cube") {
    val joint = Joint(Array(0.0, 0.0, -1.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0, 0.0, 1/2.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isDefined)
  }

  test("The plane -z = 0.5 with non-zero origin should intersect the unit cube") {
    val joint = Joint(Array(0.0, 0.0, -1.0), localOrigin=Array(0.0,0.0,-1/4), center=Array(0.0, 0.0, 1/4.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isDefined)
  }

  test("The plane z = 2 should not intersect the unit cube") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0, 0.0, 2.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isEmpty)
  }

  test("The plane z = 1 with non-zero origin should not intersect the unit cube") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0,0.0,1.0), center=Array(0.0, 0.0, 2.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isEmpty)
  }

  test("The plane y = 1 should not intersect the unit cube") {
    val joint = Joint(Array(0.0, 1.0, 0.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0, 1.0, 0.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isEmpty)
  }

  test("The plane y = 1/2.0 with non-zero origin should not intersect the unit cube") {
    val joint = Joint(Array(0.0, 1.0, 0.0), localOrigin=Array(0.0,1/2.0,0.0), center=Array(0.0, 1.0, 0.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    val translatedJoint = joint.updateJoint(Array(0.0, 0.0, 0.0))
    assert(unitCube.intersects(translatedJoint).isEmpty)
  }

  test("The plane -y = 1 should not intersect the unit cube") {
    val joint = Joint(Array(0.0, -1.0, 0.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0, 1.0, 0.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isEmpty)
  }

  test("The plane y = 0.99 should intersect the unit cube") {
    val joint = Joint(Array(0.0, 1.0, 0.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0, 0.99, 0.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isDefined)
  }

  test("The plane y = 0.49 with non-zero origin should intersect the unit cube") {
    val joint = Joint(Array(0.0, 1.0, 0.0), localOrigin=Array(0.0,0.5,0.0), center=Array(0.0, 0.99, 0.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isDefined)
  }

  test("The plane -y = 0.99 should intersect the unit cube") {
    val joint = Joint(Array(0.0, -1.0, 0.0), localOrigin=Array(0.0,0.0,0.0), center=Array(0.0, 0.99, 0.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isDefined)
  }

  test("The plane -y = 0.49 with non-zero origin should intersect the unit cube") {
    val joint = Joint(Array(0.0, -1.0, 0.0), localOrigin=Array(0.0,0.5,0.0), center=Array(0.0, 0.99, 0.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isDefined)
  }

  test("The plane x/sqrt(2.0) - z/sqrt(2.0) = 1 at non-zero origin should intersect the unit cube") {
    val joint = Joint(Array(1.0/sqrt(2.0), 0.0, -1.0/sqrt(2.0)), localOrigin=Array(-1.0/sqrt(2.0),0.0,1.0/sqrt(2.0)),
                      center=Array(0.0, 0.0, 0.0), phi=0, cohesion=0, shape=Vector.empty)
    val translatedJoint = joint.updateJoint(Array(0.0, 0.0, 0.0))
    assert(unitCube.intersects(translatedJoint).isDefined)
  }

  test("The plane x/sqrt(2.0) - z/sqrt(2.0) = 1 at Array(0.0,0.0,1.0) should not intersect the unit cube") {
    val joint = Joint(Array(1.0/sqrt(2.0), 0.0, -1.0/sqrt(2.0)), localOrigin=Array(-1.0/sqrt(2.0),0.0,1.0 + 1.0/sqrt(2.0)),
                      center=Array(0.0, 0.0, 1.0),  phi=0, cohesion=0, shape=Vector.empty)
    assert(unitCube.intersects(joint).isEmpty)
  }

  test("The plane x/sqrt(2.0) - z/sqrt(2.0) = 1 at global origin should intersect the two cube") {
    val joint = Joint(Array(1.0/sqrt(2.0), 0.0, -1.0/sqrt(2.0)), localOrigin=Array(-1.0/sqrt(2.0),0.0,1.0/sqrt(2.0)),
                       center=Array(0.0, 0.0, 0.0), phi=0, cohesion=0, shape=Vector.empty)
    assert(twoCube.intersects(joint).isDefined)
  }

  test("The plane x/sqrt(2.0) - z/sqrt(2.0) = 1 at Array(0.0,0.0,1.0) should intersect the two cube") {
    val joint = Joint(Array(1.0/sqrt(2.0), 0.0, -1.0/sqrt(2.0)), localOrigin=Array(-1.0/sqrt(2.0),0.0,1.0 + 1.0/sqrt(2.0)),
                       center=Array(0.0, 0.0, 1.0),  phi=0, cohesion=0, shape=Vector.empty)
    assert(twoCube.intersects(joint).isDefined)
  }

  test("The non-persistent joint z < 1.0 should not intersect the two cube") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0, 0.0, 0.0), center=Array(2.0, 0.0, 1.0),
                      phi=0.0, cohesion=0.0, shape=jointBounds)
    assert(twoCube.intersects(joint).isEmpty)
  }

  test("The non-persistent joint z < 1.0 should intersect the two cube") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0, 0.0, 0.0), center=Array(1.99, 0.01, 1.0),
                      phi=0.0, cohesion=0.0, shape=jointBounds)
    assert(twoCube.intersects(joint).isDefined)
  }

  test("The non-persistent joint z < 1.0 with non-global origin should not intersect the two cube") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(3.0, 0.0, 0.0), center=Array(3.0, 0.0, 1.0),
                      phi=0.0, cohesion=0.0, shape=jointBounds2)
    val translatedJoint = joint.updateJoint(Array(0.0, 0.0, 0.0))
    assert(twoCube.intersects(translatedJoint).isEmpty)
  }

  test("The non-persistent joint z < 1.0 with non-global origin should intersect the two cube") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(3.0, 0.0, 0.0), center=Array(2.99, 0.01, 1.0),
                      phi=0.0, cohesion=0.0, shape=jointBounds2)
    assert(twoCube.intersects(joint).isDefined)
  }

  test("The non-persistent joint z < 0 with rotated bounds should not intersect the unit cube") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0, 0.0, 0.0), center=Array(2.0, 2.0, 0.5),
                      phi=0.0, cohesion=0.0, shape=jointBounds3)
    assert(unitCube.intersects(joint).isEmpty)
  }

  test("The non-persistent joint z < 0 with rotated bounds should JUST not intersect the unit cube") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0, 0.0, 0.0),
                      center=Array(1.0 + 1.0/sqrt(2.0), 1.0 + 1.0/sqrt(2.0), 0.5),
                      phi=0.0, cohesion=0.0, shape=jointBounds3)
    assert(unitCube.intersects(joint).isEmpty)
  }

  test("The non-persistent joint z < 0 with rotated bounds should intersect the unit cube") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0, 0.0, 0.0),
                      center=Array(1.0 + 1.0/sqrt(2.0), 0.99 + 1.0/sqrt(2.0), 0.5),
                      phi=0.0, cohesion=0.0, shape=jointBounds3)
    assert(unitCube.intersects(joint).isDefined)
  }

  test("The non-persistent joint z < 0 should not intersect the non-global origin two cube ") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(1.0, 1.0, 1.0), center=Array(1.0, 3.0, 1.0),
                      phi=0.0, cohesion=0.0, shape=jointBounds)
    assert(twoCubeNonOrigin.intersects(joint).isEmpty)
  }

  test("The non-persistent joint z < 0 should intersect the non-global origin two cube ") {
    val joint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(1.0, 1.0, 1.0), center=Array(1.99, 2.99, 1.0),
      phi=0.0, cohesion=0.0, shape=jointBounds)
    assert(twoCubeNonOrigin.intersects(joint).isDefined)
  }

  test("The unit cube should not contain any redundant faces") {
    assert(unitCube.nonRedundantFaces == unitCubeFaces)
  }

  test("Adding planes x,y,z = +- 2 to the unit cube should be considered redundant") {
    val redundantBoundingFaces = unitCubeFaces ++ List(
      Face(Array(-1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0), // -x = 2
      Face(Array(1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0), // x = 2
      Face(Array(0.0, -1.0, 0.0), 2.0, phi=0, cohesion=0), // -y = 2
      Face(Array(0.0, 1.0, 0.0), 2.0, phi=0, cohesion=0), // y = 2
      Face(Array(0.0, 0.0, -1.0), 2.0, phi=0, cohesion=0), // -z = 2
      Face(Array(0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0) // z = 2
    )
    val redundantUnitCube = Block(Array(0.0, 0.0, 0.0), redundantBoundingFaces)

    assert(redundantUnitCube.nonRedundantFaces == unitCubeFaces)
  }

  test("The two cube should not contain any redundant faces") {
    assert(twoCube.nonRedundantFaces == twoCubeFaces)
  }

  test("Adding the boundary -x + 2z = 0 to the two cube should render z = 2 redundant") {
    val newFace = Face(Array(-1.0, 0.0, 2.0), 0.0, phi=0, cohesion=0)
    val redundantBoundingFaces = newFace::twoCubeFaces
    val redundant2Cube = Block(Array(1.0, 1.0, 1.0), redundantBoundingFaces)
    val expectedFaces = redundantBoundingFaces.slice(0, redundantBoundingFaces.length - 1)
    assert(redundant2Cube.nonRedundantFaces == expectedFaces)
  }

  test("Moving origin of two-cube to (1,0,0) should only affect x-distances") {
    val newFaces = twoCube.updateFaces(Array(1.0, 0.0, 0.0))
    val newBlock = Block(Array(1.0, 0.0, 0.0), newFaces)
    val testFaces = newBlock.nonRedundantFaces
    val expectedFaces = List(
      Face(Array(-1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
      Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
      Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0),
      Face(Array(0.0, 1.0, 0.0), 2.0, phi=0, cohesion=0),
      Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0),
      Face(Array(0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0)
    )
    assert(testFaces == expectedFaces)
  }

  test("Moving origin of two-cube to (1,1,1) should affect all distances") {
    val newFaces = twoCube.updateFaces(Array(1.0, 1.0, 1.0))
    val newBlock = Block(Array(1.0, 1.0, 1.0), newFaces)
    val testFaces = newBlock.nonRedundantFaces
    val expectedFaces = List(
      Face(Array(-1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
      Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0),
      Face(Array(0.0, -1.0, 0.0), 1.0, phi=0, cohesion=0),
      Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0),
      Face(Array(0.0, 0.0, -1.0), 1.0, phi=0, cohesion=0),
      Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
    )
    assert(testFaces == expectedFaces)
  }

  test("Adding the face -x=1 to a two-cube centered at origin should be considered redundant") {
    val redundantFace = Face(Array(-1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0)
    val newTwoCube = Block(Array(0.0, 0.0, 0.0), redundantFace::twoCubeFaces)
    val testFaces = newTwoCube.nonRedundantFaces
    assert(testFaces == twoCubeFaces)
  }

  test("The points of intersection between the four planes should Array(0.0, 0.0, 0.0) & Array(0.0, 5.0, 0.0)") {
    val face1 = Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0)
    val face2 = Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0)
    val face3 = Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0)
    val face4 = Face(Array(0.0, 1.0, 0.0), 5.0, phi=0, cohesion=0)
    val block = Block(Array(0.0, 0.0, 0.0), List(face1, face2, face3, face4))
    val face1Verts = List(Array(0.0, 0.0, 0.0), Array(0.0, 5.0, 0.0))
    val face2Verts = List(Array(0.0, 0.0, 0.0))
    val face3Verts = List(Array(0.0, 0.0, 0.0), Array(0.0, 5.0, 0.0))
    val face4Verts = List(Array(0.0, 5.0, 0.0))
    val expectedIntersection = Map(
      face1 -> face1Verts,
      face2 -> face2Verts,
      face3 -> face3Verts,
      face4 -> face4Verts
    )
    val vertices = block.calcVertices
    assert(vertices.keys == expectedIntersection.keys && vertices.keys.forall { key =>
      vertices.get(key).get.zip(expectedIntersection.get(key).get) forall { case (v1, v2) => v1 sameElements v2 }
    })
  }

  test("The point of intersection between the three planes should be (1.0, 0.0, 0.0)") {
    val face1 = Face(Array(0.0, 0.0, 1.0), 0.0, phi = 0.0, cohesion = 0.0)
    val face2 = Face(Array(0.0, 1.0, 0.0), 0.0, phi = 0.0, cohesion = 0.0)
    val face3 = Face(Array(1.0/sqrt(2.0), 1.0/sqrt(2.0), 0.0), 1.0/sqrt(2.0), phi = 0.0, cohesion = 0.0)
    val block = Block(Array(0.0, 0.0, 0.0), List(face1, face2, face3))
    val face1Verts = List(Array(1.0, 0.0, 0.0))
    val face2Verts = List(Array(1.0, 0.0, 0.0))
    val face3Verts = List(Array(1.0, 0.0, 0.0))
    val expectedIntersection = Map(
      face1 -> face1Verts,
      face2 -> face2Verts,
      face3 -> face3Verts
    )
    val vertices = block.calcVertices
    assert(vertices.keys == expectedIntersection.keys && vertices.keys.forall { key =>
      vertices.get(key).get.zip(expectedIntersection.get(key).get) forall { case (v1, v2) => v1 sameElements v2 }
    })
  }

  test("There should be no intersection between the planes") {
    val face1 = Face(Array(1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0)
    val face2 = Face(Array(1.0, 0.0, 0.0), 5.0, phi=0, cohesion=0)
    val face3 = Face(Array(0.0, 0.0, 1.0), 0.0, phi=0, cohesion=0)
    val block = Block(Array(1.0, 1.0, 1.0), List(face1, face2, face3))
    val expectedIntersection = Map(
      face1 -> List.empty[Array[Double]],
      face2 -> List.empty[Array[Double]],
      face3 -> List.empty[Array[Double]]
    )
    val vertices = block.calcVertices
    assert(vertices.keys == expectedIntersection.keys && vertices.keys.forall { key =>
      vertices.get(key).get.zip(expectedIntersection.get(key).get) forall { case (v1, v2) => v1 sameElements v2 }
    })
  }

  test("Centroid should be at Array(0.0, 0.0, 0.0)") {
    val face1 = Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0)
    val face2 = Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0)
    val face3 = Face(Array(0.0, 0.0, 1.0), 1.0, phi=0, cohesion=0)
    val face4 = Face(Array(-1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0)
    val face5 = Face(Array(0.0, -1.0, 0.0), 1.0, phi=0, cohesion=0)
    val face6 = Face(Array(0.0, 0.0, -1.0), 1.0, phi=0, cohesion=0)
    val block = Block(Array(0.0, 0.0, 0.0), List(face1, face2, face3, face4, face5, face6))
    val centroid = block.centroid
    val expectedCentroid = Array(0.0, 0.0, 0.0)
    assert(centroidDifference(centroid, expectedCentroid) <= NumericUtils.EPSILON)
  }

  test("Centroid should be at (0.5, 0.5, 1.0)") {
    val face1 = Face(Array(1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0)
    val face2 = Face(Array(0.0, 1.0, 0.0), 1.0, phi=0, cohesion=0)
    val face3 = Face(Array(0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0)
    val face4 = Face(Array(-1.0, 0.0, 0.0), 1.0, phi=0, cohesion=0)
    val face5 = Face(Array(0.0, -1.0, 0.0), 1.0, phi=0, cohesion=0)
    val face6 = Face(Array(0.0, 0.0, -1.0), 1.0, phi=0, cohesion=0)
    val block = Block(Array(0.5, 0.5, 0.5), List(face1, face2, face3, face4, face5, face6))
    val centroid = block.centroid
    val expectedCentroid = Array(0.5, 0.5, 1.0)
    assert(centroidDifference(centroid, expectedCentroid) < NumericUtils.EPSILON)
  }

  test("Centroid should be at Array(-1.0, -1.0, -1.0)") {
    val face1 = Face(Array(1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0)
    val face2 = Face(Array(0.0, 1.0, 0.0), 0.0, phi=0, cohesion=0)
    val face3 = Face(Array(0.0, 0.0, 1.0), 0.0, phi=0, cohesion=0)
    val face4 = Face(Array(-1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0)
    val face5 = Face(Array(0.0, -1.0, 0.0), 2.0, phi=0, cohesion=0)
    val face6 = Face(Array(0.0, 0.0, -1.0), 2.0, phi=0, cohesion=0)
    val block = Block(Array(0.0, 0.0, 0.0), List(face1, face2, face3, face4, face5, face6))
    val centroid = block.centroid
    val expectedCentroid = Array(-1.0, -1.0, -1.0)
    assert(centroidDifference(centroid, expectedCentroid) < NumericUtils.EPSILON)
  }

  test("New distances should be shifted based on input local origin") {
    val face1 = Face(Array(1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0)
    val face2 = Face(Array(0.0, 1.0, 0.0), 2.0, phi=0, cohesion=0)
    val face3 = Face(Array(0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0)
    val face4 = Face(Array(-1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0)
    val face5 = Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0)
    val face6 = Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0)
    val block = Block(Array(0.0, 0.0, 0.0), List(face1, face2, face3, face4, face5, face6))
    val updatedFaces = block.updateFaces(Array(2.0, 0.0, 0.0))
    val expectedFace1 = Face(Array(1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0)
    val expectedFace2 = Face(Array(0.0, 1.0, 0.0), 2.0, phi=0, cohesion=0)
    val expectedFace3 = Face(Array(0.0, 0.0, 1.0), 2.0, phi=0, cohesion=0)
    val expectedFace4 = Face(Array(-1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0)
    val expectedFace5 = Face(Array(0.0, -1.0, 0.0), 0.0, phi=0, cohesion=0)
    val expectedFace6 = Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0)
    val expectedFaces = List(expectedFace1, expectedFace2, expectedFace3, expectedFace4, expectedFace5, expectedFace6)
    assert(updatedFaces == expectedFaces)
  }

  test("New distances should be shifted based on input local origin - some distances should be negative") {
    val face1 = Face(Array(1.0, 0.0, 0.0), 2.0, phi=0, cohesion=0)
    val face2 = Face(Array(0.0, 1.0, 0.0), 2.0, phi=0, cohesion=0)
    val face3 = Face(Array(0.0, 0.0, -1.0), -2.0, phi=0, cohesion=0)
    val face4 = Face(Array(1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0)
    val face5 = Face(Array(0.0, 1.0, 0.0), 0.0, phi=0, cohesion=0)
    val face6 = Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0)
    val block = Block(Array(0.0, 0.0, 0.0), List(face1, face2, face3, face4, face5, face6))
    val updatedFaces = block.updateFaces(Array(2.0, 2.0, 2.0))
    val expectedFace1 = Face(Array(1.0, 0.0, 0.0), 0.0, phi=0, cohesion=0)
    val expectedFace2 = Face(Array(0.0, 1.0, 0.0), 0.0, phi=0, cohesion=0)
    val expectedFace3 = Face(Array(0.0, 0.0, -1.0), 0.0, phi=0, cohesion=0)
    val expectedFace4 = Face(Array(1.0, 0.0, 0.0), -2.0, phi=0, cohesion=0)
    val expectedFace5 = Face(Array(0.0, 1.0, 0.0), -2.0, phi=0, cohesion=0)
    val expectedFace6 = Face(Array(0.0, 0.0, -1.0), 2.0, phi=0, cohesion=0)
    val expectedFaces = List(expectedFace1, expectedFace2, expectedFace3, expectedFace4, expectedFace5, expectedFace6)
    assert(updatedFaces == expectedFaces)
  }

  test("Cutting the two-cube with faces x=0 and z=0 should produce four blocks") {
    val xPlane = Joint(Array(1.0,0.0,0.0), Array(1.0,1.0,1.0), Array(1.0,1.0,1.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    val zPlane = Joint(Array(0.0,0.0,1.0), Array(1.0, 1.0, 1.0), Array(1.0,1.0,1.0),
                       phi=0, cohesion=0, shape=Vector.empty)
    val xBlocks = twoCube.cut(xPlane)
    val xzBlocks = xBlocks.flatMap(_.cut(zPlane))

    val nonRedundantBlocks = xzBlocks.map { case block @ Block(center, faces, _) =>
      val nonRedundantFaces = block.nonRedundantFaces
      Block(center, nonRedundantFaces)
    }

    val centroidBlocks = nonRedundantBlocks.map { case block @ Block(center, _, _) =>
      val centroid = block.centroid
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, updatedFaces)
    }

    val cleanedBlocks = centroidBlocks.map { case Block(center, faces, _) =>
      Block(center, faces.map(_.applyTolerance))
    }

    // Names describe block positions when locking from fourth octant to first octant
    val bottomLeft = Block(Array(0.5, 1.0, 0.5), List(
      Face(Array(0.0, 0.0, 1.0), 0.5, 0.0, 0.0),
      Face(Array(1.0, 0.0, 0.0), 0.5, 0.0, 0.0),
      Face(Array(-1.0, 0.0, 0.0), 0.5, 0.0, 0.0),
      Face(Array(0.0, -1.0, 0.0), 1.0, 0.0, 0.0),
      Face(Array(0.0, 1.0, 0.0), 1.0, 0.0, 0.0),
      Face(Array(0.0, 0.0, -1.0), 0.5, 0.0, 0.0)
    ))

    val topLeft = Block(Array(0.5, 1.0, 1.5), List(
      Face(Array(0.0, 0.0, -1.0), 0.5, 0.0, 0.0),
      Face(Array(1.0, 0.0, 0.0), 0.5, 0.0, 0.0),
      Face(Array(-1.0, 0.0, 0.0), 0.5, 0.0, 0.0),
      Face(Array(0.0, -1.0, 0.0), 1.0, 0.0, 0.0),
      Face(Array(0.0, 1.0, 0.0), 1.0, 0.0, 0.0),
      Face(Array(0.0, 0.0, 1.0), 0.5, 0.0, 0.0)
    ))

    val bottomRight = Block(Array(1.5, 1.0, 0.5), List(
      Face(Array(0.0, 0.0, 1.0), 0.5, 0.0, 0.0),
      Face(Array(-1.0, 0.0, 0.0), 0.5, 0.0, 0.0),
      Face(Array(1.0, 0.0, 0.0), 0.5, 0.0, 0.0),
      Face(Array(0.0, -1.0, 0.0), 1.0, 0.0, 0.0),
      Face(Array(0.0, 1.0, 0.0), 1.0, 0.0, 0.0),
      Face(Array(0.0, 0.0, -1.0), 0.5, 0.0, 0.0)
    ))

    val topRight = Block(Array(1.5, 1.0, 1.5), List(
      Face(Array(0.0, 0.0, -1.0), 0.5, 0.0, 0.0),
      Face(Array(-1.0, 0.0, 0.0), 0.5, 0.0, 0.0),
      Face(Array(1.0, 0.0, 0.0), 0.5, 0.0, 0.0),
      Face(Array(0.0, -1.0, 0.0), 1.0, 0.0, 0.0),
      Face(Array(0.0, 1.0, 0.0), 1.0, 0.0, 0.0),
      Face(Array(0.0, 0.0, 1.0), 0.5, 0.0, 0.0)
    ))

    val expectedBlocks = List(bottomLeft, topLeft, bottomRight, topRight)
    assert(cleanedBlocks == expectedBlocks)
  }

  test("Bounding sphere of the unit cube should have center (0.5, 0.5, 0.5) and radius sqrt(0.5 + 0.5^2)") {
    val expectedBoundingSphere = ((0.5, 0.5, 0.5), math.sqrt(0.5 + math.pow(0.5, 2)))
    val unitCubeBS = ((unitCube.sphereCenterX, unitCube.sphereCenterY, unitCube.sphereCenterZ),
                       unitCube.sphereRadius)
    assert(unitCubeBS == expectedBoundingSphere)
  }

  test("Bounding sphere of the two cube should have center (1.0, 1.0, 1.0) and radius sqrt(3.0)") {
    val expectedBoundingSphere = ((1.0, 1.0, 1.0), math.sqrt(3.0))
    val twoCubeBS = ((twoCube.sphereCenterX, twoCube.sphereCenterY, twoCube.sphereCenterZ),
                     twoCube.sphereRadius)
    assert(twoCubeBS == expectedBoundingSphere)
  }

  test("Bounding sphere and radius for tetrahedral block should be (0.45, 0.45, 0.45) and radius sqrt(0.6075)") {
    val expectedBoundingSphere = (Array(0.45, 0.45, 0.45), math.sqrt(0.6075))
    val tetBlockBS = (Array(tetrahedralBlock.sphereCenterX, tetrahedralBlock.sphereCenterY,
                       tetrahedralBlock.sphereCenterZ), tetrahedralBlock.sphereRadius)
    assert(centroidDifference(expectedBoundingSphere._1, tetBlockBS._1) < NumericUtils.EPSILON)
    assert(expectedBoundingSphere._2 - tetBlockBS._2 < NumericUtils.EPSILON)
  }

  test("-z=-0.5 should intersect the two cube - negative joint distance check") {
    val joint = Joint(Array(0.0, 0.0, -1.0), localOrigin=Array(0.0, 0.0, 0.0), center=Array(0.0, 0.0, 1/2.0),
      phi=0, cohesion=0, shape=Vector.empty)
    assert(twoCube.intersects(joint).isDefined)
  }

  test("The unit cube should have an inscribable sphere with max radius 0.5") {
    assert(Math.abs(Math.abs(unitCube.maxInscribableRadius) - 0.5) <= NumericUtils.EPSILON)
  }

  test("The two cube should have an inscribable sphere with max radius 1.0") {
    assert(Math.abs(twoCube.maxInscribableRadius - 1.0) <= NumericUtils.EPSILON)
  }

  test("The two cube with non-zero origin should have an inscribable sphere with max radius 1.0") {
    assert(Math.abs(twoCubeNonOrigin.maxInscribableRadius - 1.0) <= NumericUtils.EPSILON)
  }

  test("Rectangular prism should have an inscribable sphere with max radius 0.25") {
    assert(Math.abs(rectPrism.maxInscribableRadius - 0.25) <= NumericUtils.EPSILON)
  }

  test("Cutting the unit cube at x=0.5 with a minimum inscribable radius of 0.25 should produce two children") {
    val cutJoint = Joint(Array(1.0, 0.0, 0.0), localOrigin=Array(0.0, 0.0, 0.0), center=Array(0.5, 0.0, 0.0),
      phi = 0, cohesion = 0, shape = Vector.empty)
    assert(unitCube.cut(cutJoint, minSize=0.25).length == 2)
  }

  test("Cutting the unit cube at x=0.5 with a minimum inscribable radius of 0.26 should produce no new children") {
    val cutJoint = Joint(Array(1.0, 0.0, 0.0), localOrigin=Array(0.0, 0.0, 0.0), center=Array(0.5, 0.0, 0.0),
      phi = 0, cohesion = 0, shape = Vector.empty)
    val children = unitCube.cut(cutJoint, minSize=0.26)
    assert(children.length == 1)
    assert(children.head == unitCube)
  }

  test("Cutting non-origin two-cube at z=1 with minimum inscribable radius of 0.5 should produce two new children") {
    val cutJoint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0, 0.0, 0.0), center=Array(0.0, 0.0, 1.0),
                         phi=0.0, cohesion=0.0, shape=Vector.empty)
    assert(twoCubeNonOrigin.cut(cutJoint, minSize=0.5).length == 2)
  }

  test("Cutting non-origin two-cube at z=1 with minimum inscribable radius of 0.6 should produce no new children") {
    val cutJoint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0, 0.0, 0.0), center=Array(0.0, 0.0, 1.0),
                         phi=0.0, cohesion=0.0, shape=Vector.empty)
    val children = twoCubeNonOrigin.cut(cutJoint, minSize=0.6)
    assert(children.length == 1)
    assert(children.head == twoCubeNonOrigin)
  }

  test("Cutting the unit cube at x=0.5 with maximum aspect ratio of 3 should produce two new children") {
    val cutJoint = Joint(Array(1.0, 0.0, 0.0), localOrigin=Array(0.0, 0.0, 0.0), center=Array(0.5, 0.0, 0.0),
      phi = 0, cohesion = 0, shape = Vector.empty)
    assert(unitCube.cut(cutJoint, maxAspectRatio=3.0).length == 2)
  }

  test("Cutting the unit cube at x=0.5 with maximum aspect ratio of 2.9 should produce no new children") {
    val cutJoint = Joint(Array(1.0, 0.0, 0.0), localOrigin=Array(0.0, 0.0, 0.0), center=Array(0.5, 0.0, 0.0),
      phi = 0, cohesion = 0, shape = Vector.empty)
    val children = unitCube.cut(cutJoint, maxAspectRatio=2.9)
    assert(children.length == 1)
    assert(children.head == unitCube)
  }

  test("Cutting non-origin two-cube at z=1 with maximum aspect ratio of 3 should produce two new children") {
    val cutJoint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0, 0.0, 0.0), center=Array(0.0, 0.0, 1.0),
      phi=0.0, cohesion=0.0, shape=Vector.empty)
    assert(twoCubeNonOrigin.cut(cutJoint, maxAspectRatio=3.0).length == 2)
  }

  test("Cutting non-origin two-cube at z=1 with maximum aspect ratio of 2.9 should produce no new children") {
    val cutJoint = Joint(Array(0.0, 0.0, 1.0), localOrigin=Array(0.0, 0.0, 0.0), center=Array(0.0, 0.0, 1.0),
      phi=0.0, cohesion=0.0, shape=Vector.empty)
    val children = twoCubeNonOrigin.cut(cutJoint, maxAspectRatio=2.9)
    assert(children.length == 1)
    assert(children.head == twoCubeNonOrigin)
  }

  test("Volume of unit cube should be 1.0") {
    val volume = unitCube.volume
    assert(math.abs(volume - 1.0) <= NumericUtils.EPSILON)
  }

  test("Volume of two cube should be 8.0") {
    val volume = twoCube.volume
    assert(math.abs(volume - 8.0) <= NumericUtils.EPSILON)
  }

  test("Volume of half unit cube should be 0.5") {
    val volume = halfUnitCube.volume
    assert(math.abs(volume - 0.5) <= NumericUtils.EPSILON)
  }

  test("Centroid of very small block should be the average of vertices") {
    val calculatedCentroid = tinyBlock.centroid
    val expectedCentroid = Array(1e-6/2.0, 1e-6/2.0, 1e-6/2.0)
    val centroidComparison = (calculatedCentroid.zip(expectedCentroid) map { case (entry1, entry2) =>
      math.abs(entry1 - entry2)
    }).max
    assert(centroidComparison < NumericUtils.EPSILON)
  }

  test("Centroid of less small block should the average of vertices") {
    val calculatedCentroid = lessTinyBlock.centroid
    val expectedCentroid = Array(0.009/2.0, 0.009/2.0, 0.009/2.0)
    val centroidComparison = (calculatedCentroid.zip(expectedCentroid) map { case (entry1, entry2) =>
      math.abs(entry1 - entry2)
    }).max
    assert(centroidComparison < NumericUtils.EPSILON)
  }
}
