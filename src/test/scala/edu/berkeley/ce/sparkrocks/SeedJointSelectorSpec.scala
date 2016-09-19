package edu.berkeley.ce.sparkrocks

import org.scalatest.FunSuite

class SeedJointSelectorSpec extends FunSuite {
  test("At least one joint should be selected as seed joint - should divide the unit cube in half") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](1.0, 1.0, 1.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 1.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(1.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 1.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedBlocks, _) = SeedJointSelector.generateSeedBlocks(generatedInput.jointSets, initialBlock, 2)

    assert(seedBlocks.length >= 2)
    assert(seedBlocks forall { block =>
      math.abs(block.volume - 1.0/seedBlocks.length) < SeedJointSelector.THRESHOLD*1.0/seedBlocks.length
    })
  }

  test("At least one joint should be selected as seed joint - should divide the two cube") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedBlocks, _) = SeedJointSelector.generateSeedBlocks(generatedInput.jointSets, initialBlock, 2)

    assert(seedBlocks.length >= 2)
    assert(seedBlocks forall { block =>
      math.abs(block.volume - 8.0/seedBlocks.length) < SeedJointSelector.THRESHOLD*8.0/seedBlocks.length
    })
  }

  test("At least two seed joints should be selected - x-z planes dividing the two cube") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 2.0/3.0, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedBlocks, _) = SeedJointSelector.generateSeedBlocks(generatedInput.jointSets, initialBlock, 3)

    assert(seedBlocks.length >= 3)
    assert(seedBlocks forall { block =>
      math.abs(block.volume - 8.0/seedBlocks.length) < SeedJointSelector.THRESHOLD*8.0/seedBlocks.length
    })
  }

  test("At least three seed joints should be selected - x-z planes dividing two cube") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedBlocks, _) = SeedJointSelector.generateSeedBlocks(generatedInput.jointSets, initialBlock, 4)

    assert(seedBlocks.length >= 4)
    assert(seedBlocks forall { block =>
      math.abs(block.volume - 8.0/seedBlocks.length) < SeedJointSelector.THRESHOLD*8.0/seedBlocks.length
    })
  }

  test("At least 19 seed joints should be selected - x-z planes dividing two cube") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.02, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedBlocks, _) = SeedJointSelector.generateSeedBlocks(generatedInput.jointSets, initialBlock, 20)

    assert(seedBlocks.length >= 20)
  }

  test("At least 14 seed joints should be selected - pi planes " +
    "dividing two cube") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(45.0, 45.0, 0.01, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedBlocks, _) = SeedJointSelector.generateSeedBlocks(generatedInput.jointSets, initialBlock, 20)

    assert(seedBlocks.length >= 15)
  }

  test("Seed joint test on first Spaulding joint set") {
    val globalOrigin = Array[Double](0.0, 0.0, 0.0)
    val boundingBox = (Array[Double](-5.0, -5.0, -5.0), Array[Double](5.0, 5.0, 5.0))
    val rockVolume = Seq[InputFace](
      InputFace(90.0, 90.0, Array(4.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 4.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 4.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(-4.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, -4.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, -4.0), 30.0, 0.0)
    )
    val jointData = Seq[JointSet](
      JointSet(60.0, 25.0, 0.5, 100.0, 30.0, 0.0)
    )
    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedBlocks, _) = SeedJointSelector.generateSeedBlocks(generatedInput.jointSets, initialBlock, 3)

    assert(seedBlocks.length >= 3)
  }

  test("Select 2 seed joints from multiple joint sets") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )

    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0),
      JointSet(0.0, 0.0, 0.5, 100.0, 30.0, 0.0)
    )

    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedBlocks, _) = SeedJointSelector.generateSeedBlocks(generatedInput.jointSets, initialBlock, 4)

    assert(seedBlocks.length >=4)
  }

  test("Generate at least 8 partitions from multiple joint sets") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )

    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 0.5, 100.0, 30.0, 0.0),
      JointSet(0.0, 0.0, 0.5, 100.0, 30.0, 0.0)
    )

    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedBlocks, _) = SeedJointSelector.generateSeedBlocks(generatedInput.jointSets, initialBlock, 8)

    assert(seedBlocks.length >= 8)
  }

  test("Select seed joints from multiple joint sets") {
    val globalOrigin = Array[Double](0.5, 0.5, 0.5)
    val boundingBox = (Array[Double](0.0, 0.0, 0.0), Array[Double](2.0, 2.0, 2.0))
    val rockVolume = Seq[InputFace](
      InputFace(0.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 90.0, Array(0.0, 2.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(90.0, 90.0, Array(2.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 0.0), 30.0, 0.0),
      InputFace(0.0, 0.0, Array(0.0, 0.0, 2.0), 30.0, 0.0)
    )

    val jointData = Seq[JointSet](
      JointSet(0.0, 90.0, 1.0, 100.0, 30.0, 0.0),
      JointSet(0.0, 0.0, 1.0, 100.0, 30.0, 0.0)
    )

    val generatedInput = JointGenerator(globalOrigin, boundingBox, rockVolume, jointData)
    val initialBlock = Block(generatedInput.origin, generatedInput.rockVolume)
    val (seedBlocks, _) = SeedJointSelector.generateSeedBlocks(generatedInput.jointSets, initialBlock, 4)

    assert(seedBlocks.length >=4)
  }
}