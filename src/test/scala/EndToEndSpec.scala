import scala.collection.mutable.ListBuffer
import scala.io.Source
import org.scalatest._
import edu.berkeley.ce.rockslicing._

class EndToEndSpec extends FunSuite {
  val INPUT_FILE_NAME = "endToEndData.txt"
  val OUTPUT_FILE_NAME = "blocks.json"

  test("Simple end-to-end test using unit cube and simple planes") {
    // Read input file to generate list of joints and initial rock block
    val rockBuffer = new ListBuffer[Face]()
    val jointBuffer = new ListBuffer[Joint]()
    val inputSource = Source.fromURL(getClass.getResource(s"/${INPUT_FILE_NAME}"))
    try {
      inputProcessor.readInput(inputSource, rockBuffer, jointBuffer)
    } finally {
      inputSource.close
    }
    val rockVolume = rockBuffer.toList
    val jointList = jointBuffer.toList

    // Create an initial block
    val blocks = List(Block((0.0, 0.0, 0.0), rockVolume))

    // Iterate through joints, cutting blocks where appropriate
    var cutBlocks = blocks
    for (joint <- jointList) {
      cutBlocks = cutBlocks.flatMap(_.cut(joint))
    }

    // Remove geometrically redundant joints
    val nonRedundantBlocks = cutBlocks.map { case block @ Block(center, _) =>
                                               Block(center, block.nonRedundantFaces) }
    // Calculate the centroid of each block
    /*
    val centroidBlocks = nonRedundantBlocks.map { case block @ Block(_, faces) =>
      val vertices = block.findVertices
      val mesh = block.meshFaces(vertices)
      val centroid = block.centroid(vertices, mesh)
      val updatedFaces = block.updateFaces(centroid)
      Block(centroid, faces)
    }
    */

    // Clean up double values arbitrarily close to 0.0
    val cleanedBlocks = nonRedundantBlocks.map { case Block(center, faces) =>
      Block(center, faces.map(_.applyTolerance))
    }

    val blockJson = json.blockSeqToReadableJson(cleanedBlocks)
    val expectedJsonSource = Source.fromURL(getClass.getResource(s"/${OUTPUT_FILE_NAME}"))
    try {
      val expectedJson = expectedJsonSource.mkString
      assert(blockJson.trim == expectedJson.trim)
    } finally {
      expectedJsonSource.close
    }
  }
}
