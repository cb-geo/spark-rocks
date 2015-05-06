import scala.collection.mutable.ListBuffer
import scala.io.Source
import org.scalatest._
import edu.berkeley.ce.rockslicing._

class EndToEndSpec extends FunSuite {
  val INPUT_FILE_NAME = "endToEndData.txt"
  val OUTPUT_FILE_NAME = "endToEndBlocks.json"

  test("Simple end-to-end test using unit cube and simple planes") {
    // Read input file to generate list of joints and initial rock block
    val rockBuffer = new ListBuffer[Face]()
    val jointBuffer = new ListBuffer[Joint]()
    val inputSource = Source.fromURL(getClass.getResource(s"/${INPUT_FILE_NAME}"))
    inputProcessor.readInput(inputSource, rockBuffer, jointBuffer)
    inputSource.close
    val rockVolume = rockBuffer.toList
    val jointList = jointBuffer.toList

    // Create an initial block, we'll worry about centroids later
    val blocks = List(Block((0.0, 0.0, 0.0), rockVolume))

    // Iterate through joints, cutting blocks where appropriate
    var cutBlocks = blocks
    for (joint <- jointList) {
      cutBlocks = cutBlocks.flatMap(_.cut(joint))
    }

    // Remove geometrically redundant joints
    val nonRedundantBlocks = cutBlocks.map { case block @ Block(center, _) =>
                                               Block(center, block.nonRedundantFaces) }
    // TODO Calculate the centroid of each block

    val blockJson = json.rockBlocksToReadableJson(nonRedundantBlocks)
    val expectedJson = Source.fromURL(getClass.getResource(s"/${OUTPUT_FILE_NAME}"))
    try {
      val expectedJsonString = expectedJson.mkString
      assert(blockJson == expectedJsonString)
    } finally {
      expectedJson.close
    }
  }
}
