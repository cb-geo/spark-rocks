package edu.berkeley.ce.rockslicing.json

import edu.berkeley.ce.rockslicing.types.{Block, Face}
import play.api.libs.{json => playJson}
import playJson._

/** Defines functions to convert rock block data structures to JSON format
  */
object json {
  private implicit object FaceFormat extends Format[Face] {
    def reads(jsonVal: JsValue) = JsSuccess(Face(
      ((jsonVal \ "a").as[Double], (jsonVal \ "b").as[Double], (jsonVal \ "c").as[Double]),
      (jsonVal \ "d").as[Double],
      (jsonVal \ "phi").as[Double],
      (jsonVal \ "cohesion").as[Double]
    ))

    def writes(face: Face) = JsObject(Seq(
      "a" -> JsNumber(face.a),
      "b" -> JsNumber(face.b),
      "c" -> JsNumber(face.c),
      "d" -> JsNumber(face.d),
      "phi" -> JsNumber(face.phi)
    ))
  }

  private implicit object BlockFormat extends Format[Block] {
    def reads(jsonVal: JsValue) = JsSuccess(Block(
      ((jsonVal  \ "centerX").as[Double], (jsonVal \ "centerY").as[Double], (jsonVal \ "centerZ").as[Double]),
      (jsonVal \ "faces").as[List[Face]]
    ))

    def writes(block: Block) = JsObject(Seq(
      "centerX" -> JsNumber(block.centerX),
      "centerY" -> JsNumber(block.centerY),
      "centerZ" -> JsNumber(block.centerZ),
      "faces" -> Json.toJson(block.faces)
    ))
  }

  /** Converts a list a rock blocks to human-readable JSON.
    * @param blocks A list of Block objects
    * @return A human-readable JSON representation of the blocks, as a string
    */
  def rockBlocksToReadableJson(blocks: List[Block]): String =
    Json.prettyPrint(Json.toJson(blocks))

  /** Converts a list a rock blocks to human-readable JSON.
    * @param blocks A list of Block objects
    * @return A minimal JSON representation of the blocks, as a string
    */
  def rockBlocksToMinimalJson(blocks: List[Block]): String =
    Json.stringify(Json.toJson(blocks))
}
