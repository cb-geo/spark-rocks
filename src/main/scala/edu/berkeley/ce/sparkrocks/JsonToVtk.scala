package edu.berkeley.ce.sparkrocks

import play.api.libs.json.{Format, JsNumber, JsObject, JsSuccess, JsValue, Json => PlayJson}

/**
  * Defines functions to convert rock block data to JSON format that can easily be converted to VTK format
  */
object JsonToVtk {
  private implicit object FaceFormat extends Format[Face] {
    def reads(jsonVal: JsValue) = JsSuccess(Face(
      Array((jsonVal \ "a").as[Double], (jsonVal \ "b").as[Double], (jsonVal \ "c").as[Double]),
      (jsonVal \ "d").as[Double],
      (jsonVal \ "phi").as[Double],
      (jsonVal \ "cohesion").as[Double]
    ))

    def writes(face: Face) = JsObject(Seq(
      "a" -> JsNumber(face.a),
      "b" -> JsNumber(face.b),
      "c" -> JsNumber(face.c),
      "d" -> JsNumber(face.d),
      "phi" -> JsNumber(face.phi),
      "cohesion" -> JsNumber(face.cohesion)
    ))
  }

  private implicit object BlockVtkFormat extends Format[BlockVTK] {
    def reads(jsonVal: JsValue) = JsSuccess(BlockVTK(Block(
      Array((jsonVal  \ "centerX").as[Double], (jsonVal \ "centerY").as[Double], (jsonVal \ "centerZ").as[Double]),
      (jsonVal \ "faces").as[List[Face]]
    )))

    def writes(blockVTK: BlockVTK) = {
      JsObject(Seq(
        "vertices" -> PlayJson.toJson(blockVTK.vertices),
        "vertexIDs" -> PlayJson.toJson(blockVTK.vertexIDs),
        "connectivity" -> PlayJson.toJson(blockVTK.connectivity),
        "faceCount" -> JsNumber(blockVTK.faceCount),
        "offsets" -> PlayJson.toJson(blockVTK.offsets),
        "normals" -> PlayJson.toJson(blockVTK.normals)
      ))
    }
  }

  /**
    * Converts a rock block to human-readable JSON.
    * @param blockVTK A rock block in format that is easily converted to VTK.
    * @return A human-readable JSON representation of the block as a string.
    */
  def blockVtkToReadableJson(blockVTK: BlockVTK): String =
    PlayJson.prettyPrint(PlayJson.toJson(blockVTK))

  /**
    * Converts a rock block to minimal JSON.
    * @param blockVTK A rock block in format that is easily converted to VTK.A rock block.
    * @return A minimal JSON representation of the block, as a string.
    */
  def blockVtkToMinimalJson(blockVTK: BlockVTK): String =
    PlayJson.stringify(PlayJson.toJson(blockVTK))

  /**
    * Converts a list of rock blocks to human-readable JSON.
    * @param blocksVTK A list of BlockVTK objects.
    * @return A human-readable JSON representation of the blocks, as a string.
    */
  def blockVtkSeqToReadableJson(blocksVTK: Seq[BlockVTK]): String =
    PlayJson.prettyPrint(PlayJson.toJson(blocksVTK))

  /**
    * Converts a list of rock blocks to human-readable JSON.
    * @param blocksVTK A list of BlockVTK objects.
    * @return A minimal JSON representation of the blocks, as a string.
    */
  def blockVTKSeqToMinimalJson(blocksVTK: Seq[BlockVTK]): String =
    PlayJson.stringify(PlayJson.toJson(blocksVTK))
}
