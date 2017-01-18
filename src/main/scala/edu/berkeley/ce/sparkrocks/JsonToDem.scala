package edu.berkeley.ce.sparkrocks

import play.api.libs.json.{Format, JsNumber, JsObject, JsSuccess, JsValue, Json => PlayJson}

/**
  * Defines functions to convert rock block data to JSON format that can be used as input into DEM code
  */
object JsonToDem {
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

  private implicit object FaceDemFormat extends Format[FaceDem] {
    def reads(jsonVal: JsValue) = JsSuccess(FaceDem(
      Face(Array((jsonVal \ "a").as[Double], (jsonVal \ "b").as[Double], (jsonVal \ "c").as[Double]),
        (jsonVal \ "d").as[Double],
        (jsonVal \ "phi").as[Double],
        (jsonVal \ "cohesion").as[Double]),
      (jsonVal \ "vertices").as[Seq[Array[Double]]]))

    def writes(face: FaceDem) = JsObject(Seq(
      "a" -> JsNumber(face.a),
      "b" -> JsNumber(face.b),
      "c" -> JsNumber(face.c),
      "d" -> JsNumber(face.d),
      "phi" -> JsNumber(face.phi),
      "cohesion" -> JsNumber(face.cohesion),
      "vertices" -> PlayJson.toJson(face.vertices)
    ))
  }

  private implicit object BlockDemFormat extends Format[BlockDem] {
    def reads(jsonVal: JsValue) = JsSuccess(BlockDem(Block(
      Array((jsonVal  \ "centerX").as[Double], (jsonVal \ "centerY").as[Double], (jsonVal \ "centerZ").as[Double]),
      (jsonVal \ "faces").as[List[Face]]
    )))

    def writes(block: BlockDem) = {
      JsObject(Seq(
        "centerX" -> JsNumber(block.centerX),
        "centerY" -> JsNumber(block.centerY),
        "centerZ" -> JsNumber(block.centerZ),
        "sphereCenterX" -> JsNumber(block.sphereCenterX),
        "sphereCenterY" -> JsNumber(block.sphereCenterY),
        "sphereCenterZ" -> JsNumber(block.sphereCenterZ),
        "sphereRadius" -> JsNumber(block.sphereRadius),
        "vertices" -> PlayJson.toJson(block.vertices),
        "connectivity" -> PlayJson.toJson(block.connectivity),
        "faces" -> PlayJson.toJson(block.faces)
      ))
    }
  }

  /**
    * Converts a rock block to human-readable JSON.
    * @param blockDem A rock block in format that can be used in a DEM analysis.
    * @return A human-readable JSON representation of the block as a string.
    */
  def blockDemToReadableJson(blockDem: BlockDem): String =
  PlayJson.prettyPrint(PlayJson.toJson(blockDem))

  /**
    * Converts a rock block to minimal JSON.
    * @param blockDem A rock block in format that can be used in a DEM analysis.
    * @return A minimal JSON representation of the block, as a string.
    */
  def blockDemToMinimalJson(blockDem: BlockDem): String =
  PlayJson.stringify(PlayJson.toJson(blockDem))

  /**
    * Converts a list of rock blocks to human-readable JSON.
    * @param blocksDem A list of BlockDem objects.
    * @return A human-readable JSON representation of the blocks, as a string.
    */
  def blockDemSeqToReadableJson(blocksDem: Seq[BlockDem]): String =
  PlayJson.prettyPrint(PlayJson.toJson(blocksDem))

  /**
    * Converts a list of rock blocks to human-readable JSON.
    * @param blocksDem A list of BlockDem objects.
    * @return A minimal JSON representation of the blocks, as a string.
    */
  def blockDemSeqToMinimalJson(blocksDem: Seq[BlockDem]): String =
  PlayJson.stringify(PlayJson.toJson(blocksDem))
}