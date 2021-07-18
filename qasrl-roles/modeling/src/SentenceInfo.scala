package qasrl.roles.modeling

import io.circe.generic.JsonCodec

@JsonCodec case class VerbInfo[VerbType, Arg](
  index: Int,
  verbType: VerbType,
  args: Set[Arg])

@JsonCodec case class SentenceInfo[VerbType, Arg](
  sentenceId: String,
  tokens: Vector[String],
  verbs: Map[Int, VerbInfo[VerbType, Arg]])
