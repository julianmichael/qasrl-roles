package qasrl.roles.modeling.features

import qasrl.labeling.QuestionTemplate

import io.circe.generic.JsonCodec

import jjm.ling.ESpan

object QGen {
  @JsonCodec case class SentencePrediction(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbs: List[VerbPrediction]
  )

  @JsonCodec case class VerbPrediction(
    verbIndex: Int,
    beam: List[SpanPrediction]
  )

  case class SpanPrediction(
    span: ESpan,
    spanProb: Double,
    questions: Map[QuestionTemplate, Double]
  )
  object SpanPrediction {
    import io.circe.{Json, Encoder, Decoder}
    import io.circe.syntax._

    implicit val spanPredictionEncoder: Encoder[SpanPrediction] =
      Encoder.instance[SpanPrediction](sp =>
        Json.obj(
          "span" := sp.span,
          "spanProb" := sp.spanProb,
          "questions" := sp.questions.toList.map { case (qt, prob) =>
            QuestionPrediction(qt, prob)
          }
        )
      )

    implicit val spanPredictionDecoder: Decoder[SpanPrediction] =
      Decoder.instance[SpanPrediction](c =>
        for {
          span <- c.downField("span").as[ESpan]
          spanProb <- c.downField("spanProb").as[Double]
          questions <- c.downField("questions").as[List[QuestionPrediction]].map(
            _.map(qp => qp.questionSlots -> qp.questionProb).toMap
          )
        } yield SpanPrediction(span, spanProb, questions)
      )
  }

  @JsonCodec case class QuestionPrediction(
    questionSlots: QuestionTemplate,
    questionProb: Double
  )

}
