package qasrl.roles.modeling

import cats.Order
import cats.implicits._

import io.circe.{KeyEncoder, KeyDecoder}
import io.circe.generic.JsonCodec

import monocle.macros.Lenses

@Lenses @JsonCodec case class VerbId(
  sentenceId: String, verbIndex: Int
)
object VerbId {
  import io.circe._
  implicit val verbIdKeyEncoder = KeyEncoder.instance[VerbId](vid =>
    s"${vid.sentenceId}:${vid.verbIndex}"
  )
  implicit val verbIdKeyDecoder = KeyDecoder.instance[VerbId](s =>
    scala.util.Try(VerbId(s.reverse.dropWhile(_ != ':').tail.reverse, s.reverse.takeWhile(_ != ':').reverse.toInt)).toOption
  )
  implicit val verbIdOrder = Order.whenEqual(
    Order.by[VerbId, String](_.sentenceId),
    Order.by[VerbId, Int](_.verbIndex)
  )
}
