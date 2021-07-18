package qasrl.roles.modeling

import cats.Order

import io.circe.generic.JsonCodec

import monocle.macros.Lenses

@Lenses @JsonCodec case class ArgumentId[A](
  verbId: VerbId, argument: A
)
object ArgumentId {
  implicit def argumentIdOrder[A: Order] =
    Order.whenEqual(
      Order.by[ArgumentId[A], VerbId](_.verbId),
      Order.by[ArgumentId[A], A](_.argument)
    )
}
