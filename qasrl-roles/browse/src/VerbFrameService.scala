package qasrl.roles.browse
import qasrl.roles.modeling._

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

import qasrl.labeling.SlotBasedLabel
import qasrl.data.Dataset
import qasrl.data.VerbEntry

import jjm.DotKleisli
import jjm.DotFunctionK
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm

import io.circe.generic.JsonCodec
import io.circe.{Encoder, Decoder}

case class VerbFrameService[F[_], Model, VerbType, Arg](
  f: DotKleisli[F, VerbFrameService.Request[Model, VerbType, Arg]]) { self =>
  import VerbFrameService._
  def getVerbs: F[Map[VerbType, Int]] = f(GetVerbs())
  def getModel(model: Model, verb: VerbType): F[Option[VerbClusterModel[VerbType, Arg]]] = f(GetModel(model, verb))
}
object VerbFrameService {
  @JsonCodec sealed trait Request[Model, VerbType, Arg] { type Out }
  case class GetVerbs[Model, VerbType, Arg]() extends Request[Model, VerbType, Arg] { type Out = Map[VerbType, Int] }
  @JsonCodec case class GetModel[Model, VerbType, Arg](model: Model, verb: VerbType) extends Request[Model, VerbType, Arg] { type Out = Option[VerbClusterModel[VerbType, Arg]] }

  object Request {
    implicit def verbFrameServiceRequestDotEncoder[Model: Encoder, VerbType: Encoder, Arg: Encoder] = new DotKleisli[Encoder, Request[Model, VerbType, Arg]] {
      def apply(req: Request[Model, VerbType, Arg]): Encoder[req.Out] = req match {
        case GetVerbs() => implicitly[Encoder[List[(VerbType, Int)]]]
            .contramap[Map[VerbType, Int]](_.toList).asInstanceOf[Encoder[req.Out]]
        case GetModel(_, _) => implicitly[Encoder[Option[VerbClusterModel[VerbType, Arg]]]].asInstanceOf[Encoder[req.Out]]
      }
    }
    implicit def verbFrameServiceRequestDotDecoder[Model: Decoder, VerbType: Decoder, Arg: Decoder] = new DotKleisli[Decoder, Request[Model, VerbType, Arg]] {
      def apply(req: Request[Model, VerbType, Arg]): Decoder[req.Out] = req match {
        case GetVerbs() => implicitly[Decoder[List[(VerbType, Int)]]]
            .map(_.toMap).asInstanceOf[Decoder[req.Out]]
        case GetModel(_, _) => implicitly[Decoder[Option[VerbClusterModel[VerbType, Arg]]]].asInstanceOf[Decoder[req.Out]]
      }
    }
  }

  def basicIOService[Model, VerbType, Arg](
    verbCounts: Map[VerbType, Int],
    verbModels: Map[Model, Map[VerbType, VerbClusterModel[VerbType, Arg]]],
  ): DotKleisli[IO, Request[Model, VerbType, Arg]]  = DotKleisli.fromFunctionK(
    new DotFunctionK[IO, Request[Model, VerbType, Arg]] {
      def apply[A](req: Request[Model, VerbType, Arg] { type Out = A }): IO[A] = {
        @scala.annotation.nowarn
        val res = req match {
          case GetVerbs() => IO.pure(verbCounts): IO[Map[VerbType, Int]]
          case GetModel(model, verb) => IO(verbModels.get(model).map(_.apply(verb))): IO[Option[VerbClusterModel[VerbType, Arg]]]
        }
        res.asInstanceOf[IO[A]]
      }
    }
  )
}
