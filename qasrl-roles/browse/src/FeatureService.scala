package qasrl.roles.browse

import qasrl.roles.modeling.ArgumentId
import qasrl.roles.modeling.PropBankRoleLabel
import qasrl.roles.modeling.SentenceInfo
import qasrl.roles.modeling.VerbId

import qasrl.labeling.QuestionTemplate

import jjm.DotKleisli
import jjm.ling.ESpan
import jjm.ling.en.InflectedForms

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

object FeatureService extends FeatureServiceCompanionPlatformExtensions {
  // implicit class RichFeatureService[F[_], VerbType, Arg](fs: FeatureService[F, VerbType, Arg]) extends AnyVal {
  //   def questionDists(verbType: VerbType): F[Map[ArgumentId[Arg], Map[QuestionTemplate, Double]]] = {
  //     val req: FeatureReq[VerbType, Arg] { type Out = Map[ArgumentId[Arg], Map[QuestionTemplate, Double]] } =
  //       FeatureReq(FeatureKey.QuestionDists[Arg](), verbType)
  //     fs.apply(req)
  //   }
  // }
}

case class GoldVerbInfo[Arg](
  verbSenses: Map[VerbId, String],
  argRoles: Map[ArgumentId[Arg], PropBankRoleLabel]
)
object GoldVerbInfo {
  implicit def goldVerbInfoEncoder[Arg: Encoder] = implicitly[
    Encoder[(List[(VerbId, String)], List[(ArgumentId[Arg], PropBankRoleLabel)])]
  ].contramap[GoldVerbInfo[Arg]](i => i.verbSenses.toList -> i.argRoles.toList)
  implicit def goldVerbInfoDecoder[Arg: Decoder] = implicitly[
    Decoder[(List[(VerbId, String)], List[(ArgumentId[Arg], PropBankRoleLabel)])]
  ].map { case (v, a) => GoldVerbInfo[Arg](v.toMap, a.toMap) }
}

@JsonCodec sealed trait FeatureReq[VerbType, Arg] { type Out }
object FeatureReq {
  case class AllInflectedForms[VerbType, Arg](verbType: VerbType) extends FeatureReq[VerbType, Arg] {
    type Out = List[InflectedForms]
  }
  case class Sentences[VerbType, Arg](verbType: VerbType) extends FeatureReq[VerbType, Arg] {
    type Out = Set[String]
  }
  case class Sentence[VerbType, Arg](sentenceId: String) extends FeatureReq[VerbType, Arg] {
    type Out = SentenceInfo[VerbType, Arg]
  }
  case class GoldLabels[VerbType, Arg](verbType: VerbType) extends FeatureReq[VerbType, Arg] {
    type Out = Option[GoldVerbInfo[Arg]]
  }
  case class QuestionDists[VerbType, Arg](verbType: VerbType) extends FeatureReq[VerbType, Arg] {
    type Out = Map[ArgumentId[Arg], Map[QuestionTemplate, Double]]
  }
  case class ArgSpans[VerbType, Arg](verbType: VerbType) extends FeatureReq[VerbType, Arg] {
    type Out = Map[ArgumentId[Arg], Map[ESpan, Double]]
  }
  case class ArgPrepositions[VerbType, Arg](verbType: VerbType) extends FeatureReq[VerbType, Arg] {
    type Out = Map[ArgumentId[Arg], Map[String, Double]]
  }
  case class ArgConstituentTypes[VerbType, Arg](verbType: VerbType, label: String) extends FeatureReq[VerbType, Arg] {
    type Out = Map[ArgumentId[Arg], Map[String, Double]]
  }
  case class ArgIndices[VerbType, Arg](verbType: VerbType) extends FeatureReq[VerbType, Arg] {
    type Out = Map[ArgumentId[Arg], Int]
  }
  case class ArgSyntacticFunctions[VerbType, Arg](verbType: VerbType) extends FeatureReq[VerbType, Arg] {
    type Out = Map[ArgumentId[Arg], String]
  }
  case class ArgMLMDist[VerbType, Arg](verbType: VerbType, label: String) extends FeatureReq[VerbType, Arg] {
    type Out = Map[ArgumentId[Arg], Map[String, Float]]
  }
  case class ArgPrepMLMDist[VerbType, Arg](verbType: VerbType, label: String) extends FeatureReq[VerbType, Arg] {
    type Out = Map[ArgumentId[Arg], Map[String, Float]]
  }
  case class VerbMLMDist[VerbType, Arg](verbType: VerbType, label: String) extends FeatureReq[VerbType, Arg] {
    type Out = Map[VerbId, Map[String, Float]]
  }

  // case class ArgMLMDist[Arg](pattern: String) extends ArgFeatureKey[Arg, Map[QuestionTemplate, Double]]
  // case class VerbMLMDist(pattern: String) extends VerbFeatureKey[Map[QuestionTemplate, Double]]

  implicit def featureReqDotEncoder[VerbType: Encoder, Arg: Encoder] = new DotKleisli[Encoder, FeatureReq[VerbType, Arg]] {
    def apply(req: FeatureReq[VerbType, Arg]): Encoder[req.Out] = req match {
      case AllInflectedForms(_) => implicitly[Encoder[List[InflectedForms]]]
          .asInstanceOf[Encoder[req.Out]]
      case Sentence(_) => implicitly[Encoder[SentenceInfo[VerbType, Arg]]]
          .asInstanceOf[Encoder[req.Out]]
      case Sentences(_) => implicitly[Encoder[Set[String]]]
          .asInstanceOf[Encoder[req.Out]]
      case GoldLabels(_) => implicitly[Encoder[Option[GoldVerbInfo[Arg]]]]
          .asInstanceOf[Encoder[req.Out]]
      case QuestionDists(_) => implicitly[Encoder[List[(ArgumentId[Arg], List[(QuestionTemplate, Double)])]]]
          .contramap[Map[ArgumentId[Arg], Map[QuestionTemplate, Double]]](_.iterator.map(p => p._1 -> p._2.toList).toList)
          .asInstanceOf[Encoder[req.Out]]
      case ArgSpans(_) => implicitly[Encoder[List[(ArgumentId[Arg], List[(ESpan, Double)])]]]
          .contramap[Map[ArgumentId[Arg], Map[ESpan, Double]]](_.iterator.map(p => p._1 -> p._2.toList).toList)
          .asInstanceOf[Encoder[req.Out]]
      case ArgPrepositions(_) => implicitly[Encoder[List[(ArgumentId[Arg], List[(String, Double)])]]]
          .contramap[Map[ArgumentId[Arg], Map[String, Double]]](_.iterator.map(p => p._1 -> p._2.toList).toList)
          .asInstanceOf[Encoder[req.Out]]
      case ArgConstituentTypes(_, _) => implicitly[Encoder[List[(ArgumentId[Arg], List[(String, Double)])]]]
          .contramap[Map[ArgumentId[Arg], Map[String, Double]]](_.iterator.map(p => p._1 -> p._2.toList).toList)
          .asInstanceOf[Encoder[req.Out]]
      case ArgIndices(_) => implicitly[Encoder[List[(ArgumentId[Arg], Int)]]]
          .contramap[Map[ArgumentId[Arg], Int]](_.toList)
          .asInstanceOf[Encoder[req.Out]]
      case ArgSyntacticFunctions(_) => implicitly[Encoder[List[(ArgumentId[Arg], String)]]]
          .contramap[Map[ArgumentId[Arg], String]](_.toList)
          .asInstanceOf[Encoder[req.Out]]
      case ArgMLMDist(_, _) => implicitly[Encoder[List[(ArgumentId[Arg], List[(String, Float)])]]]
          .contramap[Map[ArgumentId[Arg], Map[String, Float]]](_.iterator.map(p => p._1 -> p._2.toList).toList)
          .asInstanceOf[Encoder[req.Out]]
      case ArgPrepMLMDist(_, _) => implicitly[Encoder[List[(ArgumentId[Arg], List[(String, Float)])]]]
          .contramap[Map[ArgumentId[Arg], Map[String, Float]]](_.iterator.map(p => p._1 -> p._2.toList).toList)
          .asInstanceOf[Encoder[req.Out]]
      case VerbMLMDist(_, _) => implicitly[Encoder[List[(VerbId, List[(String, Float)])]]]
          .contramap[Map[VerbId, Map[String, Float]]](_.iterator.map(p => p._1 -> p._2.toList).toList)
          .asInstanceOf[Encoder[req.Out]]
    }
  }

  implicit def featureReqDotDecoder[VerbType: Decoder, Arg: Decoder] = new DotKleisli[Decoder, FeatureReq[VerbType, Arg]] {
    def apply(req: FeatureReq[VerbType, Arg]): Decoder[req.Out] = req match {
      case AllInflectedForms(_) => implicitly[Decoder[List[InflectedForms]]]
          .asInstanceOf[Decoder[req.Out]]
      case Sentence(_) => implicitly[Decoder[SentenceInfo[VerbType, Arg]]]
          .asInstanceOf[Decoder[req.Out]]
      case Sentences(_) => implicitly[Decoder[Set[String]]]
          .asInstanceOf[Decoder[req.Out]]
      case GoldLabels(_) => implicitly[Decoder[Option[GoldVerbInfo[Arg]]]]
          .asInstanceOf[Decoder[req.Out]]
      case QuestionDists(_) => implicitly[Decoder[List[(ArgumentId[Arg], List[(QuestionTemplate, Double)])]]]
          .map(_.iterator.map(p => p._1 -> p._2.toMap).toMap)
          .asInstanceOf[Decoder[req.Out]]
      case ArgSpans(_) => implicitly[Decoder[List[(ArgumentId[Arg], List[(ESpan, Double)])]]]
          .map(_.iterator.map(p => p._1 -> p._2.toMap).toMap)
          .asInstanceOf[Decoder[req.Out]]
      case ArgPrepositions(_) => implicitly[Decoder[List[(ArgumentId[Arg], List[(String, Double)])]]]
          .map(_.iterator.map(p => p._1 -> p._2.toMap).toMap)
          .asInstanceOf[Decoder[req.Out]]
      case ArgConstituentTypes(_, _) => implicitly[Decoder[List[(ArgumentId[Arg], List[(String, Double)])]]]
          .map(_.iterator.map(p => p._1 -> p._2.toMap).toMap)
          .asInstanceOf[Decoder[req.Out]]
      case ArgIndices(_) => implicitly[Decoder[List[(ArgumentId[Arg], Int)]]]
          .map(_.toMap)
          .asInstanceOf[Decoder[req.Out]]
      case ArgSyntacticFunctions(_) => implicitly[Decoder[List[(ArgumentId[Arg], String)]]]
          .map(_.toMap)
          .asInstanceOf[Decoder[req.Out]]
      case ArgMLMDist(_, _) => implicitly[Decoder[List[(ArgumentId[Arg], List[(String, Float)])]]]
          .map(_.iterator.map(p => p._1 -> p._2.toMap).toMap)
          .asInstanceOf[Decoder[req.Out]]
      case ArgPrepMLMDist(_, _) => implicitly[Decoder[List[(ArgumentId[Arg], List[(String, Float)])]]]
          .map(_.iterator.map(p => p._1 -> p._2.toMap).toMap)
          .asInstanceOf[Decoder[req.Out]]
      case VerbMLMDist(_, _) => implicitly[Decoder[List[(VerbId, List[(String, Float)])]]]
          .map(_.iterator.map(p => p._1 -> p._2.toMap).toMap)
          .asInstanceOf[Decoder[req.Out]]
    }
  }
}
