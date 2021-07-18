package qasrl.roles.browse

import qasrl.roles.modeling.features.Features

import jjm.DotKleisli
import jjm.implicits._

import cats.effect.IO
import cats.implicits._

trait FeatureServiceCompanionPlatformExtensions {
  def baseService[VerbType, Arg](
    features: Features[VerbType, Arg]
  ) = new DotKleisli[IO, FeatureReq[VerbType, Arg]] {
    def apply(req: FeatureReq[VerbType, Arg]): IO[req.Out] = req match {
      case FeatureReq.Sentence(sid) => features.sentenceInfos.get
          .map(_.apply(sid))
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.AllInflectedForms(vt) => features.verbInflectedFormLists
          .map(_.apply(vt))
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.Sentences(vt) => features.sentencesByVerbType.get
          .map(_.apply(vt))
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.GoldLabels(vt) => features.getIfPropBank
          .traverse(feats =>
            for {
              senses <- feats.verbSenseLabels.get
              roles <- feats.argRoleLabels.get
            } yield GoldVerbInfo(
              senses(vt.asInstanceOf[String]).value,
              roles(vt.asInstanceOf[String]).value
            )
          ).asInstanceOf[IO[req.Out]]
      case FeatureReq.QuestionDists(vt) => features.argQuestionDists.get
          .map(_.apply(vt).value)
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.ArgSpans(vt) => features.argSpans.get
          .map(_.apply(vt).value)
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.ArgPrepositions(vt) => features.argPrepositions.get
          .map(_.apply(vt).value)
          .map(_.mapVals(prepOpt => Map(prepOpt.fold("<none>")(_.toString) -> 1.0)))
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.ArgConstituentTypes(vt, label) =>
        val res =
          if(label == "ptb") features.argConstituentTypeDists.get
          else if(label == "stripped") features.argConstituentTypeDistsConverted.get
          else IO.raiseError(new IllegalArgumentException("Constituent type type must be `ptb` or `stripped`"))
        res
          .map(_.apply(vt).value)
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.VerbMLMDist(vt, label) => features
          .postprocessedVerbMLMFeatures(label).get
          .flatMap(_.get)
          .map(_.apply(vt).value)
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.ArgPrepMLMDist(vt, label) => features
          .postprocessedArgPrepMLMFeatures(label).get
          .flatMap(_.get)
          .map(_.apply(vt).value)
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.ArgMLMDist(vt, label) => features
          .postprocessedArgSpanMLMFeatures(label).get
          .flatMap(_.get)
          .map(_.apply(vt).value)
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.ArgIndices(vt) => features.argSemanticHeadIndices.get
          .map(_.apply(vt).value)
          .asInstanceOf[IO[req.Out]]
      case FeatureReq.ArgSyntacticFunctions(vt) => features.argSyntacticFunctions.get
          .map(_.apply(vt).value)
          .asInstanceOf[IO[req.Out]]
    }
  }
}
