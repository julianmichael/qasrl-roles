package qasrl.roles.modeling

import java.nio.file.{Path => NIOPath}

import cats.Order
import cats.Show
import cats.implicits._

import cats.effect.IO

import io.circe.Decoder
import io.circe.Encoder

import freelog.EphemeralTreeLogger
import freelog.SequentialEphemeralTreeLogger
import freelog.implicits._

import jjm.LowerCaseString
import jjm.io.FileUtil
import jjm.metrics.WeightedNumbers
import jjm.implicits._

import qasrl.roles.modeling.features.PropBankFeatures
import jjm.Duad
import qasrl.roles.modeling.eval.EvalUtils
import qasrl.roles.modeling.eval.ClusterPRMetric
import qasrl.roles.modeling.eval.Csv

object Comparison {

  // def labelClusters[Arg](
  //   allClusters: Map[String, Vector[Set[ArgumentId[Arg]]]],
  //   getGoldLabel: String => ArgumentId[Arg] => String
  // ) = {
  //   allClusters.map { case (verbType, clusters) =>
  //     val goldLabelFn = getGoldLabel(verbType)
  //     verbType -> clusters.map(_.unorderedFoldMap(id => Map(goldLabelFn(id) -> 1)))
  //   }
  // }

  // def getB3StatsByVerbAndLabel(labeledClusters: Map[String, Vector[Map[String, Int]]]) = {
  //   labeledClusters.mapVals { clusters =>
  //     val localGoldLabelCounts = clusters.unorderedFold
  //     ClusterPRMetric.b3PerInstanceByLabel(localGoldLabelCounts, clusters)
  //   }
  // }

  def getB3StatsByVerbAndLabel[Arg](
    allClusters: Map[String, Vector[Set[ArgumentId[Arg]]]],
    getGoldLabel: String => ArgumentId[Arg] => String)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ) = {
    allClusters.toList.infoBarTraverse("Computing B-Cubed stats") { case (verbType, clusters) =>
      Log.trace(verbType) >> IO {
        val goldLabelFn = getGoldLabel(verbType)
        val labeledClusters = clusters.map(_.unorderedFoldMap(id => Map(goldLabelFn(id) -> 1)))
        val localGoldLabelCounts = labeledClusters.unorderedFold
        verbType -> ClusterPRMetric.b3PerInstanceByLabel(localGoldLabelCounts, labeledClusters)
      }
    }.map(_.toMap)
  }

  def run[Arg: Encoder : Decoder : Order](
    outDir: NIOPath,
    features: PropBankFeatures[Arg],
    model1Name: String,
    model1: Map[String, Vector[Set[ArgumentId[Arg]]]],
    model2Name: String,
    model2: Map[String, Vector[Set[ArgumentId[Arg]]]])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    goldLabels <- features.argRoleLabels.get
    getGoldLabel = (verb: String) => (x: ArgumentId[Arg]) => goldLabels(verb).value(x).role
    model1Stats <- Log.infoBranch(model1Name)(
      getB3StatsByVerbAndLabel(model1, getGoldLabel)
    )
    model2Stats <- Log.infoBranch(model2Name)(
      getB3StatsByVerbAndLabel(model2, getGoldLabel)
    )
    _ <- Csv.writePrfComparisonCsv(
      outDir.resolve("relative-b3-by-verb-and-label.tsv"),
      "Verb", "Role",
      model1Stats,
      model2Stats
    )
    // _ <- Csv.writePrfComparisonCsv(
    //   outDir.resolve("relative-b3-by-verb.tsv"),
    //   "Verb",
    //   model1Stats.mapVals(_.values.toList.combineAll),
    //   model2Stats.mapVals(_.values.toList.combineAll)
    // )
    // _ <- Csv.writePrfComparisonCsv(
    //   outDir.resolve("relative-b3-by-label.tsv"),
    //   "Role",
    //   model1Stats.values.toList.combineAll,
    //   model2Stats.values.toList.combineAll
    // )
  } yield ()

  // def analyzeFor

}
