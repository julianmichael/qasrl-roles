package qasrl.roles.modeling.eval

import cats.Order
import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._

import freelog.EphemeralTreeLogger
import freelog.implicits._

import jjm.metrics.Chosen
import jjm.metrics.Numbers

import qasrl.roles.clustering.MergeTree
import qasrl.roles.modeling.logLevel
import qasrl.roles.modeling.getMetricsString
import qasrl.roles.modeling.ClusterSplittingCriterion

case class SplitTuningSpec(
  criterion: SplitTuningCriterion,
  thresholdsOverride: Option[List[Double]] = None
) {
  def thresholds = thresholdsOverride.getOrElse(criterion.defaultThresholds)
  def run[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    criterion.runTuning(allStats, thresholds).flatTap { tuningResults =>
      val tunedBest = SplitTuningCriterion.chooseBest(tuningResults)
      Log.info(s"Tuned results (${criterion.name}): ${getMetricsString(tunedBest)}")
    }
  }

  override def toString = {
    if(thresholds == criterion.defaultThresholds) criterion.name
    else s"$criterion=" + thresholds.mkString(",")
  }
}
object SplitTuningSpec {

  def fromString(x: String): Option[SplitTuningSpec] = {
    SplitTuningCriterion.fromString(x).map(SplitTuningSpec(_)).orElse {
      val get = x.split("=").toList.lift
      for {
        criterionStr <- get(0)
        criterion <- SplitTuningCriterion.fromString(criterionStr)
        thresholdsStr <- get(1)
        thresholds <- thresholdsStr.split(",").toList.traverse(x =>
          scala.util.Try(x.toDouble).toOption
        )
      } yield SplitTuningSpec(criterion, Some(thresholds))
    }
  }
}

// runTuning and splitTree should match up.
trait SplitTuningCriterion {
  def name: String
  def defaultThresholds: List[Double]
  def selectPoint(threshold: Double)(choices: NonEmptyList[ConfStatsPoint]): ConfStatsPoint

  def splitTree[A](
    tree: MergeTree[A],
    count: A => Double,
    threshold: Double
  ): Vector[MergeTree[A]] = ???

  def runTuning[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    thresholds: List[Double])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[List[(Double, WeightedPR)]] = {
    SplitTuningCriterion.tuneWeightedStats(
      thresholds, allStats)(
      selectPoint)
  }

  override def toString = name
}
object SplitTuningCriterion {

  def chooseBest(tuningResults: List[(Double, WeightedPR)]): Chosen[Double, WeightedPR] = {
    val runThresholds = tuningResults.map(_._1)
    val maxima = Chosen(tuningResults.toMap).keepMaximaBy(_.f1).data
    if(maxima.contains(runThresholds.max)) Chosen(Map(maxima.minBy(_._1)))
    else if(maxima.contains(runThresholds.min)) Chosen(Map(maxima.maxBy(_._1)))
    else Chosen(maxima).keepMaxBy(_.f1)
  }

  def getTunedWeightedStats[VerbType](
    allResults: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    choose: NonEmptyList[ConfStatsPoint] => ConfStatsPoint
  ): WeightedPR = {
    allResults.toList.foldMap { case (verbType, results) =>
      choose(results).weightedPR
    }
  }

  def tuneWeightedStats[VerbType, A](
    thresholds: List[A],
    allResults: Map[VerbType, NonEmptyList[ConfStatsPoint]])(
    choose: A => NonEmptyList[ConfStatsPoint] => ConfStatsPoint)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[List[(A, WeightedPR)]] = {
    thresholds.infoBarTraverse("Tuning weighted stats") { threshold =>
      IO.pure(threshold -> getTunedWeightedStats(allResults, choose(threshold)))
    }
  }

  val all = List(
    OracleCriterion,
    NumClustersCriterion,
    TotalEntropyCriterion,
    WeightedEntropyCriterion,
    LinearPenaltyCriterion,
    LogPenaltyCriterion,
    SqNumClustersPenaltyCriterion,
    CuttingDeltaCriterion,
    LossPerItemCriterion,
    MaxLossPerItemCriterion,
    WorstCaseEntropyCriterion
  )

  def fromString(x: String): Option[SplitTuningCriterion] = {
    all.find(_.name == x)
  }
}

import SplitTuningCriterion._

object NumClustersCriterion extends SplitTuningCriterion {
  val name: String = "num-clusters"
  val defaultThresholds = (1 to 35).toList.map(_.toDouble)
  def selectPoint(threshold: Double)(choices: NonEmptyList[ConfStatsPoint]): ConfStatsPoint = {
    val choicesSorted = choices.sortBy(_.numClusters)
    val qualified = choicesSorted.toList.dropWhile(_.numClusters < threshold)
    qualified.headOption.getOrElse(choicesSorted.last)
  }

  override def splitTree[A](
    tree: MergeTree[A],
    count: A => Double,
    threshold: Double
  ): Vector[MergeTree[A]] = {
    ClusterSplittingCriterion.Number(threshold.toInt).splitTree(tree, count)
  }
}

object OracleCriterion extends SplitTuningCriterion {
  val name: String = "oracle"
  val defaultThresholds = {
    val logBound = 10

    (-logBound to logBound).toList.map(scala.math.pow(1.2, _))
  }
  def selectPoint(threshold: Double)(choices: NonEmptyList[ConfStatsPoint]): ConfStatsPoint = {
    choices.toList.maxBy(_.fMeasure(threshold))
  }
}

object WeightedEntropyCriterion extends SplitTuningCriterion {
  val name: String = "wentropy"
  val defaultThresholds = (-10 to 100).map(_.toDouble / 20).toList

  private val getTotalLoss = (t: Double) => (s: ConfStatsPoint) => {
    val totalWeight = s.clusterWeights.sum
    val mixingLoss = s.clusterWeights.foldMap(weight =>
      -weight * scala.math.log(weight / totalWeight)
    )
    s.loss + (t * mixingLoss)
  }

  def selectPoint(threshold: Double)(choices: NonEmptyList[ConfStatsPoint]): ConfStatsPoint = {
    val getLoss = getTotalLoss(threshold)
    choices.minimum(Order.by(getLoss))
  }

  override def splitTree[A](
    tree: MergeTree[A],
    count: A => Double,
    threshold: Double
  ): Vector[MergeTree[A]] = {
    ClusterSplittingCriterion.Entropy(threshold).splitTree(tree, count)
  }
}

object TotalEntropyCriterion extends SplitTuningCriterion {
  val name: String = "entropy"
  val defaultThresholds = (-10 to 100).map(_.toDouble / 20).toList

  private val getTotalLoss = (t: Double) => (s: ConfStatsPoint) => {
    val numItems = s.numItems
    val mixingLoss = s.clusterSizes.foldMap(size =>
      -size * scala.math.log(size.toDouble / numItems)
    )
    s.loss + (t * mixingLoss)
  }

  def selectPoint(threshold: Double)(choices: NonEmptyList[ConfStatsPoint]): ConfStatsPoint = {
    val getLoss = getTotalLoss(threshold)
    choices.minimum(Order.by(getLoss))
  }

  override def splitTree[A](
    tree: MergeTree[A],
    count: A => Double,
    threshold: Double
  ): Vector[MergeTree[A]] = {
    ClusterSplittingCriterion.Entropy(threshold).splitTree(tree, count)
  }
}

object LogPenaltyCriterion extends SplitTuningCriterion {
  val name: String = "log-penalty"
  val defaultThresholds = (1 to 100).map(_.toDouble / 100).toList

  private val getTotalLoss = (t: Double) => (s: ConfStatsPoint) => {
    s.loss + (t * scala.math.log(s.numClusters) * s.numItems)
  }

  def selectPoint(threshold: Double)(choices: NonEmptyList[ConfStatsPoint]): ConfStatsPoint = {
    val getLoss = getTotalLoss(threshold)
    choices.minimum(Order.by(getLoss))
  }
}

object LinearPenaltyCriterion extends SplitTuningCriterion {
  val name: String = "linear-penalty"
  val defaultThresholds = (0 to 100).map(_.toDouble / 1000).toList

  private val getTotalLoss = (t: Double) => (s: ConfStatsPoint) => {
    s.loss + (t * s.numClusters * s.numItems)
  }

  def selectPoint(threshold: Double)(choices: NonEmptyList[ConfStatsPoint]): ConfStatsPoint = {
    val getLoss = getTotalLoss(threshold)
    choices.minimum(Order.by(getLoss))
  }
}

object WorstCaseEntropyCriterion extends SplitTuningCriterion {
  val name: String = "max-entropy"
  val defaultThresholds = (-10 to 100).map(_.toDouble / 20).toList

  private val getTotalLoss = (t: Double) => (s: ConfStatsPoint) => {
    val numItems = s.numItems
    val mixingEntropy = s.clusterSizes.foldMap(size =>
      -(size.toDouble / numItems) * scala.math.log(size.toDouble / numItems)
    )
    s.lossesPerItem.max + (t * mixingEntropy)
  }

  def selectPoint(threshold: Double)(choices: NonEmptyList[ConfStatsPoint]): ConfStatsPoint = {
    val getLoss = getTotalLoss(threshold)
    choices.minimum(Order.by(getLoss))
  }
}

object SqNumClustersPenaltyCriterion extends SplitTuningCriterion {
  val name: String = "sq-nclusters"
  val defaultThresholds = (0 to 50).map(_.toDouble / 2).toList

  private val getTotalLoss = (t: Double) => (s: ConfStatsPoint) => {
    s.loss + (t * scala.math.pow(s.numClusters, 2.0))
  }

  def selectPoint(threshold: Double)(choices: NonEmptyList[ConfStatsPoint]): ConfStatsPoint = {
    val getLoss = getTotalLoss(threshold)
    choices.minimum(Order.by(getLoss))
  }
}

object CuttingDeltaCriterion extends SplitTuningCriterion {
  val name: String = "cut-delta"
  val defaultThresholds = (0 to 100).map(_.toDouble / 10).toList

  def selectPoint(threshold: Double)(choices: NonEmptyList[ConfStatsPoint]): ConfStatsPoint = {
    val choicesSorted = choices.sortBy(_.loss)
    val deltaThreshold = threshold * choicesSorted.head.numItems
    choicesSorted.toList.sliding(2)
      .filter(_.size == 2)
      .takeWhile(w => w(1).loss - w(0).loss < deltaThreshold)
      .toList.lastOption
      .fold(choicesSorted.head)(_(0))
  }
}

object LossPerItemCriterion extends SplitTuningCriterion {
  val name: String = "loss"
  val defaultThresholds = (0 to 40).toList.map(_.toDouble / 10.0)

  def selectPoint(threshold: Double)(choices: NonEmptyList[ConfStatsPoint]): ConfStatsPoint = {
    val choicesSorted = choices.sortBy(-_.lossPerItem)
    val qualified = choicesSorted.toList.takeWhile(_.lossPerItem > threshold)
    qualified.lastOption.getOrElse(choicesSorted.head)
  }
}

object MaxLossPerItemCriterion extends SplitTuningCriterion {
  val name: String = "max-loss"
  val defaultThresholds = (0 to 100).toList.map(_.toDouble / 100.0)

  def selectPoint(threshold: Double)(choices: NonEmptyList[ConfStatsPoint]): ConfStatsPoint = {
    val choicesWithLPIs = choices.map(p => p -> p.lossesPerItem.max)
    val choicesSorted = choicesWithLPIs.sortBy(-_._2)
    val qualified = choicesSorted.toList.takeWhile(_._2 > threshold)
    qualified.lastOption.getOrElse(choicesSorted.head)._1
  }
}
