package qasrl.roles.modeling.eval

import qasrl.roles.clustering.MergeTree
import qasrl.roles.modeling._
import qasrl.roles.modeling.features.Features

import qasrl.labeling.QuestionTemplate

import jjm.Duad
import jjm.NonMergingMap
import jjm.io.FileUtil
import jjm.metrics._
import jjm.implicits._

import freelog.EphemeralTreeLogger
import freelog.SequentialEphemeralTreeLogger
import freelog.implicits._

import cats.Monoid
import cats.Order
import cats.Show
import cats.data.NonEmptyList
import cats.implicits._

import cats.effect.IO
import cats.effect.Timer
import cats.effect.concurrent.Ref

import java.nio.file.{Path => NIOPath}

import scala.annotation.tailrec

object Evaluation {

    def calculateSmoothedF1(target: WeightedPR, mean: WeightedPR, smoothing: Double): Double = {
      val smoothedP = (
        WeightedNumbers(target.precision, target.pseudocount) |+|
          WeightedNumbers(mean.precision, smoothing)
      ).stats.weightedMean
      val smoothedR = (
        WeightedNumbers(target.recall, target.pseudocount) |+|
          WeightedNumbers(mean.recall, smoothing)
      ).stats.weightedMean
      Functions.harmonicMean(smoothedP, smoothedR)
    }


  import com.cibo.evilplot.plot.aesthetics.DefaultTheme._

  def getAllClusteringConfs[A](
    goldLabelTree: MergeTree[(Map[A, Int], Double)],
    extraClusters: Map[String, (Map[A, Int], Double)],
    metric: ClusterPRMetric,
  ): NonEmptyList[ConfStatsPoint] = {
    val goldClusterSizes = goldLabelTree.unorderedFoldMap(_._1) |+| extraClusters.unorderedFoldMap(_._1)
    NonEmptyList.fromList(
      goldLabelTree.clusterSplittingsStreamByMaxLossPerCount(_.unorderedFold).take(1000).map { clusters =>
        val losses = clusters.map(_.loss)
        val clusterSizes = clusters.map(_.unorderedFoldMap(_._1.unorderedFold))
        val clusterWeights = clusters.map(_.unorderedFoldMap(_._2))
        val weightedPR = metric(
          goldClusterSizes, clusters.map(_.unorderedFoldMap(_._1)) ++
            extraClusters.values.map(_._1)
        )
        ConfStatsPoint(losses, clusterSizes, clusterWeights, weightedPR)
      }.compile.toList
    ).get
  }

  def getLeafClusteringConf[A](
    goldLabelTree: MergeTree[(Map[A, Int], Double)],
    extraClusters: Map[String, (Map[A, Int], Double)],
    metric: ClusterPRMetric,
  ): ConfStatsPoint = {
    val goldClusterSizes = goldLabelTree.unorderedFoldMap(_._1) |+| extraClusters.unorderedFoldMap(_._1)
    val clusterWeights = goldLabelTree.map(_._2).values
    val clusters = goldLabelTree.map(_._1).valuesWithLosses
    val losses = clusters.map(_._1)
    val clusterSizes = clusters.map(_._2.unorderedFold)
    val weightedPR = metric(goldClusterSizes, clusters.map(_._2) ++ extraClusters.values.map(_._1))
    ConfStatsPoint(losses, clusterSizes, clusterWeights, weightedPR)
  }

  def getAllPRStats[VerbType, InstanceId, GoldLabel](
    itemWeightsOpt: Option[Map[VerbType, Map[InstanceId, Double]]],
    clusterings: Map[VerbType, Clustering[InstanceId]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    metric: ClusterPRMetric,
    maxClustersOnly: Boolean)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, NonEmptyList[ConfStatsPoint]]] = clusterings.toList
    .filter(_._2.nonEmpty)
    .infoBarTraverse(s"Calculating ${metric.name} for all clusterings") { case (verbType, clustering) =>
      val getGoldLabelForVerb = getGoldLabel(verbType)
      val getItemWeights = itemWeightsOpt.map(_.apply(verbType)).getOrElse((id: InstanceId) => 1.0)
      Log.trace(s"$verbType (${clustering.size} leaves + extras)") >> IO {
        val extraClusterGoldCounts = clustering.extraClusters.mapVals(cluster =>
          cluster.unorderedFoldMap(id => Map(getGoldLabelForVerb(id) -> 1)) ->
            cluster.unorderedFoldMap(getItemWeights)
        )
        val confs = clustering.clusterTreeOpt match {
          case None =>
            val goldClusterSizes = extraClusterGoldCounts.unorderedFoldMap(_._1)
            NonEmptyList.of(
              ConfStatsPoint(
                Vector(), Vector(), Vector(), metric(
                  goldClusterSizes, extraClusterGoldCounts.values.toVector.map(_._1)
                )
              )
            )
          case Some(tree) =>
            val goldCountTree = tree.map(cluster =>
              cluster.unorderedFoldMap(id => Map(getGoldLabelForVerb(id) -> 1)) ->
                cluster.unorderedFoldMap(getItemWeights)
            )
            // we only consider default clustering of gold-derived models
            if(maxClustersOnly) NonEmptyList.of(
              getLeafClusteringConf(goldCountTree, extraClusterGoldCounts, metric)
            )
            else getAllClusteringConfs(goldCountTree, extraClusterGoldCounts, metric)
        }
        verbType -> confs
      }
    }.map(_.toMap)

  def runTuningCalibration[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    tuningSpec: SplitTuningSpec,
    resultsDir: NIOPath)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    val name = tuningSpec.criterion.name
    for {
      dataPoints <- allStats.toList
        .groupBy(_._2.head.numItems).toList
        .traverse { case (numItems, statSets) =>
          tuningSpec.run(statSets.toMap)(EphemeralTreeLogger.noop).map(points =>
            points.map { case (threshold, pr) =>
              PlottingJVM.WeirdLinePoint(scala.math.log(numItems.toDouble), threshold, pr.f1, (numItems * statSets.size) / 25.0)
            }
          )
        }
      _ <- PlottingJVM.plotWeirdLines(
        dataPoints,
        title = s"Scores ($name) by number of instances and thresholds",
        xAxisLabel = "Log num instances",
        yAxisLabel = "Threshold",
        path = resultsDir.resolve(s"tuning-calibration-$name.png")
      )
    } yield ()
  }

  def runTuningCalibration2[VerbType](
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    tuningSpec: SplitTuningSpec,
    resultsDir: NIOPath
  ): IO[Unit] = {
    val name = tuningSpec.criterion.name
    case class CalibrationPoint(
      threshold: Double,
      stats: ConfStatsPoint
    )
    val dataPoints: List[CalibrationPoint] = allStats.values.toList.map { choices =>
      val threshold = SplitTuningCriterion.chooseBest(
        tuningSpec.thresholds.map(t =>
          t -> tuningSpec.criterion.selectPoint(t)(choices).weightedPR
        )
      ).data.head._1
      CalibrationPoint(threshold, tuningSpec.criterion.selectPoint(threshold)(choices))
    }
    PlottingJVM.plotWeightedScatter(
      dataPoints.foldMap(p =>
        Map(
          "max F1" -> List(PlottingJVM.WeightedScatterPoint(p.stats.numItems, p.threshold, p.stats.numItems)),
        )
      ),
      title = s"Best threshold ($name) by number of instances",
      xAxisLabel = "Num instances",
      yAxisLabel = "Best threshold",
      path = resultsDir.resolve(s"tuning-calibration-$name.png")
    )
  }

  def runClusterTuning[VerbType](
    modelName: String,
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    tuningSpecs: NonEmptyList[SplitTuningSpec],
    precisionAxisLabel: String,
    recallAxisLabel: String,
    resultsDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[(SplitTuningCriterion, Double)] = {
    def outerLogger = Log
    freelog.loggers.TimingDelayedLogger.file(resultsDir.resolve("results.txt")) { fileLogger =>
      implicit val Log = outerLogger |+| fileLogger // shadow the outer one, removing implicit ambiguity
      for {
        _ <- Log.info {
          // max precision FYI for e.g. knowing how well flat clustering worked
          val maxPrecision = allStats.toList.foldMap { case (verbType, results) =>
            results.toList.maxBy(_.precision).weightedPR
          }.precision
          f"Max precision: $maxPrecision%.3f"
        }
        tuningResults <- tuningSpecs.traverse(
          tuningSpec => Log.infoBranch(s"Tuning (${tuningSpec.criterion.name})") {
            runTuningCalibration(allStats, tuningSpec, resultsDir) >>
              tuningSpec.run(allStats).map(tuningSpec.criterion.name -> _)
          }
        ).map(_.toList.toMap)
        _ <- {
          IO.unit
        }
        _ <- PlottingJVM.plotPrecisionRecallCurves(
          tuningResults,
          modelName, precisionAxisLabel, recallAxisLabel,
          resultsDir.resolve("tuning-strategies.png")
        )
        (bestCriterion, bestThreshold, bestStats) = tuningResults.toList.filter(_._1 != "oracle").map { case (name, stats) =>
          val criterion = SplitTuningCriterion.fromString(name).get
          val (threshold, result) = SplitTuningCriterion.chooseBest(stats).data.head
          (criterion, threshold, result)
        }.maxBy(_._3.f1)
        bestSettingString = bestCriterion.name + "=" + bestThreshold
        _ <- FileUtil.writeString(resultsDir.resolve("best-setting.txt"))(bestSettingString)
        _ <- FileUtil.writeString(resultsDir.resolve("best-result.txt"))(getMetricsString(bestStats))
        _ <- PlottingJVM.plotWeightedScatter(
          allStats.values.toList.foldMap { stats =>
            val res = bestCriterion.selectPoint(bestThreshold)(stats)
            Map(
              "precision" -> List(PlottingJVM.WeightedScatterPoint(res.numItems, res.precision, res.numItems)),
              "recall" -> List(PlottingJVM.WeightedScatterPoint(res.numItems, res.recall, res.numItems)),
              "f1" -> List(PlottingJVM.WeightedScatterPoint(res.numItems, res.f1, res.numItems))
            )
          },
          title = s"Tuned stats ($bestSettingString) by verb frequency",
          xAxisLabel = "Num instances",
          yAxisLabel = "Score",
          path = resultsDir.resolve("by-freq-stats.png")
        )
      } yield bestCriterion -> bestThreshold
    }
  }

  val argGoldLabels = (0 to 5).map(i => s"A$i").toSet
  val modGoldLabels = Set("TMP", "ADV", "MNR", "LOC", "PNC", "CAU", "DIR").map(x => s"AM-$x")
  val lexGoldLabels = Set("NEG", "MOD", "DIS").map(x => s"AM-$x")
  def getGoldLabelGroup[GoldLabel: Show](label: GoldLabel) = {
    val x = label.show
    if(argGoldLabels.contains(x)) "Arg"
    else if(modGoldLabels.contains(x)) "Mod"
    else if(lexGoldLabels.contains(x)) "Lex"
    else "Other"
  }

  def runClusteringEvalWithMetric[VerbType : Show, InstanceId, GoldLabel : Order : Show](
    itemDistsOpt: Option[Map[VerbType, NonMergingMap[InstanceId, Map[QuestionTemplate, Double]]]],
    parentDir: NIOPath,
    modelName: String,
    argClusterings: Map[VerbType, Clustering[InstanceId]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    tuningSpecs: NonEmptyList[SplitTuningSpec],
    metric: ClusterPRMetric)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ) = {
    Log.infoBranch(s"Calculating metrics (${metric.name})") {
      getAllPRStats(
        itemDistsOpt.map(_.mapVals(_.value.mapVals(_.unorderedFold))),
        argClusterings, getGoldLabel, metric, modelName.startsWith("gold")
      ) >>= (allStats =>
        for {
          resultsDir <- IO.pure(parentDir.resolve(metric.name)).flatTap(createDir)
          _ <- Log.info(
            "Max precision info: " + getMetricsString(
              allStats.values.toList.foldMap(x => Numbers(x.map(_.precision).maximum))
            )
              // + "\n" + allStats
              // .mapVals(_.map(_.precision).maximum).toList
              // .filter(_._2 < 0.90).sortBy(_._2).map(_._1).mkString(", ")
          )
          _ <- PlottingJVM.plotAllStatsVerbwise(
            modelName, allStats,
            metric.precisionName, metric.recallName,
            name => resultsDir.resolve(s"by-verb-$name.png")
          )
          (bestCriterion, bestThreshold) <- runClusterTuning(
            modelName, allStats,
            tuningSpecs,
            metric.precisionName, metric.recallName,
            resultsDir
          )
          allClusterings = argClusterings.mapVals { clustering =>
            clustering.clusterTreeOpt.foldMap(tree =>
              bestCriterion
                .splitTree(tree, (x: Set[InstanceId]) => x.size, bestThreshold)
                .map(_.unorderedFold)
            ) -> clustering.extraClusters.values.toVector
          }
          learnedClusterings = allClusterings.mapVals(_._1)
          // _ <- itemDistsOpt.traverse { itemDists =>
          //   IO {
          //     val allClusters = learnedClusterings.toVector.flatMap { case (verb, clusters) =>
          //       clusters.map(verb -> _)
          //     }
          //     val allDists = itemDists.values.toList.combineAll
          //     val allClusterDists = allClusters.mapSecond(_.unorderedFoldMap(allDists))
          //     Analysis.reportQuestionPairRelatedness(allClusterDists, resultsDir)
          //   }.flatten
          // }
          allClusters = allClusterings.mapVals { case (c, extra) => c ++ extra }
          allLabeledClusters = allClusters.map { case (verbType, clusters) =>
            val goldLabelFn = getGoldLabel(verbType)
            verbType -> clusters.map(_.unorderedFoldMap(id => Map(goldLabelFn(id) -> 1)))
          }
          goldLabelCounts = allLabeledClusters.unorderedFoldMap(_.unorderedFold)
          minGoldLabelCount = 5
          goldNpmis = EvalUtils.calculateAggregateNPMIs(allLabeledClusters.values.toVector)
          _ <- IO (
            Plotting.plotNPMI[GoldLabel](
              goldNpmis.filter { case (Duad(l, r), _) =>
                goldLabelCounts(l) > minGoldLabelCount && goldLabelCounts(r) > minGoldLabelCount
              }.mapVals(_.npmi),
              title = None, // f"Normalized PMIs ($bestCriterion%s=$bestThreshold%.2f)"
              ).render().write(new java.io.File(resultsDir.resolve("best-npmi.png").toString))
          )
          keys = List(
            "A0", "A1",
            "A2", "A3",
            // "DIR",
            "LOC",
            "ADV",
            "MNR",
            "PNC",
            "CAU",
            "TMP"
          )
          _ <- IO(
            Plotting.plotNPMI[String](
              goldNpmis.map { case (k, v) => k.map(_.show.replaceAll("AM-", "")) -> v.npmi },
              title = None, // f"Normalized PMIs ($bestCriterion%s=$bestThreshold%.2f)",
              keys = keys
            ).render().write(new java.io.File(resultsDir.resolve("selected-best-npmi.png").toString))
          )
          _ <- IO(
            Plotting.plotNPMI[String](
              goldNpmis.map { case (k, v) => k.map(_.show.replaceAll("AM-", "")) -> v.prob },
              title = None, // f"Normalized PMIs ($bestCriterion%s=$bestThreshold%.2f)",
              keys = keys
            ).render().write(new java.io.File(resultsDir.resolve("selected-best-jointprobs.png").toString))
          )
          _ <- IO {
            val b3instanceByVerbAndLabel = allLabeledClusters.mapVals { clusters =>
              val localGoldLabelCounts = clusters.unorderedFold
              ClusterPRMetric.b3PerInstanceByLabel(localGoldLabelCounts, clusters)
            }
            val b3instanceByVerb = b3instanceByVerbAndLabel.mapVals(_.values.toList.combineAll)
            val b3instanceByLabel = b3instanceByVerbAndLabel.values.toList.combineAll
            val b3instanceByVerbAndLabelGroup = b3instanceByVerbAndLabel.mapVals(
              _.toList.foldMap { case (label, pr) => Map(getGoldLabelGroup(label) -> pr) }
            )
            val b3instanceByLabelGroup = b3instanceByVerbAndLabel.values.toList.foldMap(
              _.toList.foldMap { case (label, pr) => Map(getGoldLabelGroup(label) -> pr) }
            )
            Csv.writePrfTableCsv(
              resultsDir.resolve("best-b3-by-verb-and-label.csv"),
              "Verb", "Role", b3instanceByVerbAndLabel
            ) >>
              Csv.writePrfTableCsv(
                resultsDir.resolve("best-b3-by-verb-and-label-group.csv"),
                "Verb", "Role", b3instanceByVerbAndLabelGroup
              ) >>
              FileUtil.writeString(
                resultsDir.resolve("best-b3-by-verb.html"))(
                Html.prfTableHtml(b3instanceByVerb).render
              ) >>
              FileUtil.writeString(
                resultsDir.resolve("best-b3-by-label.html"))(
                Html.prfTableHtml(b3instanceByLabel).render
              ) >>
              FileUtil.writeString(
                resultsDir.resolve("best-b3-by-label-group.html"))(
                Html.prfTableHtml(b3instanceByLabelGroup).render
              )
          }.flatten
        } yield ()
      )
    }
  }

  val activeMetrics: List[ClusterPRMetric] = {
    import ClusterPRMetric._
    List(
      b3instance,
      // b3mfs,
      // b3lfs,
      // b3label,
      purityCollocation
      // b3verb,
      // purityCollocationByVerb
    )
  }

  def evaluateClusters[VerbType : Show, InstanceId, GoldLabel : Order : Show](
    itemDists: Option[Map[VerbType, NonMergingMap[InstanceId, Map[QuestionTemplate, Double]]]],
    resultsDir: NIOPath,
    modelName: String,
    clusterings: Map[VerbType, Clustering[InstanceId]],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    tuningSpecs: NonEmptyList[SplitTuningSpec])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ) = activeMetrics.traverse(metric =>
    runClusteringEvalWithMetric(itemDists, resultsDir, modelName, clusterings, getGoldLabel, tuningSpecs, metric)
  ).void

  def evaluateArgumentClusters[VerbType : Show, Arg](
    questionDists: Map[VerbType, NonMergingMap[ArgumentId[Arg], Map[QuestionTemplate, Double]]],
    resultsDir: NIOPath,
    modelName: String,
    argClusterings: Map[VerbType, Clustering[ArgumentId[Arg]]],
    argRoleLabels: Map[VerbType, NonMergingMap[ArgumentId[Arg], PropBankRoleLabel]],
    tuningSpecs: NonEmptyList[SplitTuningSpec],
    useSenseSpecificRoles: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    if(useSenseSpecificRoles) {
      val getLabel = (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId)
      evaluateClusters(Some(questionDists), resultsDir, modelName, argClusterings, getLabel, tuningSpecs).void
    } else {
      val getLabel = (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId).role
      evaluateClusters(Some(questionDists), resultsDir, modelName, argClusterings, getLabel, tuningSpecs)(
        implicitly[Show[VerbType]], EvalUtils.conll08RoleOrder, implicitly[Show[String]], Log, timer
      )
    }
  }

  // summarize

  def evaluateModels[VerbType, InstanceId, GoldLabel](
    resultsDir: NIOPath,
    metric: ClusterPRMetric,
    models: Map[String, (Map[VerbType, Clustering[InstanceId]], SplitTuningSpec)],
    getGoldLabel: VerbType => InstanceId => GoldLabel,
    includeOracle: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    for {
      tunedStatsByModel <- models.toList.infoBarTraverse("Getting tuned P/R curves for models") { case (model, (argClusterings, tuningSpec)) =>
        Log.info(model) >> getAllPRStats(None, argClusterings, getGoldLabel, metric, model.startsWith("gold"))
          .flatMap(tuningSpec.run(_))
          .map(model -> _)
      }.map(_.toMap)
      _ <- Log.infoBranch(s"Plotting best tuned P/R curves") {
        PlottingJVM.plotPrecisionRecallCurves(
          tunedStatsByModel,
          "Best tuned performance", metric.precisionName, metric.recallName,
          resultsDir.resolve("best.png")
        )
      }
      _ <- FileUtil.writeString(resultsDir.resolve("best.txt"))(
        getMetricsString(
          Chosen(
            tunedStatsByModel.transform { case (model, stats) =>
              Chosen(
                stats.map { case (k, v) => f"${models(model)._2.criterion}%s=$k%.3f" -> v }.toMap
              ).keepMaxBy(_.f1)
            }
          )
        )
      )
      _ <- IO.pure(!includeOracle).ifM(
        IO.unit, for {
          oracleStatsByModel <- models.toList.infoBarTraverse("Getting oracle P/R curves for models") { case (model, (argClusterings, _)) =>
            Log.info(model) >> getAllPRStats(None, argClusterings, getGoldLabel, metric, model.startsWith("gold"))
              .flatMap(SplitTuningSpec(OracleCriterion).run(_))
              .map(model -> _)
          }.map(_.toMap)
          _ <- Log.infoBranch(s"Plotting oracle P/R curves") {
            PlottingJVM.plotPrecisionRecallCurves(
              oracleStatsByModel,
              "Oracle tuned performance", metric.precisionName, metric.recallName,
              resultsDir.resolve("oracle.png")
            )
          }
          _ <- FileUtil.writeString(resultsDir.resolve("oracle.txt"))(
            getMetricsString(
              Chosen(
                oracleStatsByModel.transform { case (model, stats) =>
                  Chosen(
                    stats.map { case (k, v) => f"oracle=$k%.3f" -> v }.toMap
                  ).keepMaxBy(_.f1)
                }
              )
            )
          )
        } yield ()
      )
    } yield ()
  }

  def evaluateArgumentModels[VerbType, Arg](
    resultsDir: NIOPath,
    metric: ClusterPRMetric,
    models: Map[String, (Map[VerbType, Clustering.Argument[Arg]], SplitTuningSpec)],
    argRoleLabels: Map[VerbType, NonMergingMap[ArgumentId[Arg], PropBankRoleLabel]],
    useSenseSpecificRoles: Boolean,
    includeOracle: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    val getRoleLabel = if(useSenseSpecificRoles) {
      (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId)
    } else {
      (verbType: VerbType) => (argId: ArgumentId[Arg]) => argRoleLabels(verbType).value(argId).role
    }
    evaluateModels(resultsDir, metric, models, getRoleLabel, includeOracle)
  }

  def evaluateVerbModels[VerbType](
    resultsDir: NIOPath,
    metric: ClusterPRMetric,
    models: Map[String, (Map[VerbType, Clustering.Verb], SplitTuningSpec)],
    getVerbSenseLabel: VerbType => VerbId => String,
    includeOracle: Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String], timer: Timer[IO]
  ): IO[Unit] = {
    evaluateModels(
      resultsDir, metric, models,
      (verbType: VerbType) => (verbId: VerbId) => getVerbSenseLabel(verbType)(verbId),
      includeOracle
    )
  }

  // def doPropBankClusterDebugging(
  //   config: Config,
  //   senseLabels: Instances.PropBankLabels,
  //   verbModelsByConfig: Map[VerbSenseConfig, Map[String, PropBankVerbClusterModel]],
  //   chosenThresholds: Map[VerbSenseConfig, Double])(
  //   implicit Log: TreeLogger[IO, String]
  // ): IO[Unit] = for {
  //   _ <- Log.infoBranch("Writing loss graph")(
  //     writeLossGraph(
  //       verbModelsByConfig.mapValues(_.mapValues(_.clusterTree)),
  //       config.globalResultsDir.map(_.resolve("loss-trends.png"))
  //     )
  //   )
  //   _ <- Log.infoBranch("Writing depth graph")(
  //     writeDepthGraph(
  //       verbModelsByConfig.mapValues(_.mapValues(_.clusterTree)),
  //       config.globalResultsDir.map(_.resolve("depth-trends.png"))
  //     )
  //   )
  //   _ <- Log.infoBranch("Writing PropBank gold sense graph") {
  //     import com.cibo.evilplot._
  //     import com.cibo.evilplot.numeric._
  //     import com.cibo.evilplot.plot._
  //     import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //     import com.cibo.evilplot.plot.renderers.PointRenderer

  //     case class ThisPoint(
  //       lemma: String,
  //       numOccurrences: Int,
  //       numSenses: Int,
  //       val x: Double,
  //       val y: Double) extends Datum2d[ThisPoint] {
  //       def withXY(x: Double = this.x, y: Double = this.x) = this.copy(x = x, y = y)
  //     }

  //     val rand = new scala.util.Random(2643642L)
  //     def noise = scala.math.abs(rand.nextGaussian / 40.0)

  //     val data = senseLabels.values.iterator.map { case (lemma, sentenceMap) =>
  //       val instances = sentenceMap.iterator.flatMap(_._2.values.iterator).toList
  //       val senses = instances.toSet
  //       ThisPoint(lemma, instances.size, senses.size, scala.math.min(1000, instances.size + noise), senses.size + (noise * 10))
  //     }.toList

  //     val plot = ScatterPlot(
	// 	    data,
	// 	    pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: ThisPoint) => "gold"), size = Some(2.0)))
	//     ).xAxis().yAxis().frame().rightLegend()

  //     config.globalPropBankResultsDir.flatMap(path =>
  //       IO(plot.render().write(new java.io.File(path.resolve("propbank-sense-counts.png").toString)))
  //     )
  //   }
  //   _ <- Log.infoBranch("Writing partition sizes graph") {
  //     import com.cibo.evilplot._
  //     import com.cibo.evilplot.numeric._
  //     import com.cibo.evilplot.plot._
  //     import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //     import com.cibo.evilplot.plot.renderers.PointRenderer

  //     case class ThisPoint(
  //       model: VerbSenseConfig,
  //       numInstances: Int,
  //       numClusters: Int,
  //       x: Double, y: Double
  //     ) extends Datum2d[ThisPoint] {
  //       def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
  //     }

  //     val rand = new scala.util.Random(2643642L)
  //     def noise = scala.math.abs(rand.nextGaussian / 40.0)

  //     val data = verbModelsByConfig.toList.flatMap { case (vsConfig, verbModels) =>
  //       val maxLossPerInstance = chosenThresholds(vsConfig)
  //       verbModels.values.toList.map { model =>
  //         val numInstances = model.clusterTree.size.toInt
  //         val clusters = model.clusterTree.splitWhile(_.loss > (maxLossPerInstance * numInstances))
  //         val numClusters = clusters.size
  //         ThisPoint(vsConfig, numInstances, numClusters, scala.math.min(1000, numInstances.toDouble + noise), numClusters.toDouble + (noise * 10))
  //       }
  //     }

  //     val plot = ScatterPlot(
	// 	    data,
	// 	    pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: ThisPoint) => x.model.modelName), size = Some(1.0)))
	//     ).xAxis().yAxis().frame().rightLegend()

  //     config.globalPropBankResultsDir.flatMap(path =>
  //       IO(plot.render().write(new java.io.File(path.resolve("predicted-cluster-counts.png").toString)))
  //     )
  //   }
  //   _ <- Log.infoBranch("Writing partition size comparison graph") {
  //     import com.cibo.evilplot._
  //     import com.cibo.evilplot.numeric._
  //     import com.cibo.evilplot.plot._
  //     import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  //     import com.cibo.evilplot.plot.renderers.PointRenderer

  //     case class ThisPoint(
  //       model: VerbSenseConfig,
  //       numGoldClusters: Int,
  //       numPredictedClusters: Int,
  //       x: Double, y: Double
  //     ) extends Datum2d[ThisPoint] {
  //       def withXY(x: Double = this.x, y: Double = this.y) = this.copy(x = x, y = y)
  //     }

  //     val rand = new scala.util.Random(2643642L)
  //     def noise = scala.math.abs(rand.nextGaussian / 4.0)

  //     val data = verbModelsByConfig.toList.flatMap { case (vsConfig, verbModels) =>
  //       val maxLossPerInstance = chosenThresholds(vsConfig)
  //       verbModels.toList.flatMap { case (verbLemma, model) =>
  //         senseLabels.values.get(verbLemma).map { verbSenseLabels =>
  //           val numGoldClusters = verbSenseLabels.values.iterator.flatMap(_.values.iterator).toSet.size
  //           val numInstances = model.clusterTree.size
  //           val clusters = model.clusterTree.splitWhile(_.loss > (maxLossPerInstance * numInstances))
  //           val numPredictedClusters = clusters.size
  //           ThisPoint(vsConfig, numGoldClusters, numPredictedClusters, numGoldClusters.toDouble + noise, numPredictedClusters.toDouble + noise)
  //         }
  //       }
  //     }

  //     val pearsonR = {
  //       val num = data.map(d => d.numGoldClusters * d.numPredictedClusters).sum
  //       val denomGold = data.map(d => d.numGoldClusters * d.numGoldClusters).sum
  //       val denomPredicted = data.map(d => d.numPredictedClusters * d.numPredictedClusters).sum
  //       num.toDouble / scala.math.exp((scala.math.log(denomGold) + scala.math.log(denomPredicted)) / 2)
  //     }

  //     val plot = ScatterPlot(
	// 	    data,
	// 	    pointRenderer = Some(PointRenderer.colorByCategory(data, ((x: ThisPoint) => x.model.modelName), size = Some(1.0)))
	//     ).xAxis().yAxis().frame().rightLegend()

  //     Log.info("Pearson's R between gold and predicted number of senses: " + pearsonR) >>
  //       config.globalPropBankResultsDir.flatMap(path =>
  //         IO(plot.render().write(new java.io.File(path.resolve("cluster-num-correlation.png").toString)))
  //       )
  //   }
  //   // _ <- verbModelsByConfig.toList.traverse { case (vsConfig, verbModels) =>
  //   //   val maxLoss = chosenThresholds(vsConfig)
  //   //   verbModels.toList.traverse { case (verbLemma, verbModel) =>
  //   //     val clusters = verbModel.clusterTree.splitWhile(_.loss > maxLoss)
  //   //     IO.unit
  //   //   }
  //   // }
  // } yield ()
}
