package qasrl.roles.modeling

import qasrl.roles.modeling.eval._
import qasrl.roles.modeling.features._

import qasrl.roles.clustering._

import cats.Monoid
import cats.Order
import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.data.OptionT
import cats.data.Validated
import cats.implicits._

import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, ExitCode, IO, IOApp, Resource}

import com.monovore.decline._
import com.monovore.decline.effect._

import java.nio.file.{Path => NIOPath}
import java.nio.file.Files
import java.nio.file.Paths

import jjm.LowerCaseString
import jjm.Duad
import jjm.NonMergingMap
import jjm.ling.ESpan
import jjm.ling.Text
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.io.FileCached
import jjm.io.FileUtil
import jjm.metrics._
import jjm.implicits._

import fs2.Stream

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec
import io.circe.syntax._

import scala.util.Random

import scala.annotation.tailrec

import freelog._
import freelog.implicits._

object RoleInductionApp extends CommandIOApp(
  name = "mill -i qasrl-roles.modeling.jvm.runMain qasrl.roles.modeling.RoleInductionApp",
  header = "Do role induction tasks.") {

  implicit val logLevel = LogLevel.Trace

  var shouldRecomputeModel: Boolean = false

  def maybeGetFromCache[A](fc: FileCached[A])(implicit Log: EphemeralTreeLogger[IO, String]): IO[A] = {
    if(shouldRecomputeModel) fc.compute
    else fc.get
  }

  def getArgumentClusters[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: ArgumentModel, features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, Clustering.Argument[Arg]]]] = {
    features.splitName >>= { splitName =>
      features.modelDir
        .map(_.resolve(model.toString))
        .flatTap(createDir)
        .map(modelDir =>
          FileCached[Map[VerbType, Clustering.Argument[Arg]]](
            path = modelDir.resolve(s"model.jsonl.gz"),
            read = path => FileUtil.readJsonLines[(VerbType, Clustering.Argument[Arg])](path)
              .infoCompile(s"Reading cached models: $path")(_.toList).map(_.toMap),
            write = (path, models) => FileUtil.writeJsonLines(path)(models.toList)) {
            model.getArgumentClusters(features)
          }
        )
    }
  }

  def runArgumentRoleInduction[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: ArgumentModel, features: Features[VerbType, Arg], tuningSpecs: NonEmptyList[SplitTuningSpec])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    for {
      argTrees <- Log.infoBranch(s"Getting argument clusters") {
        getArgumentClusters[VerbType, Arg](model, features).flatMap(maybeGetFromCache)
      }
      splitName <- features.splitName
      evalDir <- features.modelDir.map(_.resolve(model.toString)).flatTap(createDir)
      _ <- features.getIfPropBank.fold(IO.unit) { features => // shadow with more specific type
        val argTreesRefined = argTrees.asInstanceOf[Map[String, Clustering.Argument[Arg]]]
        features.argQuestionDists.get.flatMap { questionDists =>
          features.argRoleLabels.get.flatMap { argRoleLabels =>
            if(features.mode.shouldEvaluate) {
              Log.infoBranch("Evaluating argument clustering (verb sense agnostic roles)")(
                Evaluation.evaluateArgumentClusters(
                  questionDists,
                  evalDir.resolve("sense-agnostic"),
                  s"$model (sense-agnostic roles)",
                  argTreesRefined, argRoleLabels,
                  tuningSpecs,
                  useSenseSpecificRoles = false
                )
              )
              // Log.infoBranch("Evaluating argument clustering (verb sense specific roles)")(
              //   Evaluation.evaluateArgumentClusters(
              //     questionDists,
              //     evalDir.resolve("sense-specific"),
              //     s"$model (sense-specific roles)",
              //     argTreesRefined, argRoleLabels,
              //     tuningSpecs,
              //     useSenseSpecificRoles = true
              //   )
              // )
            } else Log.info(s"Skipping evaluation for run mode ${features.mode}")
          }
        }
      }
    } yield ()
  }

  def getVerbClusters[VerbType: Encoder : Decoder, Arg](
    model: VerbModel, features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, Clustering.Verb]]] = {
    features.splitName >>= { splitName =>
      features.modelDir
        .map(_.resolve(model.toString))
        .flatTap(createDir)
        .map(modelDir =>
          FileCached[Map[VerbType, Clustering.Verb]](
            path = modelDir.resolve(s"model.jsonl.gz"),
            read = path => FileUtil.readJsonLines[(VerbType, Clustering.Verb)](path)
              .infoCompile(s"Reading cached models: $path")(_.toList).map(_.toMap),
            write = (path, models) => FileUtil.writeJsonLines(path)(models.toList)) {
            model.getVerbClusters(features)
          }
        )
    }
  }

  def runVerbSenseInduction[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: VerbModel, features: Features[VerbType, Arg], tuningSpecs: NonEmptyList[SplitTuningSpec])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    for {
      verbTrees <- Log.infoBranch(s"Getting verb clusters") {
        getVerbClusters[VerbType, Arg](model, features).flatMap(maybeGetFromCache)
      }
      splitName <- features.splitName
      evalDir <- features.modelDir.map(_.resolve(model.toString)).flatTap(createDir)
      _ <- features.getIfPropBank.fold(IO.unit) { features => // shadow with more specific type
        features.verbSenseLabels.get >>= { verbSenseLabels =>
          val verbTreesRefined = verbTrees.asInstanceOf[Map[String, Clustering.Verb]]
          if(features.mode.shouldEvaluate) {
            if(features.assumeGoldVerbSense) Log.info(s"Skipping verb sense evaluation since gold senses are assumed") else {
              Log.infoBranch("Evaluating verb clustering")(
                Evaluation.evaluateClusters(
                  None, evalDir.resolve("verb"), model.toString,
                  verbTreesRefined, verbSenseLabels,
                  tuningSpecs
                )
              )
            }
          } else Log.info(s"Skipping evaluation for run mode ${features.mode}")
        }
      }
    } yield ()
  }

  def getVerbFrames[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: JointModel, features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[FileCached[Map[VerbType, VerbClusterModel[VerbType, Arg]]]] = {
    features.splitName >>= { splitName =>
      features.modelDir
        .map(_.resolve(model.toString))
        .flatTap(createDir)
        .map(modelDir =>
          FileCached[Map[VerbType, VerbClusterModel[VerbType, Arg]]](
            path = modelDir.resolve(s"model.jsonl.gz"),
            read = path => FileUtil.readJsonLines[(VerbType, VerbClusterModel[VerbType, Arg])](path)
              .infoCompile(s"Reading cached models: $path")(_.toList).map(_.toMap),
            write = (path, models) => FileUtil.writeJsonLines(path)(models.toList)) {
            model.getJointClusters(features)
          }
        )
    }
  }

  def runJointFrameInduction[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: JointModel, features: Features[VerbType, Arg], tuningSpecs: NonEmptyList[SplitTuningSpec])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = {
    for {
      verbClusterModels <- Log.infoBranch(s"Getting verb clusters") {
        getVerbFrames[VerbType, Arg](model, features).flatMap(maybeGetFromCache)
      }
      splitName <- features.splitName
      evalDir <- features.modelDir.map(_.resolve(model.toString)).flatTap(createDir)
      _ <- features.getIfPropBank.fold(IO.unit) { features => // shadow with more specific type
        val verbClusterModelsRefined = verbClusterModels.asInstanceOf[Map[String, VerbClusterModel[String, Arg]]]
        if(!features.mode.shouldEvaluate) Log.info(s"Skipping evaluation for run mode ${features.mode}")
        else {
          features.argQuestionDists.get.flatMap { questionDists =>
            features.argRoleLabels.get.flatMap { argRoleLabels =>
              // : Map[String,NonMergingMap[ArgumentId[Arg],PropBankRoleLabel]]
                  Log.infoBranch("Evaluating argument clustering (verb sense agnostic roles)")(
                    Evaluation.evaluateArgumentClusters(
                      questionDists,
                      evalDir.resolve("sense-agnostic"),
                      s"$model (sense-agnostic roles)",
                      verbClusterModelsRefined.mapVals(_.argumentClustering),
                      argRoleLabels,
                      tuningSpecs,
                      useSenseSpecificRoles = false
                    )
                  )
                  // Log.infoBranch("Evaluating argument clustering (verb sense specific roles)")(
                  //   Evaluation.evaluateArgumentClusters[String, Arg](
                  //     questionDists,
                  //     evalDir.resolve("sense-specific"),
                  //     s"$model (sense-specific roles)",
                  //     verbClusterModelsRefined.mapVals(_.argumentClustering),
                  //     argRoleLabels,
                  //     tuningSpecs,
                  //     useSenseSpecificRoles = true
                  //   )
                  // )
            }
          } >> (
            if(features.assumeGoldVerbSense) {
              Log.info(s"Skipping verb sense evaluation since gold senses are assumed")
            } else {
              features.verbSenseLabels.get >>= { (verbSenseLabels: String => VerbId => String) =>
                Log.infoBranch("Evaluating verb clustering")(
                  Evaluation.evaluateClusters(
                    None, evalDir.resolve("verb"), model.toString,
                    verbClusterModelsRefined.mapVals(_.verbClustering), verbSenseLabels,
                    tuningSpecs
                  )
                )
              }
            }
          )
        }
      }
    } yield ()
  }

  def runModeling[VerbType: Encoder : Decoder, Arg: Encoder : Decoder : Order](
    model: ClusteringModel,
    features: Features[VerbType, Arg],
    tuningSpecs: NonEmptyList[SplitTuningSpec])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = model match {
    // case argBaselineModel @ ArgumentBaselineModel(_) =>
    //   runBaselineArgumentRoleInduction(argBaselineModel, features)
    case argModel: ArgumentModel =>
      runArgumentRoleInduction(argModel, features, tuningSpecs)
    case verbModel: VerbModel =>
      runVerbSenseInduction(verbModel, features, tuningSpecs)
    case jointModel: JointModel =>
      runJointFrameInduction(jointModel, features, tuningSpecs)
  }

  def readAllModels[VerbType: Decoder, ModelType: Decoder](
    dir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[NonMergingMap[String, Map[VerbType, ModelType]]] = Option(dir)
    .filterA(fileExists)
    .flatFoldMapM(path =>
      getSubdirs(path).flatMap { (modelDirs: List[NIOPath]) =>
        modelDirs.infoBarFoldMapM(s"Reading models (${dir.getFileName})") { (modelSubdir: NIOPath) =>
          val modelName = modelSubdir.getFileName.toString
          FileUtil.readJsonLines[(VerbType, ModelType)](
            modelSubdir.resolve("model.jsonl.gz")
          ).infoCompile("Reading cached models for verbs")(_.toList).map(_.toMap)
            .map(argTrees =>
              NonMergingMap(modelName -> argTrees)
            )
        }
      }
    )

  def readAllModelSpecs(isTest: Boolean, dir: NIOPath) = {
    Option(dir).filterA(fileExists).flatFoldMapM(path =>
      getSubdirs(path).flatFoldMapM { modelSubdir =>
        val modelName = modelSubdir.getFileName.toString
        getSubdirs(modelSubdir).flatFoldMapM { evalDir =>
          getSubdirs(evalDir).flatFoldMapM { metricDir =>
            for {
              metric <- IO(ClusterPRMetric.fromString(metricDir.getFileName.toString).get)
              tuningSpecStr <- FileUtil.readString(metricDir.resolve("best-setting.txt"))
              tuningSpec <- IO(SplitTuningSpec.fromString(tuningSpecStr).get).map(spec =>
                if(!isTest) spec.copy(thresholdsOverride = None) else spec
              )
            } yield Map(evalDir.getFileName.toString -> Map(metric -> NonMergingMap(modelName -> tuningSpec)))
          }
        }
      }
    )
  }

  def runSummarize[Arg: Encoder : Decoder : Order](
    features: PropBankFeatures[Arg])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    modelDir <- features.modelDir
    argModels <- readAllModels[String, Clustering.Argument[Arg]](modelDir.resolve("arg"))
    verbModels <- readAllModels[String, Clustering.Verb](modelDir.resolve("verb"))
    jointModels <- readAllModels[String, VerbClusterModel[String, Arg]](modelDir.resolve("joint"))
    allArgModels = argModels |+| NonMergingMap(jointModels.value.mapVals(_.mapVals(_.argumentClustering)))
    allVerbModels = verbModels |+| NonMergingMap(jointModels.value.mapVals(_.mapVals(_.verbClustering)))
    modelTuningDir <- features.modelTuningDir
    argModelSpecs <- readAllModelSpecs(features.mode.isTest, modelTuningDir.resolve("arg"))
    verbModelSpecs <- readAllModelSpecs(features.mode.isTest, modelTuningDir.resolve("verb"))
    jointModelSpecs <- readAllModelSpecs(features.mode.isTest, modelTuningDir.resolve("joint"))
    allArgModelSpecs = argModelSpecs |+| (jointModelSpecs - "verb")
    allVerbModelSpecs = verbModelSpecs.get("verb").combineAll |+| jointModelSpecs.get("verb").combineAll
    goldVerbSenseLabel = (if(features.assumeGoldVerbSense) "by-sense" else "by-lemma")
    _ <- {
      for {
        argRoleLabels <- features.argRoleLabels.get
        out <- features.outDir
        split <- features.splitName
        parentResultsDir = out.resolve(s"eval/$split/$goldVerbSenseLabel")
        _ <- allArgModelSpecs.toList.traverse { case (evalMode, metricSpecs) =>
            val useSenseSpecificRoles = evalMode == "sense-specific"
            val resultsDir = parentResultsDir.resolve(evalMode)
            metricSpecs.toList.infoBarTraverse(s"Recording stats ($evalMode)") { case (metric, modelSpecs) =>
              val metricDir = resultsDir.resolve(metric.name)
              Log.info(s"Metric: $metric") >> createDir(metricDir) >> Evaluation.evaluateArgumentModels(
                metricDir, metric,
                allArgModels.value.zipValues(modelSpecs.value),
                argRoleLabels,
                useSenseSpecificRoles = evalMode == "sense-specific",
                includeOracle = !features.mode.isTest
              )
            }
          }
      } yield ()
    }
    _ <- {
      for {
        verbSenseLabels <- features.verbSenseLabels.get
        out <- features.outDir
        split <- features.splitName
        parentResultsDir = out.resolve(s"eval/$split/$goldVerbSenseLabel")
        _ <- {
          val resultsDir = parentResultsDir.resolve("verb")
          if(allVerbModels.value.isEmpty) IO.unit else {
            allVerbModelSpecs.toList.infoBarTraverse(s"Recording stats (verb)") { case (metric, modelSpecs) =>
              val metricDir = resultsDir.resolve(metric.name)
              Log.info(s"Metric: $metric") >> createDir(metricDir) >> Evaluation.evaluateVerbModels(
                metricDir, metric,
                allVerbModels.value.zipValues(modelSpecs.value),
                verbSenseLabels,
                includeOracle = !features.mode.isTest
              )
            }
          }
        }
      } yield ()
    }
  } yield ()

  def runAnalyze[Arg: Encoder : Decoder : Order](
    features: PropBankFeatures[Arg],
    shouldDo: String => Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    split <- features.splitName
    outDir <- features.outDir.map(_.resolve(s"analysis/$split")).flatTap(createDir)
    _ <- Analysis.run(features, outDir, shouldDo)
  } yield ()

  def getTunedArgClustering[VerbType : Encoder : Decoder, Arg : Encoder : Decoder : Order](
    features: Features[VerbType, Arg],
    model: ArgumentModel)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, Vector[Set[ArgumentId[Arg]]]]] = for {
    bestSettingFilename <- features.modelTuningDir.map(
      _.resolve(s"$model/sense-agnostic/pur-coll/best-setting.txt")
    )
    tuningSpecStr <- FileUtil.readString(bestSettingFilename)
    tuningSpec <- IO(SplitTuningSpec.fromString(tuningSpecStr).get)
    _ <- Log.trace(s"Tuning spec: $tuningSpec")
    _ <- IO(
      require(
        Set("entropy", "num-clusters").contains(tuningSpec.criterion.name) &&
          tuningSpec.thresholds.size == 1
      )
    )
    criterion = tuningSpec.criterion.name match {
      case "entropy" => ClusterSplittingCriterion.Entropy(tuningSpec.thresholds.head)
      case "num-clusters" => ClusterSplittingCriterion.Number(tuningSpec.thresholds.head.toInt)
    }
    clusterings <- getArgumentClusters(model, features).flatMap(_.read.map(_.get))
    allClusterSets <- clusterings.toList.infoBarTraverse("Splitting clusters") { case (verb, clustering) =>
      IO {
        val res = clustering.clusterTreeOpt.foldMap(tree =>
          criterion
            .splitTree(tree, (x: Set[ArgumentId[Arg]]) => x.size.toDouble)
            .map(_.unorderedFold)
        ) ++ clustering.extraClusters.values
        verb -> res
      }
    }
  } yield allClusterSets.toMap

  def runCompare[Arg: Encoder : Decoder : Order](
    features: PropBankFeatures[Arg],
    models: Duad[ArgumentModel])(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    split <- features.splitName
    outDir <- {
      features.outDir
        .map(_.resolve(s"comparison/$split")).flatTap(createDir)
        .map(_.resolve(s"${models.min}###${models.max}")).flatTap(createDir)
    }
    clustering1 <- getTunedArgClustering(features, models.min)
    clustering2 <- getTunedArgClustering(features, models.max)
    _ <- Comparison.run(
      outDir, features,
      models.min.toString, clustering1,
      models.max.toString, clustering2)
  } yield ()

  def getFeatures(
    setting: DataSetting, mode: RunMode)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): Features[setting.VerbType, setting.Arg] = setting match {
    case DataSetting.Qasrl => new GoldQasrlFeatures(mode)
        .asInstanceOf[Features[setting.VerbType, setting.Arg]]
    case DataSetting.Ontonotes5(assumeGoldVerbSense) => new OntoNotes5Features(mode, assumeGoldVerbSense)
        .asInstanceOf[Features[setting.VerbType, setting.Arg]]
    case DataSetting.CoNLL08(assumeGoldVerbSense) => new CoNLL08Features(mode, assumeGoldVerbSense)
        .asInstanceOf[Features[setting.VerbType, setting.Arg]]
  }

  val dataO = Opts.option[String](
    "data", metavar = DataSetting.all.mkString("|"), help = "Data setting to run in."
  ).mapValidated { setting =>
    DataSetting.fromString(setting).map(Validated.valid).getOrElse(
      Validated.invalidNel(s"Invalid data setting $setting: must be one of ${DataSetting.all.mkString(", ")}")
    )
  }

  val modeO = Opts.option[String](
    "mode", metavar = "sanity|dev|test", help = "Which mode to run in."
  ).mapValidated { string =>
    RunMode.fromString(string).map(Validated.valid).getOrElse(
      Validated.invalidNel(s"Invalid mode $string: must be sanity, dev, or test.")
    )
  }

  val modelO = Opts.option[String](
    "model", metavar = "loss spec", help = "Clustering model configuration."
  ).mapValidated { string =>
    ClusteringModel.fromString(string)
      .map(Validated.valid)
      .getOrElse(Validated.invalidNel(s"Invalid model $string. (todo: better error reporting)"))
  }

  val argModelsO = Opts.options[String](
    "model", metavar = "loss spec", help = "Clustering model configuration."
  ).mapValidated { strings =>
    strings.traverse(string =>
      ArgumentModel.fromString(string)
        .map(Validated.valid)
        .getOrElse(Validated.invalidNel(s"Invalid model $string. (Needs arg model. todo: better error reporting)"))
    )
  }

  val defaultTuningSpecs = NonEmptyList.of(
    OracleCriterion,
    NumClustersCriterion,
    TotalEntropyCriterion
  ).map(SplitTuningSpec(_))

  val tuningO = Opts.options[String](
    "tune", help = "tuning spec, e.g., num-clusters=23")
    .mapValidated(
      _.traverse(arg =>
        SplitTuningSpec.fromString(arg).map(Validated.valid).getOrElse(
          Validated.invalidNel(s"Invalid tuning spec $arg. (todo: better error reporting)")
        )
      ))
    .orNone.map(_.getOrElse(defaultTuningSpecs))

  val recomputeO = Opts.flag(
    "recompute", help = "recompute clustering model even if it is cached."
  ).orFalse

  val analysisChoicesO = Opts.option[String](
    "do", help = "Choose which analyses to do.")
    .map(_.split(",").toSet).orNone
    .map(setOpt => (x: String) => setOpt.forall(_.contains(x)))

  def withLogger[A](run: SequentialEphemeralTreeLogger[IO, String] => IO[A]): IO[A] = {
    freelog.loggers.TimingEphemeralTreeFansiLogger.debounced() >>= { Log =>
      loggerUnsafe = Log
      val res = run(Log)
      res.handleErrorWith { e =>
        import java.io.{PrintWriter,StringWriter}
        val sw = new StringWriter
        val pw = new PrintWriter(sw)
        e.printStackTrace(pw)
        Log.error(sw.toString) >> Log.flush >> IO.raiseError[A](e)
      }
    }
  }

  val setup = Opts.subcommand(
    name = "setup",
    help = "Run feature setup.")(
    dataO.orNone.map { dataSettingOpt =>
      withLogger { logger =>
        implicit val Log = logger
        Log.infoBranch("Setup") {
          for {
            _ <- Log.info(s"Mode: setup")
            dataSettings = dataSettingOpt.fold(DataSetting.all)(List(_))
            _ <- Log.info(s"Data: " + dataSettings.mkString(", "))
            _ <- dataSettings.traverse(d => getFeatures(d, RunMode.Sanity).setup)
          } yield ExitCode.Success
        }
      }
    }
  )

  val run = Opts.subcommand(
    name = "run",
    help = "Run clustering / frame induction.")(
    (dataO, modeO, modelO, tuningO, recomputeO).mapN { (data, mode, model, tuning, recompute) =>
      withLogger { logger =>
        implicit val Log = logger
        Log.infoBranch("Run") {
          for {
            _ <- Log.info(s"Mode: $mode")
            _ <- Log.info(s"Data: $data")
            _ <- Log.info(s"Model: $model")
            _ <- IO(shouldRecomputeModel = recompute)
            // need to explicitly match here to make sure typeclass instances for VerbType/Arg are available
            _ <- data match {
              case d @ DataSetting.Qasrl =>
                runModeling(model, getFeatures(d, mode), tuning)
              case d @ DataSetting.Ontonotes5(_) =>
                runModeling(model, getFeatures(d, mode), tuning)
              case d @ DataSetting.CoNLL08(_) =>
                runModeling(model, getFeatures(d, mode), tuning)
            }
          } yield ExitCode.Success
        }
      }
    }
  )

  val summarize = Opts.subcommand(
    name = "summarize",
    help = "Aggregate and analyze results from all models.")(
    (dataO, modeO).mapN { (data, mode) =>
      withLogger { logger =>
        implicit val Log = logger
        Log.infoBranch("Summarize") {
          for {
            _ <- Log.info(s"Mode: $mode")
            _ <- Log.info(s"Data: $data")
            // need to explicitly match here to make sure typeclass instances for VerbType/Arg are available
            _ <- data match {
              case d @ DataSetting.Qasrl =>
                IO.raiseError(new IllegalArgumentException("Cannot evaluate on QA-SRL."))
              case d @ DataSetting.Ontonotes5(_) =>
                runSummarize(getFeatures(d, mode).getIfPropBank.get)
              case d @ DataSetting.CoNLL08(_) =>
                runSummarize(getFeatures(d, mode).getIfPropBank.get)
            }
          } yield ExitCode.Success
        }
      }
    }
  )

  val analyze = Opts.subcommand(
    name = "analyze",
    help = "Analyze features which are agnostic to the models.")(
    (dataO, modeO, analysisChoicesO).mapN { (data, mode, analysisChoices) =>
      withLogger { logger =>
        implicit val Log = logger
        Log.infoBranch("Analyze") {
          for {
            _ <- Log.info(s"Mode: $mode")
            _ <- Log.info(s"Data: $data")
            // need to explicitly match here to make sure typeclass instances for VerbType/Arg are available
            _ <- data match {
              case d @ DataSetting.Qasrl =>
                IO.raiseError(new IllegalArgumentException("Cannot run analysis on QA-SRL."))
              case d @ DataSetting.Ontonotes5(_) =>
                runAnalyze(getFeatures(d, mode).getIfPropBank.get, analysisChoices)
              case d @ DataSetting.CoNLL08(_) =>
                runAnalyze(getFeatures(d, mode).getIfPropBank.get, analysisChoices)
            }
          } yield ExitCode.Success
        }
      }
    }
  )

  val compare = Opts.subcommand(
    name = "compare",
    help = "Compare argument model performance.")(
    (dataO, modeO, argModelsO.map(_.toList)).mapN { (data, mode, models) =>
      withLogger { logger =>
        implicit val Log = logger
        Log.infoBranch("Compare") {
          for {
            _ <- Log.info(s"Mode: $mode")
            _ <- Log.info(s"Data: $data")
            modelPairs = for(x <- models; y <- models; if x != y) yield Duad(x, y)
            _ <- modelPairs.distinct.infoBarTraverse("Comparing models") { models =>
                data match {
                  case d @ DataSetting.Qasrl =>
                    IO.raiseError(new IllegalArgumentException("Cannot run analysis on QA-SRL."))
                  case d @ DataSetting.Ontonotes5(_) =>
                    runCompare(getFeatures(d, mode).getIfPropBank.get, models)
                  case d @ DataSetting.CoNLL08(_) =>
                    runCompare(getFeatures(d, mode).getIfPropBank.get, models)
                }
            }
          } yield ExitCode.Success
        }
      }
    }
  )

  def main: Opts[IO[ExitCode]] =
    setup
      .orElse(run)
      .orElse(summarize)
      .orElse(analyze)
      .orElse(compare)
}
