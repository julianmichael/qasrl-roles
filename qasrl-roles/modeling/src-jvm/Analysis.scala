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

import jjm.Duad
import jjm.LowerCaseString
import jjm.io.FileUtil
import jjm.metrics.WeightedNumbers
import jjm.implicits._

import qasrl.labeling.QuestionTemplate

import qasrl.roles.modeling.features.PropBankFeatures
import qasrl.roles.modeling.eval.EvalUtils
import qasrl.roles.modeling.eval.Plotting

object Analysis {

  def run[Arg: Encoder : Decoder : Order](
    features: PropBankFeatures[Arg],
    outDir: NIOPath,
    shouldDo: String => Boolean)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    _ <- IO(shouldDo("role-questions")).ifM(reportRoleQuestionDists(features, outDir), IO.unit)
    _ <- IO(shouldDo("question-relatedness")).ifM(reportQuestionPairRelatednessFull(features, outDir), IO.unit)
    _ <- IO(shouldDo("rule-lexica")).ifM(reportRuleLexica(features, outDir), IO.unit)
    _ <- IO(shouldDo("wh-npmis")).ifM(reportWhNpmis(features, outDir), IO.unit)
    _ <- IO(shouldDo("sense-counts")).ifM(reportSenseCounts(features, outDir), IO.unit)
  } yield ()

  def reportSenseCounts[Arg](
    features: PropBankFeatures[Arg],
    outDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    senseLabels <- features.verbSenseLabels.get
    _ <- FileUtil.writeString(outDir.resolve("sense-counts.txt"))(
      senseLabels.toList.map { case (verbType, labels) =>
        val numSenses = labels.value.values.toSet.size
        s"$verbType\t$numSenses"
      }.mkString("\n")
    )
  } yield ()

  def reportRoleQuestionDists[Arg](
    features: PropBankFeatures[Arg],
    outDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    argRoleLabels <- features.argRoleLabels.get
    questionDists <- features.argQuestionDists.get
    args <- features.args.get
    roleQuestionDists <- args.toList.infoBarFoldMapM("Aggregating question distributions") {
      case (verbType, argIds) =>
        IO {
          val roleLabels = argRoleLabels(verbType)
          val questions = questionDists(verbType)
          argIds.unorderedFoldMap(argId =>
            Map(roleLabels(argId).role -> questions(argId))
          )
        }
    }
    _ <- FileUtil.writeString(outDir.resolve("questions.txt"))(
      roleQuestionDists.toList.map { case (role, qdist) =>
        val total = qdist.unorderedFold
        s"$role:\n" + qdist.toList
          .sortBy(-_._2)
          .map(p => p.copy(_2 = p._2 / total))
          .takeWhile(_._2 > 0.005)
          .map { case (qt, prob) => f"${qt.toQuestionString}%-60s$prob%.3f" }
          .mkString("\n")
      }.mkString("\n==========\n")
    )
    _ <- FileUtil.writeString(outDir.resolve("questions.tex"))(
      roleQuestionDists.toList.map { case (role, qdist) =>
        val total = qdist.unorderedFold
        s"\\role{$role}\n" + qdist.toList
          .sortBy(-_._2)
          .map(p => p.copy(_2 = p._2 / total))
          .takeWhile(_._2 > 0.005)
          .map { case (qt, prob) => f"& ${qt.toQuestionString}%-50s & $prob%.2f \\\\" }
          .mkString("\n")
      }.mkString("\n\\cmidrule{2-3}\n")
    )
  } yield ()

  def includeWhRolePair(wh: String, role: String) = {
    val badRoles = Set(
      "A4", "A5", "AA",
      "AM-NEG", "AM-MOD", "AM-DIS",
      "AM-PRD", "AM-EXT" //, "AM-DIR"
    )
    !badRoles.contains(role)
  }

  def reportWhNpmis[Arg](
    features: PropBankFeatures[Arg],
    outDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    argRoleLabels <- features.argRoleLabels.get
    questionDists <- features.argQuestionDists.get
    args <- features.args.get
    cooccurrences <- args.toList.infoBarFoldMapM("Constructing wh/role cooccurrence sets") {
      case (verbType, argIds) =>
        Log.trace(s"$verbType (${argIds.size} args)") >> IO {
          val roleLabels = argRoleLabels(verbType)
          val questions = questionDists(verbType)
          argIds.toVector.map { argId =>
            val whDist = questions(argId).toList.foldMap { case (q, count) =>
              Map(q.wh.toString -> count)
            }
            (verbType, 1.0,
             whDist,
             Map(roleLabels(argId).role.replaceAll("AM-", "") -> 1.0)
            )
          }
        }
    }
    npmis <- EvalUtils.calculateHeterogeneousNPMIsLoggingEfficient[
      String, String, String, Double](
      cooccurrences)
    xKeys = List(
      "what",
      "how much",
      "where",
      "how", "why",
      "when"
        // "how long"
    )
    yKeys = List(
      "A0", "A1",
      "A2", "A3",
      // "AM-DIR",
      "LOC",
      "ADV",
      "MNR",
      "PNC",
      "CAU",
      "TMP"
    )
    _ <- IO {
      import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
      Plotting.plotHeterogeneousNPMI[String, String](
        npmis.mapVals(_.npmi).filter(p => includeWhRolePair(p._1._1, p._1._2)),
        title = None,//f"Normalized PMI: wh/role",
        xKeys = xKeys,
        yKeys = yKeys
      ).render().write(new java.io.File(outDir.resolve("wh-role-npmi.png").toString))
    }
    _ <- IO {
      import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
      import com.cibo.evilplot.colors.HTMLNamedColors
      import com.cibo.evilplot.colors.ScaledColorBar
      val whMarginals = npmis.toList.foldMap { case ((wh, _), res) => Map(wh -> res.prob) }
      val roleMarginals = npmis.toList.foldMap { case ((_, role), res) => Map(role -> res.prob) }
      val roleWhProbs = npmis.map { case ((wh, role), x) => (wh, role) -> x.prob / roleMarginals(role) }
        .filter(p => includeWhRolePair(p._1._1, p._1._2))
        // .filter(p => Set("A0", "A1").forall(_ != p._1._2))
      Plotting.plotHeterogeneousNPMI[String, String](
        roleWhProbs,
        title = None, // f"Wh probabilities for each role",
        xKeys = xKeys, yKeys = yKeys,
        colors = ScaledColorBar.apply(
          List(HTMLNamedColors.white, HTMLNamedColors.blue), 0.0, 1.0
        )
      ).render().write(new java.io.File(outDir.resolve("role-wh-probs.png").toString))

      val whRoleProbs = npmis.map { case ((wh, role), x) => (wh, role) -> x.prob / whMarginals(wh) }
        .filter(p => includeWhRolePair(p._1._1, p._1._2))
      Plotting.plotHeterogeneousNPMI[String, String](
        whRoleProbs,
        title = None, // f"Role probabilities for each wh",
        xKeys = xKeys, yKeys = yKeys,
        colors = ScaledColorBar.apply(
          List(HTMLNamedColors.white, HTMLNamedColors.blue), 0.0, 1.0
        )
      ).render().write(new java.io.File(outDir.resolve("wh-role-probs.png").toString))
    }
  } yield ()

  val reasonableQuestionFrequencyCutoff = 5.0
  val reasonablePairOneSidedFrequencyCutoff = 15.0

  def writeMaxStatQuestions(
    npmis: Vector[(Duad[QuestionTemplate],EvalUtils.NPMIResult[String])],
    outDir: NIOPath, label: String, stat: EvalUtils.NPMIResult[String] => Double)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = {
    val path = outDir.resolve(s"question-$label.tsv")
    Log.infoBranch(s"Writing question ${label}s to $path") {
      FileUtil.writeString(path)(
        npmis
          // .filter(p => stat(p._2) > 0.0)
          .sortBy(p => -stat(p._2))
          .map { case (Duad(q1, q2), result) =>
            val topSources = result.sourceDistribution.toVector.sortBy(-_._2).take(20)
            val topSourcesString = topSources.map(_._1).mkString(", ")
            val topSourceLogProbsString = topSources.map { case (source, prob) =>
              f"${scala.math.log(prob) / scala.math.log(2)}%.2f"
            }.mkString(", ")
            s"${q1.toQuestionString}\t${q2.toQuestionString}\t${stat(result)}\t${topSourcesString}\t${topSourceLogProbsString}"
          }.mkString("\n")
      )
    }
  }

  def reportQuestionPairRelatedness[Source: Show](
    flatQuestionDists: Vector[(Source, Map[QuestionTemplate, Double])],
    outDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    marginals <- flatQuestionDists.toVector.infoBarFoldMapM("Computing question marginals")(_._2.pure[IO])
    totalQuestionCount = marginals.unorderedFold
    marginalsPath = outDir.resolve("question-marginals.txt")
    _ <- Log.infoBranch(s"Writing question marginals to $marginalsPath") {
      FileUtil.writeString(marginalsPath)(
        marginals.toVector.sortBy(-_._2).map { case (q, c) =>
          f"${q.toQuestionString}%-50s $c%9.1f ${c/totalQuestionCount}%.10f"
        }.mkString("\n")
      )
    }
    acceptableQuestions = {
      marginals
        .filter(_._2 > reasonableQuestionFrequencyCutoff).keySet
        .filter(_.wh == "what".lowerCase)
    }
    _ <- Log.info(s"""|${acceptableQuestions.size} "what" questions
                      |of frequency > $reasonableQuestionFrequencyCutoff."""
                    .stripMargin.replace("\n", " "))
    unfilteredNpmis <- Log.infoBranch("Calculating question NPMIs") {
      EvalUtils.calculateNPMIsLoggingEfficient(
        flatQuestionDists.map { case (source, counts) =>
          source.show -> counts.filter(p => acceptableQuestions.contains(p._1))
        }.toVector
      )
    }
    npmis = unfilteredNpmis.toVector.filter(
      p => p._1.min != p._1.max &&
        List(p._1.min, p._1.max).exists(q => marginals(q) > reasonablePairOneSidedFrequencyCutoff)
    )
    dir <- outDir.resolve("question-relatedness").pure[IO].flatTap(createDir)
    _ <- writeMaxStatQuestions(npmis, dir, "npmi", _.npmi)
    _ <- writeMaxStatQuestions(npmis, dir, "pmi", _.pmi)
    _ <- writeMaxStatQuestions(npmis, dir, "correlation", _.correlation)
    _ <- writeMaxStatQuestions(npmis, dir, "covariance", _.covariance)
    _ <- writeMaxStatQuestions(
      npmis.filter(p => !isBoringPair(p._1)),
      dir, "covariance-exciting", _.covariance
    )
  } yield ()

  def reportQuestionPairRelatednessFull[Arg](
    features: PropBankFeatures[Arg],
    outDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = Log.infoBranch("Reporting question pair relatedness") {
    for {
      questionDists <- features.argQuestionDists.get
      flatQuestionDists <- Log.infoBranch("Flattening question distributions") {
        IO(
          questionDists.toVector.flatMap { case (verb, argQs) =>
            argQs.value.values.toVector.map(verb -> _)
          }
        )
      }
      _ <- reportQuestionPairRelatedness(flatQuestionDists, outDir)
    } yield ()
  }

  def isBoringPair(pair: Duad[QuestionTemplate]) = {
    pair.min.copy(prep = pair.max.prep, obj2 = pair.max.obj2) == pair.max
  }

  def reportRuleLexica[Arg](
    features: PropBankFeatures[Arg],
    outDir: NIOPath)(
    implicit Log: SequentialEphemeralTreeLogger[IO, String]
  ): IO[Unit] = for {
    lexicaDir <- IO(outDir.resolve("lexica")).flatTap(createDir)
    _ <- {
      def writeLexicon(name: String, lexicon: Set[LowerCaseString], italicize: Boolean) = {
        FileUtil.writeString(lexicaDir.resolve(s"$name.txt"))(
          s"${lexicon.size} items.\n" +
            lexicon.toList.sorted.map(x => if(italicize) s"\\textit{$x}" else x.toString).mkString(", ")
        )
      }
      List(
        "negation" -> SideClusteringModel.negationWords,
        "modals" -> SideClusteringModel.modals,
        "discourse" -> SideClusteringModel.discourseExpressions
      ).traverse { case (name, vocab) =>
          writeLexicon(name, vocab, false) >> writeLexicon(s"$name-it", vocab, true)
      }
    }
  } yield ()
}
