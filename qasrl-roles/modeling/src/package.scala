package qasrl.roles

import cats.Functor
import cats.Order
import cats.data.NonEmptySet
import cats.implicits._

import monocle.Iso

import jjm.Duad
import jjm.NonMergingMap
import jjm.ling.ESpan
import jjm.ling.en.InflectedForms
import jjm.implicits._

import qasrl.ArgStructure
import qasrl.ArgumentSlot
import qasrl.Frame
import qasrl.data.QuestionLabel
import qasrl.data.VerbEntry
import qasrl.labeling.ClausalQuestion

import freelog.LogLevel

import scala.collection.immutable.SortedSet

package object modeling extends qasrl.roles.modeling.PackagePlatformExtensions {
  implicit val logLevel = LogLevel.Trace

  import freelog.EphemeralTreeLogger
  var loggerUnsafe: EphemeralTreeLogger[cats.effect.IO, String] = null

  type QuestionId = ArgumentId[ClausalQuestion]
  object QuestionId {
    def apply(verbId: VerbId, question: ClausalQuestion) = ArgumentId(verbId, question)
    def unapply(argId: QuestionId): Some[(VerbId, ClausalQuestion)] = Some(argId.verbId -> argId.argument)
  }

  // type TemplateQ = (ArgStructure, ArgumentSlot)
  type QAPairs = Map[ClausalQuestion, List[List[ESpan]]]

  def nonEmptySetOptionIso[A: Order] = Iso[Option[NonEmptySet[A]], Set[A]](
    _.foldMap(_.toSortedSet: Set[A]))(
    s => NonEmptySet.fromSet(SortedSet(s.toSeq:_*)))

  object Auxiliaries {
    final val doVerbs = Set("do", "does", "doing", "did", "done").map(_.lowerCase)
    final val beVerbs =
      Set("be", "being", "been", "am", "'m", "is", "'s", "ai", "are", "'re", "was", "were").map(
        _.lowerCase
      )
    val willVerbs = Set("will", "'ll", "wo").map(_.lowerCase)

    val haveVerbs =
      Set("have", "having", "'ve", "has", "had", "'d").map(_.lowerCase)
    val wouldVerbs = Set("would", "'d").map(_.lowerCase)

    val modalVerbs = Set("can", "ca", "could", "may", "might", "must", "shall", "should", "ought")
      .map(_.lowerCase) ++ wouldVerbs

    val auxiliaryVerbs =
      doVerbs ++ beVerbs ++ willVerbs ++ haveVerbs ++ modalVerbs

    val negationWords = Set("no", "not", "n't").map(_.lowerCase)
  }

  //   def filterGold(minNumAnswers: Int, maxNumInvalid: Int) = (verb: VerbEntry) => {
  //   val (invalids, valids) = verb.questionLabels.toList.flatMap {
  //     case (questionString, qLabel) =>
  //       val judgments = qLabel.answerJudgments.toList.map(_.judgment)
  //       val numInvalid = judgments.filter(_.isInvalid).size
  //       val numAnswers = judgments.size
  //       if(numAnswers >= minNumAnswers) {
  //         if(numInvalid <= maxNumInvalid) Some(Right(questionString -> qLabel))
  //         else Some(Left(questionString -> qLabel))
  //       } else None
  //   }.separate
  //   invalids.toMap -> valids.toMap
  // }

  // val filterGoldNonDense = filterGold(3, 0)
  // val filterGoldDense = filterGold(6, 1)

  def questionLabelIsValid(minNumAnswers: Int, maxNumInvalid: Int) = (qLabel: QuestionLabel) => {
    val judgments = qLabel.answerJudgments.toList.map(_.judgment)
    val numInvalid = judgments.filter(_.isInvalid).size
    val numAnswers = judgments.size
    if(numAnswers >= minNumAnswers) {
      if(numInvalid <= maxNumInvalid) true
      else false
    } else false
  }

  val questionLabelIsValidNonDense = questionLabelIsValid(3, 0)
  val questionLabelIsValidDense = questionLabelIsValid(6, 1)

  def filterOrigAnnotationRound(verb: VerbEntry): VerbEntry = {
    val newQuestionLabels = scala.collection.immutable.SortedMap(
      verb.questionLabels.values.toList
        .filter(_.questionSources.exists(_.startsWith("turk-qasrl2.0-")))
        .map { qLabel =>
          val ajs = qLabel.answerJudgments.filter(aj =>
            !(aj.sourceId.endsWith("-expansion")) && !(aj.sourceId.endsWith("-eval"))
          )
          qLabel.copy(answerJudgments = ajs)
        }
        .filter(_.answerJudgments.nonEmpty)
        .map(qLabel => qLabel.questionString -> qLabel): _*
    )
    verb.copy(questionLabels = newQuestionLabels)
  }
}
