package qasrl.roles.modeling.features

import qasrl.roles.modeling._
import qasrl.roles.modeling.util.VectorFileUtil

import java.nio.file._

import qasrl.data.Dataset
import qasrl.ArgumentSlot
import qasrl.labeling.SlotBasedLabel
import qasrl.labeling.ClauseResolution
import qasrl.labeling.ClausalQuestion
import qasrl.labeling.QuestionTemplate

import qasrl.bank.Data

import jjm.NonMergingMap
import jjm.ling.ESpan
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.io.Cell
import jjm.io.FileCached
import jjm.io.FileUtil
import jjm.implicits._

import cats.Order
import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

import fs2.Stream

import io.circe.generic.JsonCodec
import io.circe.{Encoder, Decoder}

import monocle.function.{all => Optics}

import freelog._
import freelog.implicits._

// TODO change to raw question string (or slots) for arg id
// and add clausal question as an extra feature for interpretability and/or clustering stuff
class GoldQasrlFeatures(
  mode: RunMode)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]
) extends Features[InflectedForms, ClausalQuestion](mode)(implicitly[Encoder[InflectedForms]], implicitly[Decoder[InflectedForms]], cs, Log) {

  override def getVerbLemma(verbType: InflectedForms): String = verbType.stem

  def renderVerbType(verbType: InflectedForms): String = verbType.allForms.mkString(", ")

  override val rootDir = Paths.get("frame-induction/qasrl")

  val qasrlBankPath = Paths.get("../qasrl-bank/data/qasrl-v2_1")

  private[this] def readQasrlDataset(name: String) =
    Log.infoBranch(s"Reading QA-SRL dataset $name")(
      readDataset(qasrlBankPath.resolve(name + ".jsonl.gz"))
    )

  implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)

  val qasrlBank = Cell(
    Log.infoBranch("Reading QA-SRL Bank")(
      IO(Data.readFromQasrlBank(qasrlBankPath).toEither.right.get)
    )
  )

  val dataset: RunDataCell[Dataset] = RunData(
    train = "expanded/train",
    dev = "expanded/dev",
    test = "orig/test").flatMap(
    spec => readQasrlDataset(spec).map(filterDatasetNonDense)
  ).toCell

  val qaPairs: RunDataCell[Map[InflectedForms, NonMergingMap[VerbId, QAPairs]]] = {
    dataset.data.map(
      _.sentences.iterator.flatMap { case (sid, sentence) =>
        sentence.verbEntries.values.map(sid -> _)
      }.toList.groupBy(_._2.verbInflectedForms).map {
        case (verbInflectedForms, pairs) =>
          verbInflectedForms -> pairs.groupBy(_._1).toList.foldMap { case (sid, pairs) =>
            NonMergingMap(
              pairs.map(_._2).map(v => v.verbIndex -> v).toMap.map { case (verbIndex, verb) =>
                val qLabels = verb.questionLabels.values.toList
                val resolvedQs = ClauseResolution.getResolvedFramePairs(
                  verbInflectedForms, qLabels.map(_.questionSlots)
                ).map(Function.tupled(ClausalQuestion(_, _)))
                VerbId(sid, verbIndex) -> qLabels.zip(resolvedQs).map { case (qLabel, clausalQ) =>
                  clausalQ -> (
                    qLabel.answerJudgments.toList.flatMap(_.judgment.getAnswer).map(_.spans.toList)
                  )
                }.toMap
              }
            )
          }
      }
    )
  }.toCell

  override val verbArgSets: RunDataCell[Map[InflectedForms, Map[VerbId, Set[ClausalQuestion]]]] =
    qaPairs.data.map(
      _.mapVals { verbs =>
        verbs.value.mapVals { qaPairs =>
          qaPairs.keySet
        }
      }
    ).toCell

  val argIdToSpans: ArgFeats[List[List[ESpan]]] = qaPairs.data.map(
    _.mapVals { verbs =>
      verbs.value.toList.foldMap { case (verbId, qaPairs) =>
        NonMergingMap(
          qaPairs.map { case (cq, spanLists) =>
            ArgumentId(verbId, cq) -> spanLists
          }
        )
      }
    }
  ).toCell.data

  // TODO replace with this
  // qaPairs
  //   .map(_.sentences.map { case (sid, sent) => sid -> sent.sentenceTokens })
  //   .map(NonMergingMap.apply[String, Vector[String]])
  //   .toCell("QA-SRL sentences")

  override val verbInflectedFormLists = IO(List(_))

  override val sentences = dataset.data
    .map(_.sentences.map { case (sid, sent) => sid -> sent.sentenceTokens })
    .map(NonMergingMap.apply[String, Vector[String]])
    .toCell

  // TODO temp before we have distribution results from the models
  // override val argQuestionDists: RunDataCell[ArgFeats[Map[QuestionTemplate, Double]]] = {
  //   argIdToSpans.data.map(
  //     _.mapVals { args =>
  //       NonMergingMap(
  //         args.value.transform { case (argId, _) =>
  //           Map(QuestionTemplate.fromClausalQuestion(argId.argument) -> 1.0)
  //         }
  //       )
  //     }
  //   ).toCell("QA-SRL question distributions for questions")
  // }

  // TODO incorporate gold question
  override lazy val argQuestionDists: CachedArgFeats[Map[QuestionTemplate, Double]] = {
    RunData.strings.zip(verbIdToType.data).zip(qaPairs.data)
      .flatMap { case ((split, vidToType), qaPairs) =>
      val qgPath = inputDir.resolve(s"qg/$split.jsonl.gz")
      FileUtil.readJsonLines[QGen.SentencePrediction](qgPath)
        .map { case QGen.SentencePrediction(sid, sentenceTokens, verbs) =>
          verbs.foldMap { case QGen.VerbPrediction(vi, spanPreds) =>
            val verbId = VerbId(sid, vi)
            // verbId may not be present if all QAs were filtered out (ie it was a bad predicate)
            vidToType.value.get(verbId).foldMap { verbType =>
              val qas = qaPairs(verbType).value(verbId)
              Map(
                verbType -> NonMergingMap(
                  qas.map { case (cq, spanLists) =>
                    // We might miss a few spans, which were judged very low probability by the model
                    // so they weren't decoded.
                    val spanPredLists = spanLists
                      .map(_.flatMap(s => spanPreds.find(_.span == s)))
                      .filter(_.nonEmpty)
                    if(spanPredLists.isEmpty) {
                      // back off to gold question (should be rare) if ALL spans in ALL answers are missed
                      val argId = ArgumentId(verbId, cq)
                      val qDist = Map(QuestionTemplate.fromClausalQuestion(cq) -> 1.0)
                      System.err.println(s"oopsie: $argId")
                      argId -> qDist
                    } else ArgumentId(verbId, cq) -> spanPredLists.foldMap { localPreds =>
                      val denom = localPreds.foldMap(_.spanProb) * spanPredLists.size
                      localPreds.foldMap(
                        _.questions.mapVals { prob =>
                          prob / denom
                        }
                      )
                    }
                  }
                )
              )
            }
          }
        }.infoCompile(s"Processing QG Predictions ($split)")(_.foldMonoid)
    }.toCell
  }

  override val argSpans: CachedArgFeats[Map[ESpan, Double]] = qaPairs.data.map(
    _.mapVals { verbs =>
      verbs.value.toList.foldMap { case (verbId, qaPairs) =>
        NonMergingMap(
          qaPairs.map { case (cq, spanLists) =>
            ArgumentId(verbId, cq) -> spanLists.foldMap(spans => spans.map(_ -> (1.0 / spans.size)).toMap)
          }
        )
      }
    }
  ).toCell

  override lazy val argSemanticHeadIndices: CachedArgFeats[Set[Int]] = {
    cacheArgFeats("Argument semantic head indices")(
      RunData.strings.map(_ =>
        (verbType: InflectedForms) => (argId: ArgumentId[ClausalQuestion]) => Set.empty[Int]
      )
    )
  }

  def argSyntacticFunctions: CachedArgFeats[String] = ???

  def argSyntacticFunctionsConverted: CachedArgFeats[String] = ???

  def argConstituentTypeDists: CachedArgFeats[Map[String, Double]] = ???

  def argConstituentTypeDistsConverted: CachedArgFeats[Map[String, Double]] = ???
}
