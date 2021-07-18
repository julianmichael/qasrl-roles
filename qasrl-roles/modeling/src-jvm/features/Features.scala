package qasrl.roles.modeling.features

import qasrl.roles.modeling._
import qasrl.roles.modeling.util.VectorFileUtil

import java.nio.file._

import qasrl.data.Dataset
import qasrl.ArgumentSlot
import qasrl.labeling.SlotBasedLabel
import qasrl.labeling.QuestionTemplate

import qasrl.bank.Data

import jjm.LowerCaseString
import jjm.NonMergingMap
import jjm.ling.ESpan
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.io.Cell
import jjm.io.FileCached
import jjm.io.FileUtil
import jjm.implicits._

import cats.Applicative
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

abstract class Features[VerbType : Encoder : Decoder, Arg](
  val mode: RunMode)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]) {

  implicit protected val runMode = mode

  type CachedVerbFeats[A] = RunDataCell[Map[VerbType, NonMergingMap[VerbId, A]]]
  type VerbFeats[A] = RunData[VerbType => VerbId => A]

  type CachedArgFeats[A] = RunDataCell[Map[VerbType, NonMergingMap[ArgumentId[Arg], A]]]
  type ArgFeats[A] = RunData[VerbType => ArgumentId[Arg] => A]

  // def makeArgFeats[A, B](f: (VerbId, A) => List[(Arg, B)]): VerbFeats[A] => ArgFeats[B] = {
  //   _.transform { case (_, verbs) =>
  //     verbs.value.toList.foldMap { case (verbId, feat) =>
  //       f(verbId, feat).foldMap { case (arg, argFeat) =>
  //         NonMergingMap(ArgumentId(verbId, arg) -> argFeat)
  //       }
  //     }
  //   }
  // }

  def cacheVerbFeats[A](name: String)(feats: VerbFeats[A]): CachedVerbFeats[A] =
    (verbs.data.zip(feats)).map { case (verbs, feats) =>
      verbs.transform { case (verbType, verbs) =>
        val featsForVerbType = feats(verbType)
        NonMergingMap(
          verbs.iterator.map { verbId =>
            verbId -> featsForVerbType(verbId)
          }.toMap
        )
      }
    }.toCell

  def fileCacheVerbFeats[A: Encoder : Decoder](
    name: String, log: Boolean = false)(
    feats: VerbFeats[A]
  ): CachedVerbFeats[A] = {
    (verbs.data.zip(feats)).map { case (verbs, feats) =>
      verbs.transform { case (verbType, verbs) =>
        val featsForVerbType = feats(verbType)
        NonMergingMap(
          verbs.iterator.map { verbId =>
            verbId -> featsForVerbType(verbId)
          }.toMap
        )
      }
    }.toFileCachedCell(n => cacheDir.map(_.resolve(s"$name/$n.jsonl.gz")))(
      read = path => FileUtil.readJsonLines[(VerbType,List[(VerbId,A)])](path).compile.toList
        .map(_.map { case (vt, verbs) => vt -> NonMergingMap(verbs.toMap) }.toMap),
      write = (path, a) => FileUtil.writeJsonLines(path)(a.iterator.map { case (vt, verbs) => vt -> verbs.value.toList }.toList)
    )
  }

  def cacheArgFeats[A](name: String, log: Boolean = false)(feats: ArgFeats[A]): CachedArgFeats[A] =
    if(log) {
      (args.data.zip(feats)).flatMap { case (args, feats) =>
        Log.infoBranch(s"Caching features: $name") {
          args.toList.infoBarFoldMapM("Verb types") { case (verbType, args) =>
            val featsForVerbType = feats(verbType)
            val argsForVerbType = args.toList.infoBarFoldMapM(s"$verbType") { argId =>
              IO(NonMergingMap(argId -> featsForVerbType(argId)))
            }
            argsForVerbType.map(args => Map(verbType -> args))
          }
        }
      }.toCell
    } else {
      (args.data.zip(feats)).map { case (args, feats) =>
        args.transform { case (verbType, args) =>
          val featsForVerbType = feats(verbType)
          NonMergingMap(
            args.iterator.map { argId =>
              argId -> featsForVerbType(argId)
            }.toMap
          )
        }
      }.toCell
    }

  def fileCacheArgFeats[A: Encoder : Decoder](
    name: String, log: Boolean = false)(
    feats: ArgFeats[A])(
    implicit argDecoder: Decoder[Arg], argEncoder: Encoder[Arg]
  ): CachedArgFeats[A] = {
    // for some reason, the compiler needs this up here to be happy
    val _ = implicitly[Encoder[List[(ArgumentId[Arg], A)]]]
    if(log) {
      (args.data.zip(feats)).flatMap { case (args, feats) =>
        Log.infoBranch(s"Computing features: $name") {
          args.toList.infoBarFoldMapM("Verb types") { case (verbType, args) =>
            val featsForVerbType = feats(verbType)
            val argsForVerbType = args.toList.infoBarFoldMapM(s"$verbType") { argId =>
              IO(NonMergingMap(argId -> featsForVerbType(argId)))
            }
            argsForVerbType.map(args => Map(verbType -> args))
          }
        }
      }.toFileCachedCell(n => cacheDir.map(_.resolve(s"$name/$n.jsonl.gz")))(
        read = path => FileUtil.readJsonLines[(VerbType,List[(ArgumentId[Arg],A)])](path).compile.toList
          .map(_.map { case (vt, args) => vt -> NonMergingMap(args.toMap) }.toMap),
        write = (path, a) => FileUtil.writeJsonLines(path)(a.iterator.map { case (vt, args) => vt -> args.value.toList }.toList)
      )
    } else {
      (args.data.zip(feats)).map { case (args, feats) =>
        args.transform { case (verbType, args) =>
          val featsForVerbType = feats(verbType)
          NonMergingMap(
            args.iterator.map { argId =>
              argId -> featsForVerbType(argId)
            }.toMap
          )
        }
      }.toFileCachedCell(n => cacheDir.map(_.resolve(s"$name/$n.jsonl.gz")))(
        read = path => FileUtil.readJsonLines[(VerbType,List[(ArgumentId[Arg],A)])](path).compile.toList
          .map(_.map { case (vt, args) => vt -> NonMergingMap(args.toMap) }.toMap),
        write = (path, a) => FileUtil.writeJsonLines(path)(a.iterator.map { case (vt, args) => vt -> args.value.toList }.toList)
      )
    }
  }

  def mapArgFeats[A, B](feats: ArgFeats[A])(f: A => B): ArgFeats[B] =
    feats.map(_.andThen(_.andThen(f)))

  def mapArgFeatsWithType[A, B](feats: ArgFeats[A])(f: VerbType => A => B): ArgFeats[B] =
    feats.map(func => (vt => func(vt).andThen(f(vt))))

  def mapVerbFeats[A, B](feats: VerbFeats[A])(f: A => B): VerbFeats[B] =
    feats.map(_.andThen(_.andThen(f)))

  def mapVerbFeatsWithType[A, B](feats: VerbFeats[A])(f: VerbType => A => B): VerbFeats[B] =
    feats.map(func => (vt => func(vt).andThen(f(vt))))

  def widen[A](feats: ArgFeats[A]): RunData[VerbType => ArgumentId[Arg] => A] = feats

  // overriden for propbank features
  def getIfPropBank: Option[PropBankFeatures[Arg]] = None

  def splitName = RunData.strings.get

  def tuningSplitName = RunData.strings.apply(mode.tuningDataSplit)

  // indices of important info

  def verbInflectedFormLists: IO[VerbType => List[InflectedForms]]

  val sentences: RunDataCell[NonMergingMap[String, Vector[String]]]

  val verbArgSets: RunDataCell[Map[VerbType, Map[VerbId, Set[Arg]]]]

  lazy val sentenceInfos: RunDataCell[Map[String, SentenceInfo[VerbType, Arg]]] = {
    (sentences.data, verbArgSets.data).mapN { (sents, argSets) =>
      val sentMaps = argSets.toList.foldMap { case (verbType, allVerbs) =>
        allVerbs.toList.foldMap { case (verbId, args) =>
          Map(verbId.sentenceId -> NonMergingMap(verbId.verbIndex -> VerbInfo(verbId.verbIndex, verbType, args)))
        }
      }
      sentMaps.map { case (sid, sentMap) =>
        sid -> SentenceInfo(sid, sents.value(sid), sentMap.value)
      }
    }.toCell
  }

  lazy val sentencesByVerbType: RunDataCell[Map[VerbType, Set[String]]] =
    verbArgSets.data.map(_.mapVals(_.keySet.map(_.sentenceId))).toCell

  lazy val args: RunDataCell[Map[VerbType, Set[ArgumentId[Arg]]]] = verbArgSets.data.map(
    _.transform { case (_, verbs) =>
      verbs.toList.foldMap { case (verbId, args) =>
        args.map(arg => ArgumentId(verbId, arg))
      }
    }
  ).toCell

  lazy val verbs: RunDataCell[Map[VerbType, Set[VerbId]]] = verbArgSets.data.map(
    _.transform { case (_, verbs) => verbs.keySet }
  ).toCell

  lazy val verbIdToType = verbArgSets.data.map(
    _.toList.foldMap { case (verbType, verbs) =>
      NonMergingMap(
        verbs.toList.map { case (verbId, _) =>
          verbId -> verbType
        }.toMap
      )
    }
  ).toCell

  // Metadata

  // for constructing features
  def getVerbLemma(verbType: VerbType): String

  // for logging. could replace with Show instance?
  def renderVerbType(verbType: VerbType): String

  // Various features that need to be implemented

  def argSpans: CachedArgFeats[Map[ESpan, Double]]

  def fullArgSpanSets: CachedArgFeats[Set[ESpan]] =
    cacheArgFeats("Full arg span sets")(
      mapArgFeats(argSpans.data)(_.keySet)
    )

  def argSpansForMLM: CachedArgFeats[Map[ESpan, Double]] = argSpans

  lazy val globalQuestionPrior = argQuestionDists.data.map { qFeats =>
    val pcounts = qFeats.values.toList.foldMap(
      _.value.values.toList.combineAll
    )
    val total = pcounts.values.sum
    pcounts.mapVals(_ / total)
  }.toCell

  lazy val argQuestionDistsActive: CachedArgFeats[Map[QuestionTemplate, Double]] = {
    cacheArgFeats("active/passive normalized question dists")(
      mapArgFeats(argQuestionDists.data)(dist =>
        dist.toList.foldMap { case (qt, prob) => Map(QuestionTemplate.normalizeToActive(qt) -> prob) }
      )
    )
  }

  lazy val argQuestionDistsNormalizedAdverbials: CachedArgFeats[Map[QuestionTemplate, Double]] = {
    cacheArgFeats("adverbial normalized question dists")(
      mapArgFeats(argQuestionDists.data)(dist =>
        dist.toList.foldMap { case (qt, prob) => Map(QuestionTemplate.normalizeAdverbials(qt) -> prob) }
      )
    )
  }

  lazy val argQuestionDists: CachedArgFeats[Map[QuestionTemplate, Double]] = {
    RunData.strings.zip(verbIdToType.data).zip(argSpans.data).zip(verbArgSets.data).zip(argSemanticHeadIndices.data).flatMap {
      case ((((split, vidToType), allSpans), allVerbs), argHeadIndices) =>
        val qgPath = inputDir.resolve(s"qg/$split.jsonl.gz")
        FileUtil.readJsonLines[QGen.SentencePrediction](qgPath)
          .map { case QGen.SentencePrediction(sid, _, verbs) =>
            verbs.foldMap { case QGen.VerbPrediction(vi, spans) =>
              val verbId = VerbId(sid, vi)
              val verbType = vidToType.value(verbId)
              val argSet = allVerbs(verbType)(verbId)
              val argIds = argSet.map(ArgumentId(verbId, _))
              // val allHeadIndices = argIds.unorderedFoldMap(argHeadIndices(verbType))
              Map(
                verbType -> NonMergingMap(
                  argIds.map { argId =>
                    // val headIndices = argHeadIndices(verbType)(argId)
                    val priorArgSpans = allSpans(verbType)(argId)

                    // use head-based span finding to include everything predicted by the model above the threshold
                    // val otherHeads = allHeadIndices - headIndex
                    // val questions = spans
                    //   .filter(pred =>
                    //     priorArgSpans.contains(pred.span) ||
                    //       (pred.span.contains(headIndex) &&
                    //          otherHeads.forall(h => !pred.span.contains(h))))
                    //   .foldMap(pred =>
                    //     pred.questions.mapVals(_ * pred.spanProb)
                    //   )

                    // TODO: experiment and find best setting of spans * weighting, etc.

                    // use only prior arg spans.
                    val questions = priorArgSpans.toList.foldMap { case (span, prob) =>
                      spans.find(_.span == span).foldMap { pred =>
                        pred.questions.mapVals(_ * prob)
                      }
                    }
                    argId -> questions
                  }.toMap
                )
              )
            }
          }
          .infoCompile(s"Reading QG Predictions ($split)")(_.foldMonoid)
    }.toCell
  }

  // new style arg features
  def argSemanticHeadIndices: CachedArgFeats[Set[Int]]

  def argSyntacticFunctions: CachedArgFeats[String]

  def argSyntacticFunctionsConverted: CachedArgFeats[String]

  def argConstituentTypeDists: CachedArgFeats[Map[String, Double]]

  def argConstituentTypeDistsConverted: CachedArgFeats[Map[String, Double]]

  def argPrepositions: CachedArgFeats[Option[LowerCaseString]] = ???

  def argPrepositionDists: ArgFeats[Map[String, Double]] =
    mapArgFeats(argPrepositions.data)(
      prep => Map(prep.fold("<none>")(_.toString) -> 1.0)
    )

  def argLeadingPreps: CachedArgFeats[Map[Int, Double]] = {
    cacheArgFeats("arg leading preps")(
      RunData.splits.map { _ =>
        (verbType: VerbType) => (argId: ArgumentId[Arg]) => {
          Map[Int, Double]()
        }
      }
    )
  }

  // directories

  protected val rootDir: Path

  def outDir = IO.pure(rootDir.resolve("out")).flatTap(createDir)

  // for use by frame induction etc.
  def modelDir = splitName.map(split => rootDir.resolve(s"models/$split")).flatTap(createDir)

  def modelTuningDir = tuningSplitName.map(split =>
    rootDir.resolve(s"models/$split")
  ).flatTap(createDir)

  // for inputs to feature computation
  protected def inputDir = rootDir.resolve("input")

  // for caching features that take a while to compute
  protected def cacheDir = IO.pure(rootDir.resolve("cache")).flatTap(createDir)

  // features and setup code constructed from the other provided features

  val mlmSettings = List("masked", "repeated", "symm_left", "symm_right", "symm_both")
  def mlmFeatureDir = inputDir.resolve("mlm")
  def makeMLMFeatures[A](f: (String, Path) => A): Map[String, A] = {
    mlmSettings.map(_ -> "").toMap.transform { case (setting, _) =>
      val mlmSettingDir = mlmFeatureDir.resolve(setting)
      f(setting, mlmSettingDir)
    }
  }

  @JsonCodec case class MLMFeatureId(
    sentenceId: String,
    verbLemma: String,
    span: ESpan)

  private lazy val mlmVocabs: Map[String, Map[String, Cell[NonMergingMap[String, Vector[String]]]]] = {
    List("arg", "verb").map(label =>
      label -> makeMLMFeatures { case (setting, path) =>
        Cell(
          FileUtil.readJson[Map[String, Vector[String]]](path.resolve(s"${label}_vocabs.json"))
            .map(NonMergingMap(_))
        )
      }
    ).toMap
  }

  import breeze.linalg.DenseVector

  val mlmFeatureDim = 1024
  // type -> mode -> verb lemma -> sentence id -> span -> vector
  private lazy val mlmVectors: Map[String, Map[String, RunDataCell[Map[String, Map[String, NonMergingMap[ESpan, DenseVector[Float]]]]]]] = {
    List("arg", "verb").map(label =>
      label -> makeMLMFeatures { case (setting, path) =>
        // TODO sentences is temporary
        RunData.strings.zip(sentences.data).flatMap { case (split, sents) =>
          val idsPath = path.resolve(s"${split}_${label}_ids.jsonl.gz")
          val vecPath = path.resolve(s"${split}_${label}_vecs.bin")
          // val embPath = Paths.get(filePrefix + "_emb.bin")
          for {
            // TODO vocab is temporary
            vocabs <- mlmVocabs(label)(setting).get
            ids <- Log.infoBranch("Reading verb IDs")(
              FileUtil.readJsonLines[MLMFeatureId](idsPath).compile.toList
            )
            vecs <- Log.infoBranch(s"Reading $label MLM vectors")(
              VectorFileUtil.readDenseFloatVectorsNIO(vecPath, mlmFeatureDim)
            )
            _ <- Log.info(s"Number of IDs: ${ids.size}; Number of vectors: ${vecs.size}; embedding size: ${vecs.head.size}")
            _ <- {
              val numToCheck = 20
              val propSane = vecs.take(numToCheck)
                .foldMap(
                  _.activeValuesIterator.filter(f => f >= 0.0 && f <= 1.0).size
                ).toDouble / (numToCheck * mlmFeatureDim)
              val sanityCheckText = f"Sanity check: ${propSane * 100.0}%.1f%% of sampled vector components units are between 0 and 1."
              if(propSane < 1.0) {
                Log.warn(sanityCheckText) >>
                  Log.warn("There might be endianness issues with how you're reading the vectors") >>
                  vecs.take(numToCheck).traverse(e => Log.info(e.activeValuesIterator.take(10).mkString("\t")))
              } else Log.info(sanityCheckText)
            }
            norms <- Ref[IO].of(jjm.metrics.Numbers(Vector[Float]()))
            normedVecs <- ids.zip(vecs).infoTraverse("Normalizing vectors") { case (id, vec) =>
              import breeze.math._
              import breeze.linalg._
              val total = sum(vec)
              // if(total < 0.6) {
              //   val vocab = vocabs(id.verbLemma)
              //   val sentence = sents(id.sentenceId)
              //   val token = sentence(id.index)
              //   val topElements = vec.activeIterator.map { case (i, value) =>
              //     vocab(i) -> value
              //   }.toVector.sortBy(-_._2)
              //   System.err.println(jjm.ling.Text.render(sentence))
              //   System.err.println(s"$id ($token): $total")
              //   topElements.grouped(10).take(5).foreach { pairs =>
              //     System.err.println(pairs.map(_._1).map(x => f"$x%10s").mkString(" "))
              //     System.err.println(pairs.map(_._2).map(x => f"$x%10.5f").mkString(" "))
              //   }
              // }
              // TODO make it a parameter whether to norm vectors or not. perhaps in the clustering alg instead though.
              // or at least do this in-place or something.
              norms.update(_ |+| jjm.metrics.Numbers(total)) >> IO(vec /:/ total)
            }
            _ <- norms.get >>= (n => Log.info(s"Vector normalizers: ${getMetricsString(n)}"))
            // for now don't use the normalized vectors.
            finalVecs = vecs // normedVecs
            _ <- Log.info("Not using normalized vectors.") // remove "not" if using.
          } yield ids.zip(finalVecs).foldMap { case (mlmFeatureId, vec) =>
              Map(mlmFeatureId.verbLemma -> Map(mlmFeatureId.sentenceId -> NonMergingMap(mlmFeatureId.span -> vec)))
          }
        }.toCell
      }
    ).toMap
  }

  def verbMLMVocab = mlmVocabs("verb")
  def argMLMVocab = mlmVocabs("arg")

  def verbMLMVectors = mlmVectors("verb")
  def argMLMVectors = mlmVectors("arg")

  def getVerbMLMFeatures(mode: String): VerbFeats[DenseVector[Float]] =
    verbMLMVectors(mode).data.map { mlmFeats =>
      (verbType: VerbType) => (verbId: VerbId) => {
        val span = ESpan(verbId.verbIndex, verbId.verbIndex + 1)
        mlmFeats(getVerbLemma(verbType))(verbId.sentenceId).value(span)
      }
    }

  lazy val postprocessedVerbMLMFeatures: Map[String, Cell[CachedVerbFeats[Map[String, Float]]]] = {
    mlmSettings.map(setting =>
      setting -> Cell(
        verbMLMVocab(setting).get.map(_.value).map { vocabs =>
          cacheVerbFeats("Postprocessed verb MLM features")(
            mapVerbFeatsWithType(getVerbMLMFeatures(setting)) { verbType => vec =>
              val vocab = vocabs(getVerbLemma(verbType))
              vec.toScalaVector.zipWithIndex.map { case (prob, i) =>
                vocab(i) -> prob
              }.toMap
            }
          )
        }
      )
    ).toMap
  }

  def getArgHeadMLMFeatures(featureMode: String): ArgFeats[DenseVector[Float]] = {
    argMLMVectors(featureMode).data.zip(argSemanticHeadIndices.data).map { case (mlmFeats, getArgIndices) =>
      (verbType: VerbType) => (argId: ArgumentId[Arg]) => {
        val indices = getArgIndices(verbType)(argId)
        indices.map { index =>
          val span = ESpan(index, index + 1)
          mlmFeats(getVerbLemma(verbType))(argId.verbId.sentenceId).value(span)
        }.reduce(_ + _)
      }
    }
  }

  val placeholderVec = {
    val x = DenseVector.fill[Float](mlmFeatureDim, 1e-8f)
    x(mlmFeatureDim - 1) = (1.0f - ((mlmFeatureDim - 1) * 1e-8f))
    x
  }

  def getArgPrepMLMFeatures(featureMode: String): ArgFeats[DenseVector[Float]] = {
    argMLMVectors(featureMode).data.zip(argLeadingPreps.data).map { case (mlmFeats, getArgPreps) =>
      (verbType: VerbType) => (argId: ArgumentId[Arg]) => {
        // use the first index as a placeholder for non-prepositional arguments
        val prepIndices = getArgPreps(verbType)(argId)
        val nonPrepProb = 1.0 - prepIndices.unorderedFold
        val res = placeholderVec *:* nonPrepProb.toFloat
        prepIndices.toList.foreach { case (index, prob) =>
          val span = ESpan(index, index + 1)
          val vec = mlmFeats(getVerbLemma(verbType))(argId.verbId.sentenceId).value(span)
          res += (vec *:* prob.toFloat)
        }
        res
      }
    }
  }

  def getArgSpanMLMFeatures(featureMode: String): ArgFeats[DenseVector[Float]] = {
    argMLMVectors(featureMode).data.zip(argSpansForMLM.data).map { case (mlmFeats, getArgSpans) =>
      (verbType: VerbType) => (argId: ArgumentId[Arg]) => {
        val spans = getArgSpans(verbType)(argId)
        val res = DenseVector.zeros[Float](mlmFeatureDim)
        spans.toList.foreach { case (span, prob) =>
          val vec = mlmFeats(getVerbLemma(verbType))(argId.verbId.sentenceId).value(span)
          res += (vec *:* prob.toFloat)
        }
        res
      }
    }
  }

  lazy val postprocessedArgPrepMLMFeatures: Map[String, Cell[CachedArgFeats[Map[String, Float]]]] = {
    mlmSettings.map(setting =>
      setting -> Cell(
        argMLMVocab(setting).get.map(_.value).map { vocabs =>
          cacheArgFeats("Postprocessed arg prep MLM features")(
            mapArgFeatsWithType(getArgPrepMLMFeatures(setting)) { verbType => vec =>
              val vocab = vocabs(getVerbLemma(verbType))
              vec.toScalaVector.zipWithIndex.map { case (prob, i) =>
                vocab(i) -> prob
              }.toMap
            }
          )
        }
      )
    ).toMap
  }

  lazy val postprocessedArgSpanMLMFeatures: Map[String, Cell[CachedArgFeats[Map[String, Float]]]] = {
    mlmSettings.map(setting =>
      setting -> Cell(
        argMLMVocab(setting).get.map(_.value).map { vocabs =>
          cacheArgFeats("Postprocessed arg MLM features")(
            mapArgFeatsWithType(getArgSpanMLMFeatures(setting)) { verbType => vec =>
              val vocab = vocabs(getVerbLemma(verbType))
              vec.toScalaVector.zipWithIndex.map { case (prob, i) =>
                vocab(i) -> prob
              }.toMap
            }
          )
        }
      )
    ).toMap
  }

  // XXXXXXXXX

  // just for clauses
  // def makeVerbSpecificClauseVocab(instances: Map[String, Map[Int, QAPairs]]): Vocab[ArgStructure] = {
  //   Vocab.make(
  //     instances.values.toList.foldMap(verbMap =>
  //       verbMap.values.toList.foldMap(qMap =>
  //         qMap.keys.toList.map { case (frame, slot) =>
  //           ArgStructure(frame.args, frame.isPassive).forgetAnimacy
  //         }.toSet
  //       )
  //     )
  //   )
  // }

  // generating input files for feature generation

  @JsonCodec case class MLMFeatureGenInput(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbs: Map[String, VerbMLMInput]
  )

  @JsonCodec case class VerbMLMInput(
    verbIndices: Set[Int],
    argSpans: Set[ESpan]
  )
  object VerbMLMInput {
    import cats.kernel.CommutativeMonoid
    implicit val verbMlmInputCommutativeMonoid = new CommutativeMonoid[VerbMLMInput] {
      override def empty: VerbMLMInput = VerbMLMInput(Set(), Set())
      override def combine(x: VerbMLMInput, y: VerbMLMInput): VerbMLMInput = {
        VerbMLMInput(
          x.verbIndices ++ y.verbIndices,
          x.argSpans ++ y.argSpans,
        )
      }
    }
  }

  lazy val mlmFeatureGenInputs = {
    (verbs.data, args.data, sentences.data,
     verbIdToType.data, widen(argSemanticHeadIndices.data),
     fullArgSpanSets.data,
     verbArgSets.data
    ).mapN { (verbs, args, sentences, verbIdToType, argSemanticHeadIndices, argSpans, verbArgSets) =>
      val verbsBySentenceId = verbs.values.reduce(_ union _).groupBy(_.sentenceId)
      // val argsBySentenceId = args.values.reduce(_ union _).groupBy(_.verbId.sentenceId)
      val sentenceIds = verbsBySentenceId.keySet // union argsBySentenceId.keySet
      Stream.emits[IO, String](sentenceIds.toList).map { sid =>
        MLMFeatureGenInput(
          sentenceId = sid,
          sentenceTokens = sentences(sid),
          verbs = verbsBySentenceId(sid).unorderedFoldMap { verbId =>
            val verbType = verbIdToType(verbId)
            val args = verbArgSets(verbType)(verbId).map(ArgumentId(verbId, _))
            def getHeadSpans(argId: ArgumentId[Arg]) = {
              val indices = argSemanticHeadIndices(verbType)(argId)
              indices.map(index => ESpan(index, index + 1))
            }
            val mlmInput = VerbMLMInput(
              verbIndices = Set(verbId.verbIndex),
              argSpans = args.unorderedFoldMap(argId =>
                argSpans(verbType)(argId) ++ getHeadSpans(argId)
              )
            )
            Map(getVerbLemma(verbType) -> mlmInput)
          }
        )
      }
    }
  }

  def getMLMFeatureInputOutPath(split: String) = outDir
    .map(_.resolve(s"mlm-inputs")).flatTap(createDir)
    .map(_.resolve(s"$split.jsonl.gz"))

  lazy val writeMLMInputs = RunData.strings.zip(mlmFeatureGenInputs).flatMap { case (split, inputStream) =>
    getMLMFeatureInputOutPath(split) >>= (outPath =>
      IO(Files.exists(outPath)).ifM(
        Log.info(s"MLM feature inputs already found at $outPath."),
        Log.infoBranch(s"Logging MLM feature inputs to $outPath")(
          FileUtil.writeJsonLinesStreaming(outPath, io.circe.Printer.noSpaces)(inputStream)
        )
      )
    )
  }.all.void

  @JsonCodec case class PropBankQGInput(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbEntries: Map[Int, PropBankQGVerbInput]
  )
  @JsonCodec case class PropBankQGVerbInput(
    verbIndex: Int,
    argumentSpans: Set[ESpan]
  )

  def qgInputs: RunData[Stream[IO, PropBankQGInput]] =
    (sentences.data, verbArgSets.data, fullArgSpanSets.data).mapN { case (sents, allVerbs, allSpans) =>
      Stream.emits[IO, (String, List[(VerbType, (VerbId, Set[Arg]))])](
        allVerbs.toList.flatMap(p => p._2.toList.map(p._1 -> _)).groupBy(_._2._1.sentenceId).toList
      ).map { case (sid, verbs) =>
          val verbInputs =  verbs.groupBy(_._2._1).map { case (verbId, verbInstances) =>
            val spans = verbInstances.foldMap(t =>
              t._2._2.unorderedFoldMap(arg =>
                allSpans(t._1)(ArgumentId(verbId, arg))
              )
            )
            verbId.verbIndex -> PropBankQGVerbInput(verbId.verbIndex, spans)
          }
          PropBankQGInput(sid, sents.value(sid), verbInputs)
      }
    }

  def getQGInputOutPath(split: String) = outDir
    .map(_.resolve(s"qg-inputs")).flatTap(createDir)
    .map(_.resolve(s"$split.jsonl.gz"))

  def writeQGInputs = RunData.strings.zip(qgInputs).flatMap { case (split, inputStream) =>
    getQGInputOutPath(split) >>= (outPath =>
      IO(Files.exists(outPath)).ifM(
        Log.info(s"QG Inputs already found at $outPath."),
        Log.infoBranch(s"Logging QG inputs to $outPath")(
          FileUtil.writeJsonLinesStreaming(outPath, io.circe.Printer.noSpaces)(inputStream)
        )
      )
    )
  }.all.void

  def setup = writeMLMInputs >> writeQGInputs
}
