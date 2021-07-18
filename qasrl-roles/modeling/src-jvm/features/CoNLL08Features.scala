package qasrl.roles.modeling.features

import qasrl.roles.modeling._
import qasrl.roles.modeling.util.VectorFileUtil

import java.nio.file._

import jjm.LowerCaseString
import jjm.NonMergingMap
import jjm.ling.ESpan
import jjm.ling.PredArgStructure
import jjm.ling.SyntaxTree
import jjm.ling.Text
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.io.Cell
import jjm.io.FileCached
import jjm.io.FileUtil
import jjm.implicits._

import cats.Id
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

class CoNLL08Features(
  mode: RunMode,
  assumeGoldVerbSense: Boolean)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]
) extends PropBankFeatures[Int](mode, assumeGoldVerbSense)(cs, Log) {

  import jjm.datasets.PropBankPredicate
  import jjm.datasets.conll08._

  override val rootDir = Paths.get("frame-induction/conll08")

  val dataService = new CoNLL08FileSystemService(Paths.get("data/conll08st"))

  val splits = RunData[CoNLL08Split](
    train = CoNLL08Split.Train,
    dev = CoNLL08Split.Dev,
    test = CoNLL08Split.TestWSJ
  )

  def keepOnlyVerbalPredicates(sentence: CoNLL08Sentence) = {
    import jjm.ling.en.PTBPosTags
    sentence.copy(
      predicateArgumentStructures = sentence.predicateArgumentStructures
        .filter { pas =>
          val predPos = sentence.tokens(pas.predicateIndex).pos
          PTBPosTags.verbs.contains(predPos)
        }
    )
  }

  // unused
  // def keepOnlyCommonPredicates(sentence: CoNLL08Sentence, lemmasToKeep: Set[String]) = {
  //   sentence.copy(
  //     predicateArgumentStructures = sentence.predicateArgumentStructures
  //       .filter { pas =>
  //         lemmasToKeep.contains(pas.predicate.lemma)
  //       }
  //   )
  // }

  // unused
  // def keepOnlyCommonPredicatesInDataset(ds: NonMergingMap[String, CoNLL08Sentence]) = {
  //   val predLemmaCounts = ds.value.unorderedFoldMap(_.predicateArgumentStructures.map(_.predicate.lemma).counts)
  //   val lemmasToKeep = predLemmaCounts.filter(_._2 > 20).keySet
  //   NonMergingMap(ds.value.map { case (sid, sent) => sid -> keepOnlyCommonPredicates(sent, lemmasToKeep) })
  // }

  // def dropIrrelevantRoles(sentence: CoNLL08Sentence) = {
  //   CoNLL08Sentence.predicateArgumentStructures
  //     .composeTraversal(Optics.each)
  //     .composeLens(PredArgStructure.arguments)
  //     .modify(
  //       _.filterNot(arg => PropBankRoleLabel.roleLabelIsIrrelevant(arg._1))
  //     )(sentence)
  // }

  def dropCorefAndResumptiveRoles(sentence: CoNLL08Sentence) = {
    import jjm.ling.en.PTBPosTags
    sentence.copy(
      predicateArgumentStructures = sentence.predicateArgumentStructures
        .map { pas =>
          // NOTE: the commented bits below print out edge case phenomena (assuming verbal predicates).

          // there are a few cases of this remaning for verbal predicates.
          // AFAICT they are annotation mistakes in gold.
          // if(pas.arguments.map(_._2).contains(pas.predicate.index)) {
          //   System.err.println("Argument coincides with predicate: " + pas.toString)
          //   System.err.println("Argument coincides with predicate: " + jjm.ling.Text.render(sentence.tokens))
          // }

          // Normally pred/arg indices coinciding, or SU roles, are used for non-verbal predicates.
          // there's a (less) small number of these. I assume we should keep them..
          // But I also kinda think they're mistakes. Unclear.
          // if(pas.arguments.map(_._1).contains("SU")) {
          //   System.err.println("SU argument: " + pas.toString)
          //   System.err.println("SU argument: " + jjm.ling.Text.render(sentence.tokens))
          // }

          // There is a not-huge number of these. Unclear how AM-TM is different from AM-TMP.
          // It only appears twice in the trainins set so I think it's a mistake.
          // and I can't totally figure out what AA is about.
          // if(pas.arguments.map(_._1).contains("AA") || pas.arguments.map(_._1).contains("AM-TM")) {
          //   System.err.println("Weird argument: " + pas.toString)
          //   System.err.println("Weird argument: " + jjm.ling.Text.render(sentence.tokens))
          // }

          // potential processing to bring the number of gold labels down to 22.
          // doesn't change the results for the baseline.
          // pas.copy(
          //   arguments = pas.arguments.filterNot(
          //     l => l._1.startsWith("R-") || l._1.startsWith("C-") || l._1 == "SU"
          //   ).map { case (label, index) =>
          //       (if(label == "AM-TM") "AM-TMP" else label) -> index
          //   }
          // )

          pas.copy(
            arguments = pas.arguments.filterNot(
              l => l._1.startsWith("R-") || l._1.startsWith("C-")
            )
          )
        }
    )
  }

  // TODO add unfiltered dataset separately for feature generation?

  val dataset: RunDataCell[NonMergingMap[String, CoNLL08Sentence]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    splits.flatMap(split =>
      dataService
        .streamSentences[IO](split)
        .map(s =>
          NonMergingMap(
            Map(s.id.toString -> dropCorefAndResumptiveRoles(keepOnlyVerbalPredicates(s)))
          )
        )
        .infoCompile(s"Reading CoNLL 2008 data ($split)")(_.foldMonoid)
    ).toCell
  }

  override val sentences: RunDataCell[NonMergingMap[String, Vector[String]]] =
    dataset.data.map(sents =>
      NonMergingMap(
        sents.value.map { case (sid, sent) =>
          sid -> sent.tokens.map(_.token)
        }
      )
    ).toCell

  val predArgStructures: CachedVerbFeats[PredArgStructure[PropBankPredicate, Int]] = {
    dataset.data.map(
      _.value.toList.foldMap { case (sid, sentence) =>
        sentence.predicateArgumentStructures.foldMap { pas =>
          val verbType = if(assumeGoldVerbSense) {
            s"${pas.predicate.lemma}.${pas.predicate.sense}"
          } else pas.predicate.lemma
          val verbId = VerbId(sid, pas.predicateIndex)
          Map(verbType -> NonMergingMap(Map(verbId -> pas)))
        }
      }
    ).toCell
  }

  override val verbArgSets: RunDataCell[Map[String, Map[VerbId, Set[Int]]]] =
    predArgStructures.data.map(allPAStructures =>
      allPAStructures.mapVals { verbs =>
        verbs.value.mapVals { pas =>
          pas.arguments.map(_._2).toSet
        }
      }
    ).toCell

  override lazy val verbSenseLabels =
    cacheVerbFeats("CoNLL 2008 verb sense labels")(
      mapVerbFeats(predArgStructures.data) { pas =>
        val predicate = pas.predicate
        s"${predicate.lemma}.${predicate.sense}"
      }
    )

  override lazy val argRoleLabels: CachedArgFeats[PropBankRoleLabel] =
    predArgStructures.data.map(predArgStructures =>
      predArgStructures.mapVals { verbs =>
        verbs.value.toList.foldMap { case (verbId, pas) =>
          pas.arguments.foldMap { case (roleLabel, index) =>
            val argId = ArgumentId(verbId, index)
            val label = PropBankRoleLabel(pas.predicate.sense, roleLabel)
            NonMergingMap(argId -> label)
          }
        }
      }
    ).toCell


  // other things that could be used as argument keys

  // val position = {
  //   if(argIndex < verbIndex) "left"
  //   else if(argIndex > verbIndex) "right"
  //   else "same" // don't generally expect this to happen
  // }
  // val preposition = {
  //   val argToken = sentence.tokens(argIndex)
  //   if(argToken.pos == "IN") argToken.lemma.toLowerCase
  //   else ""
  // }

  val argSyntacticFunctions: CachedArgFeats[String] = {
    cacheArgFeats("CoNLL 2008 argument syntactic functions")(
      dataset.data.map { data =>
        (verbType: String) => (argId: ArgumentId[Int]) => {
          val sentence = data(argId.verbId.sentenceId)
          val dependencies = sentence.childToParentDependencies
          dependencies(argId.argument)._1
        }
      }
    )
  }

  val argSyntacticFunctionsConverted: CachedArgFeats[String] = {
    cacheArgFeats("CoNLL 2008 converted argument syntactic functions")(
      dataset.data.map { data =>
        (verbType: String) => (argId: ArgumentId[Int]) => {
          val sentence = data(argId.verbId.sentenceId)
          val dependencies = sentence.childToParentDependencies
          val verbIndex = argId.verbId.verbIndex
          val verbChildDepLabels = dependencies.filter(_._2 == verbIndex).map(_._1).toSet
          val isPassive = verbChildDepLabels.contains("LGS") || (
            sentence.tokens(verbIndex).pos == "VBN" &&
              dependencies(verbIndex)._1 == "VC" &&
              sentence.tokens(dependencies(verbIndex)._2).lemma == "be"
          )
          // val objIndices = dependencies.filter(_._2 == verbIndex).filter(_._1 == "OBJ").map(_._2).toSet

          dependencies(argId.argument)._1 match {
            // just handle passives reasonably
            case "SBJ" if isPassive => "OBJ"
            case "LGS" => "SBJ"
            // I tried these but they didn't help / reduced purity too much
            // case "OBJ" if objIndices.exists(_ < argIndex) => "OBJ2"
            // case "OBJ" if isPassive => "OBJ2"
            // case "LGS" => "SBJ-trans"
            // case "SBJ" if objIndices.isEmpty => "SBJ-intrans"
            // case "SBJ" if objIndices.nonEmpty => "SBJ-trans"
            case x => x
          }
        }
      }
    )
  }

  import scala.annotation.tailrec
  @tailrec final def getDependencyPathToRoot(
    dependencies: Vector[(String, Int)],
    index: Int,
    acc: List[(String, Int)] = Nil
  ): List[(String, Int)] = {
    if(index == -1) acc
    else {
      val dep = dependencies(index)
      getDependencyPathToRoot(dependencies, dep._2, dep :: acc)
    }
  }

  val argDependencyPaths: CachedArgFeats[String] = {
    cacheArgFeats("CoNLL 2008 predicate-argument dependency paths")(
      dataset.data.map { data =>
        (verbType: String) => (argId: ArgumentId[Int]) => {
          val sentence = data(argId.verbId.sentenceId)
          val dependencies = sentence.childToParentDependencies
          val verbIndex = argId.verbId.verbIndex
          val argIndex = argId.argument

          val predPathToRoot = getDependencyPathToRoot(dependencies, verbIndex)
          val argPathToRoot = getDependencyPathToRoot(dependencies, argIndex)
          val predPathToLCA = predPathToRoot.takeWhile { case (_, i) =>
            i != argIndex && !argPathToRoot.exists(_._2 == i)
          }
          val argPathToLCA = argPathToRoot.takeWhile { case (_, i) =>
            i != argIndex && !predPathToRoot.exists(_._2 == i)
          }
          val pathStr = predPathToLCA.mkString("->") + "*" + argPathToLCA.reverse.mkString("<-")
          pathStr
        }
      }
    )
  }

  override val argSemanticHeadIndices: CachedArgFeats[Set[Int]] = {
    cacheArgFeats("Argument semantic head indices")(
      dataset.data.map { data =>
        (verbType: String) => (argId: ArgumentId[Int]) => {
          val sentence = data(argId.verbId.sentenceId)
          val dependencies = sentence.childToParentDependencies
          val argIndex = argId.argument

          val validPrepPOS = Set("IN", "TO", "RB")
          // together these other POS tags should cover everything interesting. they can stay heads
          // val invalidPrepPOS = Set("VBG", "VBN", "VBD", "RP", "JJ")

          // if(dependencies.contains("PMOD" -> argIndex)) {
          //   if(!(validPrepPOS ++ invalidPrepPOS).contains(sentence.tokens(argIndex).pos)) {
          //     System.err.println(jjm.ling.Text.render(sentence.tokens))
          //     System.err.println(s"$argIndex: ${sentence.tokens(argIndex)}")
          //     val pmodIndex = dependencies.indexOf("PMOD" -> argIndex)
          //     System.err.println(s"$pmodIndex: ${sentence.tokens(pmodIndex)}")
          //   }
          // }

          val validToPOS = Set[String]("TO")
          // TO is the only pos that appears with IM dependents
          // val invalidToPOS = Set[String]()

          // if(dependencies.contains("IM" -> argIndex)) {
          //   if(!(validToPOS ++ invalidToPOS).contains(sentence.tokens(argIndex).pos)) {
          //     System.err.println(jjm.ling.Text.render(sentence.tokens))
          //     System.err.println(s"$argIndex: ${sentence.tokens(argIndex)}")
          //     val imIndex = dependencies.indexOf("IM" -> argIndex)
          //     System.err.println(s"$imIndex: ${sentence.tokens(imIndex)}")
          //   }
          // }

          // XXX currently collected MLM distributions are for index i, not argIndex. need to update
          val head = Option(dependencies.indexOf("PMOD" -> argIndex))
            .filter(_ >= 0)
            .filter(i => validPrepPOS.contains(sentence.tokens(argIndex).pos))
            .orElse(
              Option(dependencies.indexOf("IM" -> argIndex))
                .filter(_ >= 0)
                .filter(i => validToPOS.contains(sentence.tokens(argIndex).pos))
            ).getOrElse(argIndex)

          Set(head)
        }
      }
    )
  }

  lazy val argConstituentTypeDists: CachedArgFeats[Map[String, Double]] = {
    ptb2ArgConstituentTypes
  }

  lazy val argConstituentTypeDistsConverted: CachedArgFeats[Map[String, Double]] = {
    ptb2ArgConstituentTypesConverted
  }

  override lazy val argSpans: CachedArgFeats[Map[ESpan, Double]] = {
    origPropBankArgSpans
  }

  // TODO: decide what (potentially different) arrangement of spans to use for MLM features.
  // spansWithoutLeadingPrep

  override lazy val fullArgSpanSets: CachedArgFeats[Set[ESpan]] =
    cacheArgFeats("arg spans extended")(
      (origPropBankArgSpans.data, argSpansWithoutLeadingPrep.data, argLeadingPreps.data).mapN {
        (orig, modified, preps) => (verbType: String) => (argId: ArgumentId[Int]) => {
          orig(verbType)(argId).keySet |+|
            modified(verbType)(argId).keySet |+|
            preps(verbType)(argId).keySet.map(i => ESpan(i, i + 1))
        }
      }
    )

  override lazy val argSpansForMLM: CachedArgFeats[Map[ESpan, Double]] = {
    argSpansWithoutLeadingPrep
    // cacheArgFeats("arg spans for MLM")(
    //   (dataset.data, origPropBankArgSpans.data).mapN { (data, getPBSpans) =>
    //     (verbType: String) => (argId: ArgumentId[Int]) => {
    //       val pbSpans = getPBSpans(verbType)(argId)
    //       val sentence = data.value(argId.verbId.sentenceId)
    //       pbSpans.toList.flatMap { case (span, prob) =>
    //         val moddedSpan = sentence.tokens(span.begin).pos match {
    //           case "IN" | "RB" => None
    //           case "TO" => None
    //           case _ => Some(span)
    //         }
    //         moddedSpan.map(_ -> prob)
    //       }.toMap
    //     }
    //   }
    // )
  }

  import jjm.datasets.conll05
  import jjm.datasets.ptb2
  import jjm.datasets.propbank1
  import jjm.datasets.propbank3

  val conll05Path = Paths.get("data/conll05st-release")

  val conll05Splits = {
    import conll05.CoNLL05Split._
    RunData(
      train = IO.pure(Train),
      dev = IO.pure(Dev),
      test = IO.pure(TestWSJ)
    )
  }

  lazy val conll05Sentences: RunDataCell[NonMergingMap[String, conll05.CoNLL05Sentence]] = {
    import conll05._
    import scala.concurrent.ExecutionContext.Implicits.global
    val service = new CoNLL05FileSystemService(conll05Path)
    conll05Splits.flatMap(split =>
      service.streamSentences[IO](split)
        .map(s => s.id.toString -> s)
        .infoCompile("Reading CoNLL 2005 sentences")(
          _.toList.map(l => NonMergingMap(l.toMap))
        )
    ).toCell
  }

  val ptb2Path = Paths.get("data/treebank2")

  lazy val ptb2Sentences: Cell[NonMergingMap[ptb2.PTB2SentenceId, ptb2.PTB2Sentence]] =
    Cell {
      val service = new ptb2.PTB2FileSystemService(ptb2Path)
      import scala.concurrent.ExecutionContext.Implicits.global
      service.streamFiles[IO]
        .evalMap(f => Log.trace(f.path.toString).as(f))
        .flatMap(f => Stream.emits[IO, ptb2.PTB2Sentence](f.sentences))
        .map(s => NonMergingMap(s.id -> s))
        .infoCompile("Reading Penn Treebank sentences")(_.foldMonoid)
    }

  lazy val ptb2TextIndex =
    Cell {
      ptb2Sentences.get.map(
        _.value.toList.foldMap { case (sid, sentence) =>
          Map(Text.render(sentence.tokens.filter(_.pos != "-NONE-")) -> Set(sid))
        }
      )
    }

  lazy val conllToPTB2SentenceAlignments: Cell[NonMergingMap[String, ptb2.PTB2SentenceId]] =
    Cell {
      cacheDir.map(_.resolve("ptb-alignments.jsonl.gz")) >>= (cachePath =>
        FileCached.get[NonMergingMap[String, ptb2.PTB2SentenceId]](
          path = cachePath,
          read = path => FileUtil
            .readJsonLines[(String, ptb2.PTB2SentenceId)](path)
            .infoCompile("Reading alignment pairs")(_.toList.map(ls => NonMergingMap(ls.toMap))),
          write = (path, alignments) => FileUtil
            .writeJsonLines[(String, ptb2.PTB2SentenceId)](path)(alignments.value.toList))(
          for {
            ptbSentences <- ptb2Sentences.get
            // ptbIndex <- ptb2SentenceIndex.get
            ptbTextIndex <- ptb2TextIndex.get
            conllSentences <- dataset.data.all.map(_.combineAll)
            alignments <- conllSentences.value.toList.infoBarTraverse("Aligning CoNLL sentences") { case (sid, sentence) =>

              val collapsedTokens = sentence.tokens.zipWithIndex.foldLeft("") {
                case (str, (next, idx)) => {
                  if(sentence.paddingIndices.contains(idx)) str + next.token
                  else str + " " + next.token
                }
              }.trim.split(" ").toList
              val sentText = Text.render(collapsedTokens)

              val textMatches = ptbTextIndex.getOrElse(sentText, Set.empty[ptb2.PTB2SentenceId])

              val alignment = if(textMatches.size >= 1) {
                // if(textMatches.size > 1) {
                //   System.err.println("\n===== Multiple matches! =====")
                //   System.err.println(sentText)
                //   textMatches.foreach(System.err.println)
                // }
                sid -> textMatches.head
              } else {
                // val softMatches = sentence.tokens.foldMap(tok =>
                //   ptbIndex.get(tok.token.lowerCase).combineAll
                // ).toVector.sortBy(-_._2).take(5)
                // val topPath = softMatches.head._1

                System.err.println("\n===== NO TEXT MATCH! =====")
                System.err.println(sid)
                System.err.println(sentText)
                System.err.println(Text.render(sentence.tokens))
                // softMatches.foreach { case (path, count) =>
                //   System.err.println(s"$count: $path")
                //   val ptbSentence = ptbSentences(path)
                //   System.err.println(Text.render(ptbSentence.tokens.filter(_.pos != "-NONE-")))
                //   System.err.println(Text.render(ptbSentence.tokens.map(t => s"${t.token}#${t.pos}")))
                // }
                System.err.println()

                ???
                // sid -> topPath
              }

              Log.trace(f"${alignment._1}%-15s -> ${alignment._2}%s") >> IO.pure(alignment)
            }
          } yield NonMergingMap(alignments.toMap)
        )
      )
    }

  lazy val ptb2ArgConstituentTypes: CachedArgFeats[Map[String, Double]] = {
    // TODO decide what to do about including the VerbType, for sense-agnostic and sense-specific ones to share file cache...
    fileCacheArgFeats("ptb-constituent-types", log = true)(
      dataset.data.map { data =>
        for {
          ptb2Sentences <- ptb2Sentences.get
          propbank1Sentences <- propbank1Sentences.get
          conllToPTB2SentenceAlignments <- conllToPTB2SentenceAlignments.get
        } yield (verbType: String) => (argId: ArgumentId[Int]) => {
          val sentenceId = argId.verbId.sentenceId
          val verbIndex = argId.verbId.verbIndex
          val argIndex = argId.argument
          val sentence = data(sentenceId)

          val ptbSid = conllToPTB2SentenceAlignments(sentenceId)
          val ptbSentence = ptb2Sentences(ptbSid)
          val ptbTree = ptbSentence.syntaxTree

          def error = {
            System.err.println(sentenceId)
            System.err.println(ptbSid)
            System.err.println(Text.render(sentence.tokens))
            ???
          }

          propbank1Sentences.value.get(ptbSid).fold[Map[String, Double]](
            error // Map.empty[String, Double]
          ) { propBankSentence =>
            val recalculatedPAStructures = reindexPAStructures(
              ptbTree, propBankSentence, sentence
            )

            val pbArguments = recalculatedPAStructures
              .find(_.predicateIndex == verbIndex)
              .foldMap(_.arguments) // no spans for cases where predicate was not matched
              .filter(_._2._2.exists(_.contains(argIndex))) // take spans for _any_ args that match, incl. the case when none do. in practice I think it's one or none
              .foldMap(_._2._1)

            import propbank1.PropBankArgument.TracedNonterminal
            val constituentTypes: List[String] = pbArguments.toList
              .flatMap(_.getTopConstituentLabels(ptbTree))

            // TODO postprocess. perhaps remove -SBJ, etc.

            val prob = 1.0 / constituentTypes.size

            constituentTypes.foldMap(x => Map(x -> prob))
          }
        }
      }.flatMap(x => x)
    )
  }

  private[this] def stripNonterminalInfo(symbol: String) = symbol.takeWhile(_ != '-')

  // this is not currently used. doesn't seem to help
  private[this] def convertNonterminal(curSymbol: String, pos: String) = (curSymbol, pos) match {
    case (x, "TO") => x + "[to]"
    case (x, "VB") => x + "[b]"
    case (x, "VBZ" | "VBD" | "VBP") => x + "[dcl]"
    case (x, "VBN") => x + "[pt]"
    case (x, "VBG") => x + "[ng]"
    case (x, _) => x
  }

  // could just map over the non-coverted ones with stripNonterminalInfo
  // since convertNonterminal is no longer used
  lazy val ptb2ArgConstituentTypesConverted: CachedArgFeats[Map[String, Double]] = {
    cacheArgFeats("converted PTB constituent types")(
      (dataset.data, ptb2ArgConstituentTypes.data).mapN { (data, constituentTypes) =>
        (verbType: String) => (argId: ArgumentId[Int]) => {
          val sentenceId = argId.verbId.sentenceId
          val sentence = data(sentenceId)
          val headPos = sentence.tokens(argId.argument).pos
          constituentTypes(verbType)(argId).toList.foldMap { case (symbol, prob) =>
            // val newSymb = convertNonterminal(stripNonterminalInfo(symbol), headPos)
            val newSymb = stripNonterminalInfo(symbol)
            Map(newSymb -> prob)
          }
        }
      }
    )
  }

  override lazy val argPrepositions: CachedArgFeats[Option[LowerCaseString]] = {
    cacheArgFeats("prepositions")(
      dataset.data.map { data =>
        (verbType: String) => (argId: ArgumentId[Int]) => {
          val sentenceId = argId.verbId.sentenceId
          val sentence = data(sentenceId)
          val token = sentence.tokens(argId.argument)
          if(token.pos == "IN" || token.pos == "TO") Some(token.token.lowerCase) else None
        }
      }
    )
  }

  import propbank1.PropBankArgument

  val propbank1Path = Paths.get("data/propbank_1")

  lazy val propbank1Sentences: Cell[NonMergingMap[ptb2.PTB2SentenceId, propbank1.PropBank1Sentence]] =
    Cell {
      val service = new propbank1.PropBank1FileSystemService(propbank1Path)
      import scala.concurrent.ExecutionContext.Implicits.global
      service.streamSentences[IO]
        .evalMap(s => Log.trace(s.id.toString).as(NonMergingMap(s.id -> s)))
        .infoCompile("Reading PropBank sentences")(_.foldMonoid)
    }

  def reindexPAStructures(
    ptbTree: SyntaxTree[ptb2.PTB2Token],
    propBankSentence: propbank1.PropBank1Sentence,
    conll08Sentence: CoNLL08Sentence
  ): List[PredArgStructure[PropBankPredicate, (Set[PropBankArgument], Set[ESpan])]] = {

    val emptyElementIndices = ptbTree.toList.filter(_.pos == "-NONE-").map(_.index)
    // counts of added forms from splitting for each index in the unsplit case
    val numExtraSplitFormsByIndex = conll08Sentence.paddingIndices.unorderedFoldMap { index =>
      // subtract 1 for every padding index incl. or before this one.
      val originalIndex = index - conll08Sentence.paddingIndices.count(_ <= index)
      Map(originalIndex -> 1)
    }
    val numSplitFormsByIndex = (0 until (conll08Sentence.tokens.size - conll08Sentence.paddingIndices.size))
      .map(i => numExtraSplitFormsByIndex.getOrElse(i, 0))
      .toList // turn map into a list of extra split form counts

    // add a 0 do a cumulative sum to know the total offset before a token position
    val preTokenSplitFormOffsets = numSplitFormsByIndex.scanLeft(0)(_ + _)

    def reindexPredicate(pi: Int): Int = (pi: Id[Int])
      .map(pi => pi - emptyElementIndices.count(_ < pi))
      .map(pi => pi + preTokenSplitFormOffsets(pi))

    // collapse together multiple predicates at the same index.
    // this is what was done in the conversion to conll format anyway.
    val reindexedSpanSetsWithPredicateIndex = propBankSentence.predArgStructures.foldMap { pas =>

      val predicateIndex = reindexPredicate(pas.predicateIndex)

      Map(
        predicateIndex -> pas.arguments.map { case (_, arg) =>
          arg -> arg.allBranches
            .map(branch => ptbTree.getSubtree(branch).toSpan)
            .map { span =>
              // reindex to remove gaps
              // TODO test this to be sure it works..
              val newBegin = span.begin - emptyElementIndices.count(_ < span.begin)
              val newEnd = span.end - emptyElementIndices.count(_ < span.end)
              ESpan(newBegin, newEnd)
            }
            .map { span =>
              // reindex to account for split tokens in conll 08
              // TODO test this to be sure it works...
              val newBegin = span.begin + preTokenSplitFormOffsets(span.begin)
              val newEnd = span.end + preTokenSplitFormOffsets(span.end)
              ESpan(newBegin, newEnd)
            }
            .filter(s => s.end > s.begin) // remove empty elements
            .toSet
          }.toSet
      )
    }

    conll08Sentence.predicateArgumentStructures.flatMap { pas =>
      if(reindexedSpanSetsWithPredicateIndex.get(pas.predicateIndex).isEmpty) {
        System.err.println("Can't find predicate!!")
        System.err.println(propBankSentence.id)
        System.err.println(Text.render(conll08Sentence.tokens))
        System.err.println(ptbTree.toList.map(t => s"${t.token} (${t.index})").mkString(", "))
        System.err.println(pas)
        None
      } else Some {
        val matchedArgs = reindexedSpanSetsWithPredicateIndex(pas.predicateIndex)
        val newArgs = pas.arguments.map { case (label, index) =>
          val thisPredicateSpanSets = matchedArgs.filter(_._2.exists(_.contains(index)))
          var hadToSwitchPredicates = false
          val spanSets = {
            if(thisPredicateSpanSets.isEmpty) {
              System.err.println("Missing argument for predicate!!")
              hadToSwitchPredicates = true
              reindexedSpanSetsWithPredicateIndex.values
                .flatMap(_.find(_._2.exists(_.contains(index))))
            } else thisPredicateSpanSets
          }
          // val spanSets: Set[Set[ESpan]] = if(trialSpanSets.isEmpty) {
          //   reindexedSpanSetsWithPredicateIndex.values.find(
          //     _.exists(_.contains(index))
          //   ).getOrElse(Set.empty[Set[ESpan]])
          // } else trialSpanSets
          val renderedNewSpanSets = spanSets.map(ss => ss._2.toList.sorted.map(Text.renderSpan(conll08Sentence.tokens, _)).mkString(" / "))
          val pbPas = propBankSentence.predArgStructures
            .filter(pbPas => reindexPredicate(pbPas.predicateIndex) == pas.predicateIndex)
          val renderedOrigSpanSets = pbPas.flatMap(_.arguments).map(
            _._2.allBranches
              .map(ptbTree.getSubtree)
              .map(_.toSpan)
              .sorted
              .map(Text.renderSpan(ptbTree.toList.map(t => if(t.pos == "-NONE-") "###" else t.token), _))
              .filterNot(_.matches("###"))
              .map(
                _.replaceAll(" ### ", " ")
                  .replaceAll("### ", "")
                  .replaceAll(" ###", "")
                  .replaceAll("###", "")
                  .trim)
              .filter(_.nonEmpty)
              .mkString(" / ")
          )
          val newQuerySpanSets = renderedNewSpanSets.map(_.replaceAll(" ", ""))
          val origQuerySpanSets = renderedOrigSpanSets.map(_.replaceAll(" ", ""))
          if(
            (hadToSwitchPredicates || spanSets.size != 1 || newQuerySpanSets.exists(rnss => !origQuerySpanSets.contains(rnss)))
            // (spanSets.size != 1 || renderedNewSpanSets.exists(rnss => !renderedOrigSpanSets.contains(rnss)))
            //   && !renderedOrigSpanSets.exists(_.contains("U.S.based"))
          ) {
            // System.err.println(renderedNewSpanSets)
            // System.err.println(renderedOrigSpanSets)
            System.err.println(propBankSentence.id)
            System.err.println(Text.render(conll08Sentence.tokens))
            System.err.println(ptbTree.toList.map(t => s"${t.token} (${t.index})").mkString(", "))

            System.err.println(s"Empty element indices: " + emptyElementIndices.toList.sorted.mkString(", "))
            System.err.println(s"Num extra split forms: " + numExtraSplitFormsByIndex.toList.mkString(", "))
            System.err.println(s"Num split forms: " + numSplitFormsByIndex.toList.mkString(", "))
            System.err.println(s"Pre-token number of split forms:")
            System.err.println(
              ptbTree.toList.filterNot(_.pos == "-NONE-").zip(preTokenSplitFormOffsets).map { case (tok, num) =>
                f"${tok.token}%15s $num%3d"
              }.mkString
            )

            System.err.println(s"PropBank arguments for ${ptbTree.toList(pbPas.head.predicateIndex)}")
            System.err.println(pbPas.flatMap(_.arguments).mkString("\n"))
            renderedOrigSpanSets.foreach(System.err.println)
            System.err.println("All matched span sets:")
            System.err.println(matchedArgs.mkString("\n"))
            // propBankSentence.predArgStructures.sortBy(pbPas => scala.math.abs(pas.predicateIndex - pbPas.predicateIndex)).headOption.foreach { pbPas =>
            //   System.err.println(s"PropBank arguments for ${ptbTree.toList(pbPas.predicateIndex)}:")
            //   System.err.println(pbPas.arguments.mkString("\n"))
            //   System.err.println(
            //     pbPas.arguments
            //       .map(_._2.allBranches
            //              .map(ptbTree.getSubtree)
            //              .map(_.toSpan)
            //              .map(Text.renderSpan(ptbTree.toList.map(t => if(t.pos == "-NONE-") "###" else t.token), _))
            //              .filterNot(_.matches("###"))
            //              .map(
            //                _.replaceAll("### ", " ")
            //                  .replaceAll(" ###", " "))
            //             .mkString(" / "))
            //       .mkString("\n")
            //   )
            // }
            System.err.println(s"CoNLL 08 predicate: ${conll08Sentence.tokens(pas.predicateIndex)}")
            System.err.println(s"Matched spans for argument ${conll08Sentence.tokens(index).token} ($index)")
            System.err.println(spanSets)
            System.err.println(
              spanSets.map(spanSet =>
                spanSet._2.map(Text.renderSpan(conll08Sentence.tokens, _)).mkString(" / ")
              ).mkString("\n==========\n")
            )
            System.err.println("\n==========\n")
          }
          label -> (spanSets.map(_._1).toSet -> spanSets.toList.foldMap(_._2))
        }
        pas.copy(arguments = newArgs)
      }
    }
  }

  lazy val origPropBankArgSpans: CachedArgFeats[Map[ESpan, Double]] = {
    val verbTypeLabel = if(assumeGoldVerbSense) "by-sense" else "by-lemma"
    fileCacheArgFeats(s"aligned-propbank-spans-$verbTypeLabel", log = true)(
      (dataset.data, argRoleLabels.data).mapN { (data, argRoleLabels) =>
        for {
          ptb2Sentences <- ptb2Sentences.get
          propbank1Sentences <- propbank1Sentences.get
          conllToPTB2SentenceAlignments <- conllToPTB2SentenceAlignments.get
        } yield (verbType: String) => (argId: ArgumentId[Int]) => {
          val sentenceId = argId.verbId.sentenceId
          val verbIndex = argId.verbId.verbIndex
          val argIndex = argId.argument
          val sentence = data(sentenceId)
          val paddingIndices = sentence.paddingIndices

          val ptbSid = conllToPTB2SentenceAlignments(sentenceId)
          val ptbSentence = ptb2Sentences(ptbSid)
          propbank1Sentences.value.get(ptbSid).fold(Map.empty[ESpan, Double]) { propBankSentence =>
            val recalculatedPAStructures = reindexPAStructures(
              ptbSentence.syntaxTree, propBankSentence, sentence
            )//: List[PredArgStructure[PropBankPredicate, Set[ESpan]]]

            val spans = recalculatedPAStructures
              .find(_.predicateIndex == verbIndex)
              .foldMap(_.arguments) // no spans for cases where predicate was not matched
              .filter(_._2._2.exists(_.contains(argIndex))) // take spans for _any_ args that match, incl. the case when none do. in practice I think it's one or none
              .foldMap(_._2._2)

            spans.map(_ -> (1.0 / spans.size)).toMap
          }
        }
      }.flatMap(x => x)
    )
  }

  val validPrepPOS = Set("IN", "TO", "RB")

  // TODO: possibly handle more complex cases like "long before Sony entered the children's market"
  val argSpansWithoutLeadingPrep: CachedArgFeats[Map[ESpan, Double]] = {
    cacheArgFeats("spans without leading prep")(
      dataset.data.zip(origPropBankArgSpans.data).map { case (data, getPBSpans) =>
        (verbType: String) => (argId: ArgumentId[Int]) => {
          val pbSpans = getPBSpans(verbType)(argId)
          val sentence = data.value(argId.verbId.sentenceId)
          pbSpans.toList.foldMap { case (span, prob) =>
            if(validPrepPOS.contains(sentence.tokens(span.begin).pos) && span.length > 1) {
              Map(ESpan(span.begin + 1, span.end) -> prob)
            } else Map(span -> prob)
          }
        }
      }
    )
  }

  override lazy val argLeadingPreps: CachedArgFeats[Map[Int, Double]] = {
    cacheArgFeats("arg leading preps")(
      dataset.data.zip(origPropBankArgSpans.data).map { case (data, getPBSpans) =>
        (verbType: String) => (argId: ArgumentId[Int]) => {
          val pbSpans = getPBSpans(verbType)(argId)
          val sentence = data.value(argId.verbId.sentenceId)
          pbSpans.toList.foldMap { case (span, prob) =>
            if(validPrepPOS.contains(sentence.tokens(span.begin).pos)) {
              Map(span.begin -> prob)
            } else Map()
          }
        }
      }
    )
  }

  lazy val dependencyInferredArgSpans: CachedArgFeats[Map[ESpan, Double]] = {
    cacheArgFeats("CoNLL 2008 inferred arg spans", log = true)(
      dataset.data.zip(argRoleLabels.data).zip(argSemanticHeadIndices.data).map { case ((data, roleLabels), semanticHeads) =>
        (verbType: String) => (argId: ArgumentId[Int]) => {

          val argIndex = argId.argument
          val sid = argId.verbId.sentenceId
          val sentence = data(sid)
          val dependencies = sentence.childToParentDependencies
          val parentToChildren = dependencies.map(_._2).zipWithIndex
            .foldMap { case (parent, child) => Map(parent -> Set(child)) }

          @tailrec def closureSpan(fibers: Map[Int, Set[Int]], curSet: Set[Int]): ESpan = {
            val nextSet = curSet
              .unorderedFoldMap(i => fibers.get(i).combineAll)
              .union(curSet)
            if(curSet == nextSet) ESpan(curSet.min, curSet.max + 1)
            else closureSpan(fibers, nextSet)
          }
          val span = closureSpan(parentToChildren, Set(argId.argument))

          val otherArgIndices = sentence.predicateArgumentStructures
            .find(_.predicateIndex == argId.verbId.verbIndex).get
            .arguments.map(_._2).filter(_ != argId.argument).toSet
          val otherArgsString = otherArgIndices.toList.sorted
            .map(i => s"${sentence.tokens(i).token} ($i)").mkString(", ")
          val containsAnotherArg = otherArgIndices.exists(span.contains)

          val verbIndex = argId.verbId.verbIndex
          val immediateBlockedIndices = otherArgIndices + verbIndex

          def followVerbChain(i: Int): Set[Int] = {
            val (label, parent) = dependencies(i)
            if(parent == argIndex) Set(i) // cut off this path at the base
            else if(label == "VC") followVerbChain(parent) + i // keep going
            else Set(i) // we're done
          }

          def followDepChain(i: Int): Set[Int] = {
            val (label, parent) = dependencies(i)
            if(parent == argIndex) Set(i) // cut off this path at the base
            else if(parent == -1) Set(i) // we're done
            else followDepChain(parent) + i // keep going
          }

          // go up in the tree a bit, yeeah. this gets us to the "actual" head
          val blockedIndices = immediateBlockedIndices.unorderedFoldMap(followDepChain)

          val lowerBound = blockedIndices.filter(_ < argIndex).toList.maximumOption.getOrElse(-1)
          val upperBound = blockedIndices.filter(_ > argIndex).toList.minimumOption.getOrElse(sentence.tokens.size)

          @tailrec def cullObsoleteUnaryChains(fibers: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
            val obsoletes = fibers.toList.filter(_._2.isEmpty).map(_._1).toSet
            if(obsoletes.isEmpty) fibers
            else cullObsoleteUnaryChains(
              fibers.mapVals(_.filterNot(obsoletes.contains)) -- obsoletes
            )
          }

          val parentToAdmissibleChildren = parentToChildren.mapVals(_.filter(i => i > lowerBound && i < upperBound))
          val blockedSpan = closureSpan(parentToAdmissibleChildren, Set(argId.argument))

          val parentToAdmissibleChildren2 = cullObsoleteUnaryChains(
            parentToAdmissibleChildren
          )
          val blockedSpan2 = closureSpan(parentToAdmissibleChildren2, Set(argId.argument))

          if(!sid.startsWith("train") && containsAnotherArg) {
            System.err.println("\n" + sid)
            System.err.println(jjm.ling.Text.render(sentence.tokens))
            System.err.println(s"PREDICATE: ${sentence.tokens(verbIndex).token} ($verbIndex)")
            val roleLabel = roleLabels(verbType)(argId).role
            System.err.println(s"$roleLabel: ${sentence.tokens(argId.argument).token}")
            System.err.println(jjm.ling.Text.renderSpan(sentence.tokens, span))

            System.err.println(s"Other args: $otherArgsString")
            System.err.println(s"Contains another argument? " + (if(containsAnotherArg) "YES" else "NO"))
            System.err.println(jjm.ling.Text.renderSpan(sentence.tokens, blockedSpan))
            System.err.println(jjm.ling.Text.renderSpan(sentence.tokens, blockedSpan2))
          }

          val semHeads = semanticHeads(verbType)(argId)
          val headSpans = semHeads.map(semHead => ESpan(semHead, semHead + 1))

          Map(span -> 0.5) |+| headSpans.unorderedFoldMap(headSpan => Map(headSpan -> (0.25 / headSpans.size)))
        }
      }
    )
  }

  // TODO: remove VerbType from VerbFeats and ArgFeats? Maybe?

  val propbank3Path = Paths.get("data/propbank-release")

  lazy val propbank3Sentences: Cell[NonMergingMap[propbank3.PropBank3SentenceId, propbank3.PropBank3Sentence]] = Cell {
    val service = new propbank3.PropBank3FileSystemService(propbank3Path)
    import scala.concurrent.ExecutionContext.Implicits.global
    service.streamSentencesFromProps[IO]
      .evalMap(s => Log.trace(s.id.toString).as(NonMergingMap(s.id -> s)))
      .infoCompile("Reading PropBank sentences")(_.foldMonoid)
  }

  lazy val propbank3SentencesCoNLLStyle: Cell[NonMergingMap[propbank3.PropBank3SentenceId, propbank3.PropBank3SentenceCoNLLStyle]] = Cell {
    val service = new propbank3.PropBank3FileSystemService(propbank3Path)
    import scala.concurrent.ExecutionContext.Implicits.global
    service.streamSentencesFromCoNLLSkels[IO]
      .evalMap(s => Log.trace(s.id.toString).as(NonMergingMap(s.id -> s)))
      .infoCompile("Reading PropBank sentences")(_.foldMonoid)
  }

  def relabelPAStructures(
    ptbTree: SyntaxTree[ptb2.PTB2Token],
    propBankSentence: propbank3.PropBank3SentenceCoNLLStyle,
    conll08Sentence: CoNLL08Sentence
  ): Map[Int, PredArgStructure[PropBankPredicate, Int]] = {
    conll08Sentence.predicateArgumentStructures.map { pas =>
      val predIndex = pas.predicateIndex
      propBankSentence.predArgStructures.find(_.predicateIndex == predIndex) match {
        case None =>
          System.err.println("\nCan't find predicate!!")
          System.err.println(propBankSentence.id)
          System.err.println(Text.render(conll08Sentence.tokens))
          System.err.println(
            conll08Sentence.tokens.map(t => s"${t.token} (${t.index})").mkString(", ")
          )
          System.err.println(ptbTree.toList.map(t => s"${t.token} (${t.index})").mkString(", "))
          System.err.println(pas)
          System.err.println("-----")
          System.err.println(propBankSentence.predArgStructures.mkString("\n"))
          predIndex -> pas // just use original
        case Some(pbPas) =>
          predIndex -> pas.copy(
            predicate = pbPas.predicate,
            arguments = pas.arguments.map { case (origLabel, argIndex) =>
              val labelOpt = pbPas.arguments.find(_._2.contains(argIndex)).map(_._1)
              labelOpt match {
                case None =>
                  System.err.println("\nCan't find argument!! Index: " + argIndex)
                  System.err.println(propBankSentence.id)
                  System.err.println(Text.render(conll08Sentence.tokens))
                  System.err.println(
                    conll08Sentence.tokens.map(t => s"${t.token} (${t.index})").mkString(", ")
                  )
                  System.err.println(ptbTree.toList.map(t => s"${t.token} (${t.index})").mkString(", "))
                  System.err.println(pas)
                  System.err.println("-----")
                  System.err.println(propBankSentence.predArgStructures.mkString("\n"))
                  origLabel -> argIndex
                case Some(label) =>
                  label -> argIndex
              }
            }
          )
      }
    }.toMap
  }

  val filteredPropBank3Roles = Set(
    "LINK-SLC", "LINK-PRO", "LINK-PSV"
  )

  val pb3OmittedFilesPath = Paths.get("data/pb3-missing-from-ontonotes.txt")

  lazy val pb3OmittedFiles: IO[List[ptb2.PTB2FilePath]] =
    FileUtil.readString(pb3OmittedFilesPath)
      .map(_.split("\n").toList.map(ptb2.PTB2FilePath.fromString))
    // fs2.io.file.readAll[IO](pb3OmittedFilesPath

  lazy val propBank3PredArgStructures: CachedVerbFeats[PredArgStructure[PropBankPredicate, Int]] = {
    fileCacheVerbFeats("propbank3-gold-pas", log = true)(
      dataset.data.map { data =>
        for {
          omittedPaths <- pb3OmittedFiles
          ptb2Sentences <- ptb2Sentences.get
          propbank3Sentences <- propbank3SentencesCoNLLStyle.get
          conllToPTB2SentenceAlignments <- conllToPTB2SentenceAlignments.get
        } yield (verbType: String) => (verbId: VerbId) => {
          val sentenceId = verbId.sentenceId
          val verbIndex = verbId.verbIndex
          val sentence = data(sentenceId)
          val paddingIndices = sentence.paddingIndices

          val ptbSid = conllToPTB2SentenceAlignments(sentenceId)
          if(omittedPaths.contains(ptbSid.filePath)) {
            System.err.println(s"=== OMITTED: $ptbSid ===")
            sentence.predicateArgumentStructures.find(_.predicateIndex == verbIndex).get
          } else {
            val ptbSentence = ptb2Sentences(ptbSid)
            val pb3Sid = propbank3.PropBank3SentenceId.parse(
              s"nw/wsj/${ptbSid.filePath.pathSuffix}".replaceAll("\\.mrg$", ".parse"),
              ptbSid.sentenceNum
            )
            val pb3Sentence = propbank3Sentences.value.get(pb3Sid).get
            val newPASs = relabelPAStructures(ptbSentence.syntaxTree, pb3Sentence, sentence)
            if(!newPASs.contains(verbIndex)) {
              System.err.println("\nMISSING!!!!!!")
              System.err.println(ptbSid)
              System.err.println(pb3Sid)
              System.err.println(sentence.tokens.grouped(10).mkString("\n"))
              System.err.println(newPASs)
              System.err.println(verbIndex)
              sentence.predicateArgumentStructures.find(_.predicateIndex == verbIndex).get
            } else newPASs(verbIndex)
          }
        }
      }.flatMap(x => x)
    )
  }
}
