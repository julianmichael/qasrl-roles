package qasrl.roles.modeling

import qasrl.roles.clustering._
import qasrl.roles.modeling.features._

import cats.Order
import cats.data.NonEmptyVector
import cats.effect.IO
import cats.implicits._

import jjm.Vocab
import jjm.implicits._

import qasrl.labeling.QuestionTemplate

import freelog.EphemeralTreeLogger
import freelog.implicits._

sealed trait ClusteringModel
object ClusteringModel {
  def fromString(x: String): Option[ClusteringModel] = {
    if(x.startsWith("arg/")) {
      BaselineArgumentModel.fromString(x.drop("arg/".length))
        .orElse(FullArgumentModel.fromString(x.drop("arg/".length)))
    } else if(x.startsWith("verb/")) {
      BaselineVerbModel.fromString(x.drop("verb/".length))
        .orElse(FullVerbModel.fromString(x.drop("verb/".length)))
    } else if(x.startsWith("joint/")) {
      JointModel.fromString(x.drop("joint/".length))
    } else None
  }
}

object JointModel {
  def fromString(x: String): Option[JointModel] = {
    val specs = x.split(";").lift
    for {
      entCoeff <- specs(0).flatMap(x => scala.util.Try(x.toDouble).toOption)
      argModel <- specs(1).map(_.replaceAll(":", "/").drop("arg/".length)).flatMap(FullArgumentModel.fromString)
      verbModel <- specs(2).map(_.replaceAll(":", "/").drop("verb/".length)).flatMap(FullVerbModel.fromString)
    } yield new JointModel(argModel, verbModel, 10, entCoeff)
  }

  def toString(model: JointModel): String = {
    val argString = model.argModel.toString.replaceAll("/", ":")
    val verbString = model.verbModel.toString.replaceAll("/", ":")
    f"joint/${model.innerEntropyCoefficient}%.2f;$argString%s;$verbString%s"
  }
}
case class JointModel(
  argModel: FullArgumentModel,
  verbModel: FullVerbModel,
  numFlatInnerClusters: Int,
  innerEntropyCoefficient: Double
) extends ClusteringModel {

  override def toString: String = JointModel.toString(this)

  // type FlatAlg[Arg, RoleParam] = JointFlatClusteringAlgorithm[VerbId, ArgumentId[Arg], RoleParam]
  // type AgglomAlg[Arg, RoleParam] = JointAgglomerativeClusteringAlgorithm[VerbId, ArgumentId[Arg], RoleParam]
  // type AlgPair[Arg, RoleParam] = (FlatAlg[Arg, RoleParam], AgglomAlg[Arg, RoleParam])
  type ArgFlatAlg[Arg] = FlatClusteringAlgorithm { type Index = ArgumentId[Arg] }
  type ArgAgglomAlg[Arg] = AgglomerativeClusteringAlgorithm { type Index = ArgumentId[Arg] }
  type ArgAlgPair[Arg] = (ArgFlatAlg[Arg], ArgAgglomAlg[Arg])

  type VerbFlatAlg = FlatClusteringAlgorithm { type Index = VerbId }
  type VerbAgglomAlg = AgglomerativeClusteringAlgorithm { type Index = VerbId }
  type VerbAlgPair = (VerbFlatAlg, VerbAgglomAlg)

  type JointFlatAlg = CompositeFlatClusteringAlgorithm {
    type Index = VerbId
    // type ClusterParam = (_1.ClusterParam, _2.ClusterParam)
  }
  type JointAgglomAlg = CompositeAgglomerativeClusteringAlgorithm {
    type Index = VerbId
  }
  type JointAlgPair = (JointFlatAlg, JointAgglomAlg)

  def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit] = {
    features.verbArgSets.get.void >>
      argModel.lossTerms.keySet.toList.traverse(_.init(features)).void >>
      verbModel.lossTerms.keySet.toList.traverse(_.init(features)).void
  }

  def create[VerbType, Arg](
    features: Features[VerbType, Arg], verbType: VerbType
  ) = {
    val argTermVec = argModel.lossTerms.toVector
    val argLambdas = argModel.lossTerms.map(_._2)

    val verbTermVec = verbModel.lossTerms.toVector
    val verbLambdas = verbModel.lossTerms.map(_._2)

    for {
      verbArgSets <- features.verbArgSets.get.map(_.apply(verbType))
      verbArgNevs = verbArgSets.map { case (verbId, argSet) =>
        verbId -> argSet.iterator.map(arg => ArgumentId(verbId, arg)).toVector
      }
      (argFlatAlg, argAgglomAlg) <- argTermVec
      .traverse(_._1.create(features, verbType): IO[ArgAlgPair[Arg]])
      .map(_.unzip[ArgFlatAlg[Arg], ArgAgglomAlg[Arg]])
      .map { case (flatAlgs, agglomAlgs) =>
        val weightedFlatAlg = new WeightedFlatClusteringAlgorithm[ArgumentId[Arg]](flatAlgs.zip(argLambdas))
        val weightedAgglomAlg = new WeightedAgglomerativeClusteringAlgorithm[ArgumentId[Arg]](agglomAlgs.zip(argLambdas))
        (weightedFlatAlg, weightedAgglomAlg)
      }
      (verbFlatAlg, verbAgglomAlg) <- verbTermVec
      .traverse(_._1.create(features, verbType): IO[VerbAlgPair])
      .map(_.unzip[VerbFlatAlg, VerbAgglomAlg])
      .map { case (flatAlgs, agglomAlgs) =>
        val weightedFlatAlg = new WeightedFlatClusteringAlgorithm[VerbId](flatAlgs.zip(verbLambdas))
        val weightedAgglomAlg = new WeightedAgglomerativeClusteringAlgorithm[VerbId](agglomAlgs.zip(verbLambdas))
        (weightedFlatAlg, weightedAgglomAlg)
      }
      flatAlg = new CompositeFlatClusteringAlgorithm {
        val _1 = verbFlatAlg
        val _1Lambda = 1.0
        val _2 = new JointFlatClusteringAlgorithm[VerbId, ArgumentId[Arg], argFlatAlg.ClusterParam](
          innerAlgorithm = argFlatAlg,
          getSubInstances = verbArgNevs,
          numInnerClusters = numFlatInnerClusters,
          innerHardEMStoppingDelta = new HybridClustering().flatClusteringHardStoppingDelta
        )
        val _2Lambda = 1.0
      }
      agglomAlg = new CompositeAgglomerativeClusteringAlgorithm {
        val _1 = verbAgglomAlg
        val _1Lambda = 1.0
        val _2 = new JointAgglomerativeClusteringAlgorithm[VerbId, ArgumentId[Arg], argAgglomAlg.ClusterParam](
          innerAlgorithm = argAgglomAlg,
          getSubInstances = verbArgNevs,
          getLossPenalty = (clusterSizes: Vector[Int]) => {
            val total = clusterSizes.sum
            clusterSizes.foldMap(size => innerEntropyCoefficient * -size * scala.math.log(size.toDouble / total))
          }
        )
        val _2Lambda = 1.0
      }
    } yield (flatAlg, agglomAlg)
  }

  def getJointClusters[VerbType, Arg : Order](
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, VerbClusterModel[VerbType, Arg]]] = {
    for {
      _ <- Log.info("Initializing model features") // TODO maybe extend branching API to make this nice
      _ <- this.init(features)
      allVerbArgSets <- features.verbArgSets.get
      results <- allVerbArgSets.toList.infoBarTraverse("Clustering verbs") { case (verbType, verbs) =>
        val verbIds = verbs.keySet
        Log.info(features.renderVerbType(verbType)) >> {
          // TODO I don't think any of these should be empty
          NonEmptyVector.fromVector(verbIds.toVector).traverse { args =>
            this.create(features, verbType) >>= { case (flatAlgorithm, agglomAlgorithm) =>
              val setAgglomAlgorithm = new AgglomerativeSetClustering(agglomAlgorithm) // repetitive here, but whatever
              new HybridClustering().run(args, flatAlgorithm, agglomAlgorithm).flatMap { case (verbTree, param) =>
                IO { // wrapped this here to make sure logging works in the correct order inside
                  // if this is empty, that means there are no arguments at all.
                  // if this ever happens, I'll need to adjust my VerbClusterModel data type.
                  val argTreeOpt = NonEmptyVector.fromVector(param._2).map { argTrees =>
                    val argAlgorithm = agglomAlgorithm._2.innerAlgorithm
                    argAlgorithm.finishAgglomerativeClustering(argTrees)._1
                  }
                  verbType -> VerbClusterModel(verbType, Clustering[VerbId](Some(verbTree)), Clustering[ArgumentId[Arg]](argTreeOpt.map(_.map(Set(_)))))
                }
              }
            }
          }
        }
      }
    } yield results.flatten.toMap
  }
}

case class SideClusteringModel(
  modals: Boolean,
  negation: Boolean,
  adjuncts: Boolean,
  discourse: Boolean,
  syntf: Boolean,
  syntfConverted: Boolean,
  gold: Boolean,
  goldWithSense: Boolean
){
  def extractSideClusters[VerbType, Arg: Order](
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, Map[String, Set[ArgumentId[Arg]]]]] = for {
    args <- features.args.get
    sentences <- features.sentences.get.map(_.value)
    headIndices <- features.argSemanticHeadIndices.get
    syntacticFuncs <- features.argSyntacticFunctions.get
    syntacticFuncsConverted <- features.argSyntacticFunctionsConverted.get
    argSpans <- features.argSpans.get
    roleLabels <- features.getIfPropBank.traverse(_.argRoleLabels.get)
    results <- args.toList.infoBarTraverse("Constructing side clusters") { case (verbType, args) =>
      Log.trace(verbType.toString) >> IO {
        val heads = headIndices(verbType)
        val funcs = syntacticFuncs(verbType)
        val funcsConverted = syntacticFuncsConverted(verbType)
        val spans = argSpans(verbType)
        val roleLabelsOpt = roleLabels.map(_.apply(verbType.toString))
        val getSideClusterLabel = (argId: ArgumentId[Arg]) => {
          val sentence = sentences(argId.verbId.sentenceId)
          val modal = Option("MODAL")
            .filter(_ => modals)
            .filter { _ =>
              val theseSpans = spans(argId)
              theseSpans.size == 1 && SideClusteringModel.modals.contains(
                jjm.ling.Text.renderSpan(sentence, theseSpans.head._1).lowerCase
              )
            }

          val neg = Option("NEG")
            .filter(_ => negation)
            .filter { _ =>
              val theseSpans = spans(argId)
              theseSpans.size == 1 && jjm.ling.en.Inflections.negationWords.contains(
                jjm.ling.Text.renderSpan(sentence, theseSpans.head._1).lowerCase
              )
            }

          val disc = Some("DIS")
            .filter(_ => discourse)
            .filter { _ =>
              val theseSpans = spans(argId)
              theseSpans.size == 1 && SideClusteringModel.discourseExpressions.contains(
                jjm.ling.Text.renderSpan(sentence, theseSpans.head._1).lowerCase
              )
            }

          val adjunct = Some(funcs(argId))
            .filter(_ => adjuncts)
            .filter(SideClusteringModel.adjunctFuncs.contains)

          val syntfunc = Some(funcs(argId))
            .filter(_ => syntf)

          val syntfuncConverted = Some(funcsConverted(argId))
            .filter(_ => syntfConverted)

          val goldRole = roleLabelsOpt
            .map(_.apply(argId).role)
            .filter(_ => gold)

          val goldRoleWithSense = roleLabelsOpt
            .map(_.apply(argId).toString)
            .filter(_ => goldWithSense)

          modal.orElse(neg).orElse(disc).orElse(adjunct)
            .orElse(syntfunc).orElse(syntfuncConverted)
            .orElse(goldRole).orElse(goldRoleWithSense)
        }
        verbType -> args.groupBy(getSideClusterLabel).flatMap { case (keyOpt, v) => keyOpt.map(_ -> v) }
      }
    }
    _ <- Log.info(s"${results.foldMap(_._2.unorderedFoldMap(_.size))} arguments extracted into side clusters.")
  } yield results.toMap
  override def toString = {
    List(
      if(modals) "m" else "",
      if(negation) "n" else "",
      if(adjuncts) "a" else "",
      if(discourse) "d" else "",
      if(syntf) "s" else if(syntfConverted) "c" else "",
      if(gold) "g" else if(goldWithSense) "r" else ""
    ).mkString
  }
}
object SideClusteringModel {
  val negationWords = jjm.ling.en.Inflections.negationWords ++
    Set("never", "no longer").map(_.lowerCase)
  val modals = jjm.ling.en.Inflections.modalVerbs ++
    jjm.ling.en.Inflections.willVerbs ++
    Set("won't", "can't").map(_.lowerCase) ++
    Set("have", "going", "used", "gonna", "gon", "'ve", "able").map(_.lowerCase) // "phrasal modals"
  val adjunctFuncs = Set("TMP", "MNR", "LOC", "DIR")
  val discourseExpressions = Set(
    // standard examples from guidelines
    "also", "however", "too", "as well",
    "but", "and", "as we've seen before", "instead",
    "on the other hand", "for instance",
    // others
    "or", "though", // "then", "next" // questionable
    "hence",
    "furthermore", "moreover", "thus", "so", // "further" /// questionable
    // "either",
    "nonetheless", "regardless", // "well" // questionable
    "rather", "ironically", "indeed", // "just", "even", "only", "say" // questionable
    "certainly", "frankly", "similarly", "thereby", "therefore",
    // "exactly",
    // "still",
    "specifically", "particularly", "in particular",
    // "meanwhile", "meantime", "in the meantime",
    "in turn", "and so",
    "as a result", "after all",
    "in addition", "in contrast", "in other words",
    "for example",
    // "rather than",
    "for one thing", "for one",
    "in this case", "in that case", "in any case",
    "in any event", "in fact", "of course",
    // interjections
    "oh my god", "ah", "damn", // from guidelines
    "uh", "um", "gosh", "oh my gosh", "oh gosh",
  ).map(_.lowerCase)
  def fromString(x: String): Option[SideClusteringModel] = {
    Some(
      SideClusteringModel(
        x.contains("m"),
        x.contains("n"),
        x.contains("a"),
        x.contains("d"),
        x.contains("s"),
        x.contains("c") && !x.contains("s"),
        x.contains("g"),
        x.contains("r") && !x.contains("g"),
      )
    )
  }
}

sealed abstract class ArgumentModel(val sideClusteringModelOpt: Option[SideClusteringModel]) extends ClusteringModel {

  def clusterRemainingArguments[VerbType, Arg : Order](
    features: Features[VerbType, Arg],
    args: Map[VerbType, Set[ArgumentId[Arg]]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]]

  def getArgumentClusters[VerbType, Arg : Order](
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, Clustering.Argument[Arg]]] = for {
    allArgs <- features.args.get
    extraRoles <- sideClusteringModelOpt.foldMap(_.extractSideClusters(features))
    remainingArgs = allArgs.flatMap { case (vt, args) =>
      val extraArgs = extraRoles.get(vt).foldMap(_.unorderedFold)
      val remArgs = args.filterNot(extraArgs.contains)
      if(remArgs.isEmpty) None else Some(vt -> remArgs)
    }
    trees <- clusterRemainingArguments(features, remainingArgs)
  } yield allArgs.keys.iterator
    .map(vt => vt -> Clustering(trees.get(vt), extraRoles.getOrElse(vt, Map())))
    .toMap
}
object ArgumentModel {
  implicit val argumentModelOrder = Order.by[ArgumentModel, String](_.toString)

  def fromString(x: String): Option[ArgumentModel] =
    ClusteringModel.fromString(x).collect {
      case model: ArgumentModel => model
    }

  def fromStringWithSideModel[A](
    x: String, make: String => Option[SideClusteringModel] => Option[A]
  ): Option[A] = {
    if(x.contains("->")) {
      val substrings = x.split("->")
      SideClusteringModel.fromString(substrings(0)).flatMap(sideModel =>
        make(substrings(1))(Some(sideModel))
      )
    } else make(x)(None)
  }
}

case class BaselineArgumentModel(
  override val sideClusteringModelOpt: Option[SideClusteringModel], setting: String
) extends ArgumentModel(sideClusteringModelOpt) {

  private def getBaselineLabelCounts[VerbType, Arg, A](
    args: Map[VerbType, Set[ArgumentId[Arg]]],
    labels: VerbType => ArgumentId[Arg] => A)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[A, Int]] = {
    args.toList.infoBarFoldMapM("Counting baseline labels") { case (verbType, argIds) =>
      IO(argIds.toList.foldMap(argId => Map(labels(verbType)(argId) -> 1)).withDefaultValue(0))
    }
  }

  def makeDrawer[A](counts: Map[A, Int]): () => A = {
    val total = counts.values.sum
    val dist = counts.mapValues(_.toDouble / total).toVector
    () => {
      val rnd = scala.util.Random.nextDouble
      val res = dist.foldM(rnd) { case (mass, (label, prob)) =>
        if(prob >= mass) Left(label) else Right(mass - prob)
      }
      res match {
        case Right(mass) =>
          System.err.println(f"Didn't use all the mass! $mass%.5f")
            ???
        case Left(label) => label
      }
    }
  }

  def drawPair[A](items: Vector[A], getSize: A => Int): (Int, Int) = {
    val sizes = items.map(getSize)
    val total = sizes.sum
    val dist = sizes.map(_.toDouble / total)
    val fstRnd = scala.util.Random.nextDouble
    val fst = dist.zipWithIndex.foldM(fstRnd) { case (mass, (prob, index)) =>
      if(prob >= mass) Left(index) else Right(mass - prob)
    }.left.get
    val sndRnd = scala.util.Random.nextDouble
    val renorm = total.toDouble / (total - sizes(fst))
    val renormDist = dist.map(_ * renorm)
    val snd = renormDist.zipWithIndex.foldM(sndRnd) { case (mass, (prob, index)) =>
      if(index == fst) Right(mass) // skip over fst
      else if(prob >= mass) Left(index)
      else Right(mass - prob)
    }.left.get
    (fst, snd)
  }

  private def getBaselineLabels[VerbType, Arg](
    args: Map[VerbType, Set[ArgumentId[Arg]]],
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ) = setting match {
    case "syntf2" => features.argSyntacticFunctionsConverted.get.map(Some(_))
    case "syntf" => features.argSyntacticFunctions.get.map(Some(_))
    case "gold_nosense" => for {
      feats <- IO(features.getIfPropBank.get)
      argRoleLabels <- feats.mapArgFeats(feats.argRoleLabels.data)(_.role).get
    } yield Some(argRoleLabels.asInstanceOf[VerbType => ArgumentId[Arg] => String])
    case "gold_nosense+noise" => for {
      feats <- IO(features.getIfPropBank.get)
      argRoleLabels <- feats.mapArgFeats(feats.argRoleLabels.data)(_.role).get
      labelCounts <- getBaselineLabelCounts(
        args.asInstanceOf[Map[String, Set[ArgumentId[Arg]]]], argRoleLabels
      )
    } yield {
      val drawLabel = makeDrawer(labelCounts)
      Some(
        argRoleLabels.andThen(
          _.andThen(label =>
            if(scala.util.Random.nextDouble > 0.9) drawLabel() else label
          )
        ).asInstanceOf[VerbType => ArgumentId[Arg] => String]
      )
    }
    case "gold_wsense" => for {
      feats <- IO(features.getIfPropBank.get)
      argRoleLabels <- feats.mapArgFeats(feats.argRoleLabels.data)(_.toString).get
    } yield Some(argRoleLabels.asInstanceOf[VerbType => ArgumentId[Arg] => String])
    case "gold_wsense+noise" => for {
      feats <- IO(features.getIfPropBank.get)
      argRoleLabels <- feats.argRoleLabels.data.get
      labelCounts <- getBaselineLabelCounts(
        args.asInstanceOf[Map[String, Set[ArgumentId[Arg]]]], argRoleLabels
      )
    } yield {
      val drawLabel = makeDrawer(labelCounts)
      Some(
        argRoleLabels.andThen(
          _.andThen(label =>
            if(scala.util.Random.nextDouble > 0.9) label.copy(role = drawLabel().role).toString else label.toString
          )
        ).asInstanceOf[VerbType => ArgumentId[Arg] => String]
      )
    }
    case "random_linear" => {
      IO(Some((vt: VerbType) => ((argId: ArgumentId[Arg]) => argId.toString)))
    }
    case "random" | "random_weighted" => {
      IO(None)
    }
    case _ => ??? // should never happen
  }

  import scala.annotation.tailrec
  @tailrec private def randomlyCluster[A](items: NonEmptyVector[MergeTree[A]]): MergeTree[A] = {
    // else if(items.size == 2) MergeTree.Merge(0.0, items.head, items.tail.head)
    if(items.size == 1) items.head
    else {
      val i1 = (scala.util.Random.nextDouble * items.size).toInt
      val i2 = (scala.util.Random.nextDouble * (items.size - 1)).toInt
      val left = items.getUnsafe(i1)
      val swappedItems = items.updatedUnsafe(i1, items.getUnsafe(items.size.toInt - 1))
      val right = swappedItems.getUnsafe(i2)
      randomlyCluster(NonEmptyVector.fromVectorUnsafe(items.updatedUnsafe(i2, MergeTree.Merge(0.0, left, right)).init))
    }
  }
  def randomClustering[A](items: NonEmptyVector[A]): MergeTree[A] = {
    randomlyCluster(items.map(MergeTree.Leaf(0.0, _)))
  }

  import scala.annotation.tailrec
  @tailrec private def randomlyClusterWeighted[A](items: NonEmptyVector[MergeTree[A]]): MergeTree[A] = {
    // else if(items.size == 2) MergeTree.Merge(0.0, items.head, items.tail.head)
    if(items.size == 1) items.head
    else {
      val (i1, i2) = drawPair[MergeTree[A]](items.toVector, _.size.toInt)
      val newItem = MergeTree.Merge(0.0, items.getUnsafe(i1), items.getUnsafe(i2))
      val prevLast = items.getUnsafe(items.size.toInt - 1)
      randomlyClusterWeighted(
        NonEmptyVector.fromVectorUnsafe(
          items
            .updatedUnsafe(i1, newItem)
            .updatedUnsafe(i2, prevLast)
            .init
        )
      )
    }
  }
  def weightedRandomClustering[A](items: NonEmptyVector[A]): MergeTree[A] = {
    randomlyClusterWeighted(items.map(MergeTree.Leaf(0.0, _)))
  }

  def clusterRemainingArguments[VerbType, Arg : Order](
    features: Features[VerbType, Arg],
    args: Map[VerbType, Set[ArgumentId[Arg]]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]] = {
    for {
      _ <- Log.info("Initializing model features") // TODO maybe extend branching API to make this nice
      baselineLabelsOpt <- getBaselineLabels(args, features)
      baselineLabelCountsOpt <- baselineLabelsOpt.traverse(getBaselineLabelCounts(args, _))
      // TODO only traverse over verb types?
      allVerbArgSets <- features.verbArgSets.get
      results <- allVerbArgSets.toList.infoBarTraverse("Clustering arguments") { case (verbType, _) =>
        Log.info(features.renderVerbType(verbType)) >> {
          (baselineLabelsOpt, baselineLabelCountsOpt).mapN { (baselineLabels, baselineLabelCounts) =>
            val baselineLabelsForVerb = baselineLabels(verbType)
            val getOrderedVerbSets = (argIds: Set[ArgumentId[Arg]]) => argIds
              .groupBy(baselineLabelsForVerb)
              .toVector.sortBy(p => baselineLabelCounts(p._1)).map(_._2)
            // some of them are empty due present verbs with no args (that weren't filtered out). gotta skip those
            NonEmptyVector.fromVector(args.get(verbType).foldMap(_.toVector)).traverse { verbArgs =>
              IO {
                verbType -> getOrderedVerbSets(verbArgs.toVector.toSet)
                  .map(MergeTree.Leaf(0.0, _))
                  .reduceLeft[MergeTree[Set[ArgumentId[Arg]]]](MergeTree.Merge(0.0, _, _))
              }
            }
          }.getOrElse {
            // random baseline
            if(setting == "random") {
              NonEmptyVector.fromVector(args.get(verbType).foldMap(_.toVector)).traverse { verbArgs =>
                IO(verbType -> randomClustering(verbArgs.map(Set(_))))
              }
            } else {
              require(setting == "random_weighted")
              NonEmptyVector.fromVector(args.get(verbType).foldMap(_.toVector)).traverse { verbArgs =>
                IO(verbType -> weightedRandomClustering(verbArgs.map(Set(_))))
              }
            }
          }
        }
      }
    } yield results.flatten.toMap
  }
  override def toString = BaselineArgumentModel.toString(this)
}
object BaselineArgumentModel {

  val validSettings = Set(
    "syntf", "syntf2", "random", "random_linear", "random_weighted",
    "gold_nosense", "gold_nosense+noise",
    "gold_wsense", "gold_wsense+noise"
  )
  // too lazy for proper parser combinators
  // TODO add better error reporting
  def fromString(x: String): Option[BaselineArgumentModel] = {
    ArgumentModel.fromStringWithSideModel(
      x, y => sideModelOpt =>
      if(validSettings.contains(y)) Some(BaselineArgumentModel(sideModelOpt, y))
      else None
    )
  }

  def toString(model: BaselineArgumentModel): String = {
    val suffix = model.sideClusteringModelOpt.fold(model.setting)(x => s"$x->${model.setting}")
    "arg/" + suffix
  }
}

case class FullArgumentModel private (
  override val sideClusteringModelOpt: Option[SideClusteringModel], lossTerms: Map[FullArgumentModel.LossTerm, Double]
) extends ArgumentModel(sideClusteringModelOpt) {

  override def toString: String = FullArgumentModel.toString(this)

  type FlatAlg[Arg] = FlatClusteringAlgorithm { type Index = ArgumentId[Arg] }
  type AgglomAlg[Arg] = AgglomerativeClusteringAlgorithm { type Index = ArgumentId[Arg] }
  type AlgPair[Arg] = (FlatAlg[Arg], AgglomAlg[Arg])

  def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit] = {
    lossTerms.keySet.toList.traverse(_.init(features)).void
  }

  def create[VerbType, Arg](
    features: Features[VerbType, Arg], verbType: VerbType
  ): IO[AlgPair[Arg]] = {
    val termVec = lossTerms.toVector
    val lambdas = termVec.map(_._2)
    termVec.traverse(_._1.create(features, verbType): IO[AlgPair[Arg]])
      .map(_.unzip[FlatAlg[Arg], AgglomAlg[Arg]])
      .map { case (flatAlgs, agglomAlgs) =>
        val weightedFlatAlg = new WeightedFlatClusteringAlgorithm[ArgumentId[Arg]](flatAlgs.zip(lambdas))
        val weightedAgglomAlg = new WeightedAgglomerativeClusteringAlgorithm[ArgumentId[Arg]](agglomAlgs.zip(lambdas))
        (weightedFlatAlg, weightedAgglomAlg)
    }
  }

  def clusterRemainingArguments[VerbType, Arg : Order](
    features: Features[VerbType, Arg],
    args: Map[VerbType, Set[ArgumentId[Arg]]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, MergeTree[Set[ArgumentId[Arg]]]]] = {
    for {
      _ <- Log.info("Initializing model features") // TODO maybe extend branching API to make this nice
      _ <- this.init(features)
      // TODO only traverse over verb types?
      allVerbArgSets <- features.verbArgSets.get
      results <- allVerbArgSets.toList.infoBarTraverse("Clustering arguments") { case (verbType, verbs) =>
        Log.info(features.renderVerbType(verbType)) >> {
          // some of them are empty due present verbs with no args (that weren't filtered out). gotta skip those
          NonEmptyVector.fromVector(args.get(verbType).foldMap(_.toVector)).traverse { verbArgs =>
            this.create(features, verbType) >>= { case (flatAlgorithm, agglomAlgorithm) =>
              val setAgglomAlgorithm = new AgglomerativeSetClustering(agglomAlgorithm) // repetitive here, but whatever
              new HybridClustering().run(verbArgs, flatAlgorithm, agglomAlgorithm).map {
                case (argTree, _) => verbType -> argTree
              }
            }
          }
        }
      }
    } yield results.flatten.toMap
  }
}

object FullArgumentModel {

  def apply(sideModelOpt: Option[SideClusteringModel], terms: (LossTerm, Double)*): FullArgumentModel = {
    val total = terms.map(_._2).sum
    val termMap = terms.map { case (term, weight) =>
      term -> (scala.math.round(weight / total * 100.0) / 100.0)
    }.toMap
    require(terms.size == termMap.size)
    new FullArgumentModel(sideModelOpt, termMap)
  }

  val termIndex = List[(LossTerm, String)](
    NoOp -> "noop",
    QuestionEntropy -> "qent",
    QuestionEntropyActive -> "qent_active",
    QuestionEntropyOracleArgAdj -> "qent_argadj",
    QuestionEntropyNoHow -> "qent_nohow",
    QuestionEntropyOracleArgAdjNoHow -> "qent_argadj_nohow",
    QuestionEntropyNormalizedAdverbials -> "qent_nadv",
    MaxPriorEntropy -> "pent",
    SyntacticFunction -> "syntf",
    SyntacticFunctionConverted -> "syntf2",
    ConstituentType -> "syntc",
    ConstituentTypeConverted -> "syntc2",
    Preposition -> "prep",
    DifferentVerbs -> "dv"
  ) ++ List("masked", "repeated", "symm_both", "symm_left", "symm_right").flatMap(mode =>
    List(
      HeadMLMEntropy(mode) -> s"head_mlm_$mode",
      PrepMLMEntropy(mode) -> s"prep_mlm_$mode",
      SpanMLMEntropy(mode) -> s"span_mlm_$mode"
    )
  )
  val termToString = termIndex.toMap
  val stringToTerm = termIndex.map(_.swap).toMap

  // too lazy for proper parser combinators
  // TODO add better error reporting
  def fromString(x: String): Option[FullArgumentModel] = {
    ArgumentModel.fromStringWithSideModel(
      x, y => sideModelOpt => {
        if(stringToTerm.contains(y)) Some(FullArgumentModel(sideModelOpt, stringToTerm(y) -> 1.0))
        else scala.util.Try {
          val termStrings = y.split("\\+").toList
          val terms = termStrings.map { term =>
            if(stringToTerm.contains(term)) (stringToTerm(term) -> 1.0)
            else {
              val components = term.split("\\*")
              stringToTerm(components(0)) -> components(1).toDouble
            }
          }
          FullArgumentModel(sideModelOpt, terms: _*)
        }.toOption
      }
    )
  }
  def toString(model: FullArgumentModel): String = {
    val postSuffix = model.lossTerms.toList.map { case (term, weight) =>
      if(weight == 1.0) termToString(term)
      else f"${termToString(term)}%s*${weight}%.2f"
    }.sorted.mkString("+")
    val suffix = model.sideClusteringModelOpt.fold(postSuffix)(x => s"$x->$postSuffix")
    "arg/" + suffix
  }

  sealed trait LossTerm {

    type FlatParam
    type AgglomParam

    def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit]

    def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ): IO[
      (FlatClusteringAlgorithm { type Index = ArgumentId[Arg]; type ClusterParam = FlatParam },
       AgglomerativeClusteringAlgorithm { type Index = ArgumentId[Arg]; type ClusterParam = AgglomParam })
    ]
  }

  abstract class DistributionEntropy[A] extends LossTerm {
    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[Map[A, Double]]

    type FlatParam = DenseMultinomial
    type AgglomParam = MinEntropyClusteringSparse.ClusterMixture

    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = getDists(features).get.as(())

    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = for {
      dists <- getDists(features).get.map(_.apply(verbType))
      args <- features.args.get.map(_.apply(verbType))
      // priorEntropy = {
      //   val totalCounts = args.unorderedFoldMap(dists)
      //   val total = totalCounts.unorderedFold
      //   val res = totalCounts.values.iterator.filter(_ > 0).map { count =>
      //     val prob = count / total
      //     scala.math.log(prob) * prob
      //   }.sum * -1.0
      //   if(res == 0.0) 1.0 else res
      // }
      vocab = Vocab.make(args.unorderedFoldMap(argId => dists(argId).keySet))
      // tfidf = TFIDF.makeTransform(
      //   headProbabilityMass = 0.99,
      //   priorSmoothingLambda = 1000.0,
      //   priorTruncationHead = .99
      // )(args.toList.map(dists))
      indexedInstances = args.iterator.map { argId =>
        // val dist = tfidf(dists(argId))
        val dist = dists(argId)
        // argId -> dist.map { case (q, p) => vocab.getIndex(q) -> (p / priorEntropy) }
        argId -> dist.map { case (q, p) => vocab.getIndex(q) -> p }
      }.toMap
    } yield (
      new DirichletMAPClusteringSparse(indexedInstances, vocab.size, 0.01),
      new MinEntropyClusteringSparse(indexedInstances, vocab.size)
    )
  }

  case object QuestionEntropy extends DistributionEntropy[QuestionTemplate] {
    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[Map[QuestionTemplate, Double]] = features.argQuestionDists.data
  }

  // with added passive->active alternation rules
  case object QuestionEntropyActive extends DistributionEntropy[QuestionTemplate] {
    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[Map[QuestionTemplate, Double]] = features.argQuestionDistsActive.data
  }

  case object QuestionEntropyOracleArgAdj extends DistributionEntropy[(QuestionTemplate, Boolean)] {
    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[Map[(QuestionTemplate, Boolean), Double]] = {
      features.getIfPropBank.get.argRoleLabels.data
        .zip(features.argQuestionDists.data)
        .map { case (roleLabels, questionDists) =>
          (verb: VerbType) => (argId: ArgumentId[Arg]) => {
            val isAdjunct = roleLabels(verb.asInstanceOf[String])(argId).role.startsWith("AM-")
            questionDists(verb)(argId).map { case (q, c) =>
              (q, isAdjunct) -> c
            }
          }
        }
    }
  }

  case object QuestionEntropyNoHow extends DistributionEntropy[QuestionTemplate] {
    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[Map[QuestionTemplate, Double]] =
      features.mapArgFeats(features.argQuestionDists.data)(dist =>
        dist.map { case (qt, pcount) =>
          if(qt.wh == "how".lowerCase) qt -> (pcount / 100.0)
          else qt -> pcount
        }
      )
  }

  case object QuestionEntropyOracleArgAdjNoHow extends DistributionEntropy[(QuestionTemplate, Boolean)] {
    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[Map[(QuestionTemplate, Boolean), Double]] = {
      features.getIfPropBank.get.argRoleLabels.data
        .zip(features.argQuestionDists.data)
        .map { case (roleLabels, questionDists) =>
          (verb: VerbType) => (argId: ArgumentId[Arg]) => {
            val isAdjunct = roleLabels(verb.asInstanceOf[String])(argId).role.startsWith("AM-")
            questionDists(verb)(argId).map { case (q, origC) =>
              val c = if(q.wh == "how".lowerCase) origC / 100.0 else origC
              (q, isAdjunct) -> c
            }
          }
        }
    }
  }

  // normalize out adverbials
  case object QuestionEntropyNormalizedAdverbials extends DistributionEntropy[QuestionTemplate] {
    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[Map[QuestionTemplate, Double]] = features.argQuestionDistsNormalizedAdverbials.data
  }

  case object ConstituentType extends DistributionEntropy[String] {
    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[Map[String, Double]] = features.argConstituentTypeDists.data
  }

  case object ConstituentTypeConverted extends DistributionEntropy[String] {
    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[Map[String, Double]] = features.argConstituentTypeDistsConverted.data
  }

  case object Preposition extends DistributionEntropy[String] {
    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[Map[String, Double]] = features.argPrepositionDists
  }

  case object SyntacticFunction extends DistributionEntropy[String] {
    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[Map[String, Double]] =
      features.mapArgFeats(features.argSyntacticFunctions.data)(
        syntf => Map(syntf -> 1.0)
      )
  }

  case object SyntacticFunctionConverted extends DistributionEntropy[String] {
    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[Map[String, Double]] =
      features.mapArgFeats(features.argSyntacticFunctionsConverted.data)(
        syntf => Map(syntf -> 1.0)
      )
  }

  case object NoOp extends LossTerm {
    type FlatParam = DenseMultinomial
    type AgglomParam = MinEntropyClusteringSparse.ClusterMixture

    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.args.get.as(())

    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = for {
      args <- features.args.get.map(_.apply(verbType))
      indexedInstances = args.iterator.map { argId =>
        argId -> Map(0 -> 1.0)
      }.toMap
    } yield (
      new DirichletMAPClusteringSparse(indexedInstances, 1, 0.01),
      new MinEntropyClusteringSparse(indexedInstances, 1)
    )
  }

  case object DifferentVerbs extends LossTerm {
    type FlatParam = Map[VerbId, Double]
    type AgglomParam = Map[VerbId, Double]

    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.verbArgSets.get.as(())

    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = for {
      argIdToVerbId <- features.verbArgSets.get.map(_.apply(verbType)).map(
        _.toList.flatMap { case (verbId, args) =>
          args.toList.map(arg => ArgumentId(verbId, arg) -> verbId)
        }.toMap
      )
    } yield (
      new NonLikeClustering(argIdToVerbId),
      new NonLikeClustering(argIdToVerbId)
    )
  }

  case object MaxPriorEntropy extends LossTerm {
    type FlatParam = DenseMultinomial
    type AgglomParam = Int

    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.args.get.as(())

    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = for {
      args <- features.args.get.map(_.apply(verbType))
      indexedInstances = args.iterator.map { argId =>
        argId -> Map(0 -> 1.0)
      }.toMap
    } yield (
      new DirichletMAPClusteringSparse(indexedInstances, 1, 0.01),
      new MaxPriorEntropyClustering[ArgumentId[Arg]](args.size)
    )
  }

  // TODO
  // case object MaximumEntropy extends LossTerm {
  //   type FlatParam = DenseMultinomial
  //   type AgglomParam = MinEntropyClusteringSparse.ClusterMixture

  //   override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.argSyntacticFunctions.get.as(())

  //   override def create[VerbType, Arg](
  //     features: Features[VerbType, Arg], verbType: VerbType
  //   ) = for {
  //     argSyntacticFunctions <- features.argSyntacticFunctions.get.map(_.apply(verbType))
  //     syntFuncVocab = Vocab.make(argSyntacticFunctions.value.values.toSet)
  //     // TODO add tf-idf transform?
  //     indexedInstances = argSyntacticFunctions.value.map { case (argId, syntFunc) =>
  //       argId -> Map(syntFuncVocab.getIndex(syntFunc) -> 1.0)
  //     }
  //   } yield (
  //     new DirichletMAPClusteringSparse(indexedInstances, syntFuncVocab.size, 0.01),
  //     new MinEntropyClusteringSparse(indexedInstances, syntFuncVocab.size)
  //   )
  // }

  import breeze.linalg.DenseVector

  abstract class DenseDistributionEntropy extends LossTerm {
    type FlatParam = DenseVector[Float]
    type AgglomParam = MinEntropyClusteringDense.ClusterMixture

    def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[DenseVector[Float]]

    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = getDists(features).get.as(())

    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = for {
      argMLMFeatures <- getDists(features).get.map(_.apply(verbType))
      // tfidf = TFIDF.makeTransform(
      //   headProbabilityMass = 0.99,
      //   priorSmoothingLambda = 1000.0,
      //   priorTruncationHead = .99
      // )(questionDists.value)
    } yield (
      new DirichletMAPClusteringDense(argMLMFeatures, features.mlmFeatureDim, 0.01f),
      new MinEntropyClusteringDense(argMLMFeatures, features.mlmFeatureDim)
    )
  }

  case class HeadMLMEntropy(mode: String) extends DenseDistributionEntropy {
    override def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[DenseVector[Float]] = {
      features.getArgHeadMLMFeatures(mode)
    }
  }

  case class PrepMLMEntropy(mode: String) extends DenseDistributionEntropy {
    override def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[DenseVector[Float]] = {
      features.getArgPrepMLMFeatures(mode)
    }
  }

  case class SpanMLMEntropy(mode: String) extends DenseDistributionEntropy {
    override def getDists[VerbType, Arg](
      features: Features[VerbType, Arg]
    ): features.ArgFeats[DenseVector[Float]] = {
      features.getArgSpanMLMFeatures(mode)
    }
  }
}

sealed trait VerbModel extends ClusteringModel {
  def getVerbClusters[VerbType, Arg](
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, Clustering.Verb]]
}
object VerbModel {
  def fromString(x: String): Option[VerbModel] =
    ClusteringModel.fromString(x).collect {
      case model: VerbModel => model
    }
}

case class BaselineVerbModel(setting: String) extends VerbModel {

  private def getBaselineLabelCounts[VerbType, Arg, A](
    verbs: Set[VerbId],
    labels: VerbId => A)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): Map[A, Int] = {
    verbs.toList.foldMap(verbId => Map(labels(verbId) -> 1).withDefaultValue(0))
  }

  def makeDrawer[A](counts: Map[A, Int]): () => A = {
    val total = counts.values.sum
    val dist = counts.mapValues(_.toDouble / total).toVector
    () => {
      val rnd = scala.util.Random.nextDouble
      val res = dist.foldM(rnd) { case (mass, (label, prob)) =>
        if(prob >= mass) Left(label) else Right(mass - prob)
      }
      res match {
        case Right(mass) =>
          System.err.println(f"Didn't use all the mass! $mass%.5f")
            ???
        case Left(label) => label
      }
    }
  }

  def drawPair[A](items: Vector[A], getSize: A => Int): (Int, Int) = {
    val sizes = items.map(getSize)
    val total = sizes.sum
    val dist = sizes.map(_.toDouble / total)
    val fstRnd = scala.util.Random.nextDouble
    val fst = dist.zipWithIndex.foldM(fstRnd) { case (mass, (prob, index)) =>
      if(prob >= mass) Left(index) else Right(mass - prob)
    }.left.get
    val sndRnd = scala.util.Random.nextDouble
    val renorm = total.toDouble / (total - sizes(fst))
    val renormDist = dist.map(_ * renorm)
    val snd = renormDist.zipWithIndex.foldM(sndRnd) { case (mass, (prob, index)) =>
      if(index == fst) Right(mass) // skip over fst
      else if(prob >= mass) Left(index)
      else Right(mass - prob)
    }.left.get
    (fst, snd)
  }

  private def getBaselineLabels[VerbType, Arg](
    verbs: Map[VerbType, Set[VerbId]],
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Option[VerbType => VerbId => String]] = setting match {
    case "gold" => for {
      feats <- IO(features.getIfPropBank.get)
      verbSenseLabels <- feats.verbSenseLabels.get
    } yield Some(verbSenseLabels).asInstanceOf[Option[VerbType => VerbId => String]]
    case "gold+uniform_noise" => for {
      feats <- IO(features.getIfPropBank.get)
      verbs  <- feats.verbs.get
      verbSenseLabels <- feats.verbSenseLabels.get
    } yield {
      val labelCountsByVerb = verbs.transform { case (verbType, verbIds) =>
        val getLabel = verbSenseLabels(verbType)
        verbIds.map(getLabel).map(_ -> 1).toMap
      }
      Some(
        (verbType: String) => {
          val goldLabels = verbSenseLabels(verbType)
          val labelCounts = labelCountsByVerb(verbType)
          val drawLabel = makeDrawer(labelCounts)
          (verbId: VerbId) => {
            val goldLabel = goldLabels(verbId)
            if(scala.util.Random.nextDouble > 0.9) drawLabel() else goldLabel
          }
        }
      ).asInstanceOf[Option[VerbType => VerbId => String]]
    }
    case "gold+weighted_noise" => for {
      feats <- IO(features.getIfPropBank.get)
      verbs  <- feats.verbs.get
      verbSenseLabels <- feats.verbSenseLabels.get
    } yield {
      val labelCountsByVerb = verbs.transform { case (verbType, verbIds) =>
        getBaselineLabelCounts(verbIds, verbSenseLabels(verbType))
      }
      Some(
        (verbType: String) => {
          val goldLabels = verbSenseLabels(verbType)
          val labelCounts = labelCountsByVerb(verbType)
          val drawLabel = makeDrawer(labelCounts)
          (verbId: VerbId) => {
            val goldLabel = goldLabels(verbId)
            if(scala.util.Random.nextDouble > 0.9) drawLabel() else goldLabel
          }
        }
      ).asInstanceOf[Option[VerbType => VerbId => String]]
    }
    case "random_linear" => {
      IO(Some((vt: VerbType) => ((verbId: VerbId) => verbId.toString)))
    }
    case "random" | "random_weighted" => {
      IO(None)
    }
    case _ => ??? // should never happen
  }

  import scala.annotation.tailrec
  @tailrec private def randomlyCluster[A](items: NonEmptyVector[MergeTree[A]]): MergeTree[A] = {
    // else if(items.size == 2) MergeTree.Merge(0.0, items.head, items.tail.head)
    if(items.size == 1) items.head
    else {
      val i1 = (scala.util.Random.nextDouble * items.size).toInt
      val i2 = (scala.util.Random.nextDouble * (items.size - 1)).toInt
      val left = items.getUnsafe(i1)
      val swappedItems = items.updatedUnsafe(i1, items.getUnsafe(items.size.toInt - 1))
      val right = swappedItems.getUnsafe(i2)
      randomlyCluster(NonEmptyVector.fromVectorUnsafe(items.updatedUnsafe(i2, MergeTree.Merge(0.0, left, right)).init))
    }
  }
  def randomClustering[A](items: NonEmptyVector[A]): MergeTree[A] = {
    randomlyCluster(items.map(MergeTree.Leaf(0.0, _)))
  }

  import scala.annotation.tailrec
  @tailrec private def randomlyClusterWeighted[A](items: NonEmptyVector[MergeTree[A]]): MergeTree[A] = {
    // else if(items.size == 2) MergeTree.Merge(0.0, items.head, items.tail.head)
    if(items.size == 1) items.head
    else {
      val (i1, i2) = drawPair[MergeTree[A]](items.toVector, _.size.toInt)
      val newItem = MergeTree.Merge(0.0, items.getUnsafe(i1), items.getUnsafe(i2))
      val prevLast = items.getUnsafe(items.size.toInt - 1)
      randomlyClusterWeighted(
        NonEmptyVector.fromVectorUnsafe(
          items
            .updatedUnsafe(i1, newItem)
            .updatedUnsafe(i2, prevLast)
            .init
        )
      )
    }
  }
  def weightedRandomClustering[A](items: NonEmptyVector[A]): MergeTree[A] = {
    randomlyClusterWeighted(items.map(MergeTree.Leaf(0.0, _)))
  }

  def getVerbClusters[VerbType, Arg](
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, Clustering.Verb]] = {
    for {
      _ <- Log.info("Initializing model features")
      allVerbs <- features.verbs.get
      baselineLabelsOpt <- getBaselineLabels(allVerbs, features)
      results <- allVerbs.toList.infoBarTraverse("Clustering verbs") { case (verbType, verbs) =>
        Log.info(features.renderVerbType(verbType)) >> {
          baselineLabelsOpt.map { baselineLabels =>
            val baselineLabelsForVerb = baselineLabels(verbType)
            // some of them are empty due present verbs with no args (that weren't filtered out). gotta skip those
            NonEmptyVector.fromVector(allVerbs(verbType).toVector).traverse { verbs =>
              IO {
                val orderedVerbSets = verbs.toVector.toSet
                  .groupBy(baselineLabelsForVerb)
                  .toVector.sortBy(p => p._2.size).map(_._2)
                verbType -> orderedVerbSets
                  .map(MergeTree.Leaf(0.0, _))
                  .reduceLeft[MergeTree[Set[VerbId]]](MergeTree.Merge(0.0, _, _))
              }
            }
          }.getOrElse {
            // random baseline
            if(setting == "random") {
              NonEmptyVector.fromVector(allVerbs(verbType).toVector).traverse { args =>
                IO(verbType -> randomClustering(args.map(Set(_))))
              }
            } else {
              require(setting == "random_weighted")
              NonEmptyVector.fromVector(allVerbs(verbType).toVector).traverse { args =>
                IO(verbType -> weightedRandomClustering(args.map(Set(_))))
              }
            }
          }
        }
      }
    } yield results.flatten.toMap.mapVals(x => Clustering(Some(x)))
  }
  override def toString = BaselineVerbModel.toString(this)
}
object BaselineVerbModel {

  // too lazy for proper parser combinators
  // TODO add better error reporting
  def fromString(x: String): Option[BaselineVerbModel] = x match {
    case x @ (
      "random" | "random_linear" | "random_weighted"
        | "gold" | "gold+uniform_noise" | "gold+weighted_noise") => Some(BaselineVerbModel(x))
    case _ => None
  }
  def toString(model: BaselineVerbModel): String = {
    "verb/" + model.setting
  }
}

case class FullVerbModel private (
  lossTerms: Map[FullVerbModel.LossTerm, Double]
) extends VerbModel {

  override def toString: String = FullVerbModel.toString(this)

  type FlatAlg = FlatClusteringAlgorithm { type Index = VerbId }
  type AgglomAlg = AgglomerativeClusteringAlgorithm { type Index = VerbId }
  type AlgPair = (FlatAlg, AgglomAlg)

  def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit] = {
    lossTerms.keySet.toList.traverse(_.init(features)).void
  }

  def create[VerbType, Arg](
    features: Features[VerbType, Arg], verbType: VerbType
  ): IO[AlgPair] = {
    val termVec = lossTerms.toVector
    val lambdas = termVec.map(_._2)
    termVec.traverse(_._1.create(features, verbType): IO[AlgPair])
      .map(_.unzip[FlatAlg, AgglomAlg])
      .map { case (flatAlgs, agglomAlgs) =>
        val weightedFlatAlg = new WeightedFlatClusteringAlgorithm[VerbId](flatAlgs.zip(lambdas))
        val weightedAgglomAlg = new WeightedAgglomerativeClusteringAlgorithm[VerbId](agglomAlgs.zip(lambdas))
        (weightedFlatAlg, weightedAgglomAlg)
    }
  }

  def getVerbClusters[VerbType, Arg](
    features: Features[VerbType, Arg])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Map[VerbType, Clustering.Verb]] = {
    for {
      _ <- Log.info("Initializing model features") // TODO maybe extend branching API to make this nice
      _ <- this.init(features)
      allVerbArgSets <- features.verbArgSets.get
      results <- allVerbArgSets.toList.infoBarTraverse("Clustering verbs") { case (verbType, verbs) =>
        val verbIds = verbs.keySet
        Log.info(features.renderVerbType(verbType)) >> {
          // TODO I don't think any of these should be empty
          NonEmptyVector.fromVector(verbIds.toVector).traverse { args =>
            this.create(features, verbType) >>= { case (flatAlgorithm, agglomAlgorithm) =>
              val setAgglomAlgorithm = new AgglomerativeSetClustering(agglomAlgorithm) // repetitive here, but whatever
              new HybridClustering().run(args, flatAlgorithm, agglomAlgorithm).map {
                case (verbTree, _) => verbType -> verbTree
              }
            }
          }
        }
      }
    } yield results.flatten.toMap.mapVals(x => Clustering(Some(x)))
  }
}

object FullVerbModel {

  def apply(terms: (LossTerm, Double)*): FullVerbModel = {
    val total = terms.map(_._2).sum
    val termMap = terms.map { case (term, weight) =>
      term -> (scala.math.round(weight / total * 100.0) / 100.0)
    }.toMap
    require(terms.size == termMap.size)
    new FullVerbModel(termMap)
  }

  val termIndex = List[(LossTerm, String)](
    NoOp -> "noop"
  ) ++ List("masked", "repeated", "symm_both", "symm_left", "symm_right").map(mode =>
    MLMEntropy(mode) -> s"mlm_$mode",
  )
  val termToString = termIndex.toMap
  val stringToTerm = termIndex.map(_.swap).toMap

  // too lazy for proper parser combinators
  // TODO add better error reporting
  def fromString(x: String): Option[FullVerbModel] = {
    if(stringToTerm.contains(x)) Some(FullVerbModel(stringToTerm(x) -> 1.0))
    else scala.util.Try {
      val termStrings = x.split("\\+").toList
      val terms = termStrings.map { term =>
        if(stringToTerm.contains(term)) (stringToTerm(term) -> 1.0)
        else {
          val components = term.split("\\*")
          stringToTerm(components(0)) -> components(1).toDouble
        }
      }
      FullVerbModel(terms: _*)
    }.toOption
  }
  def toString(model: FullVerbModel): String = {
    "verb/" + model.lossTerms.toList.map { case (term, weight) =>
      if(weight == 1.0) termToString(term)
      else f"${termToString(term)}%s*${weight}%.2f"
    }.sorted.mkString("+")
  }

  sealed trait LossTerm {

    type FlatParam
    type AgglomParam

    def init[VerbType, Arg](features: Features[VerbType, Arg]): IO[Unit]

    def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ): IO[
      (FlatClusteringAlgorithm { type Index = VerbId; type ClusterParam = FlatParam },
       AgglomerativeClusteringAlgorithm { type Index = VerbId; type ClusterParam = AgglomParam })
    ]
  }

  import breeze.linalg.DenseVector

  case class MLMEntropy(mode: String) extends LossTerm {
    type FlatParam = DenseVector[Float]
    type AgglomParam = MinEntropyClusteringDense.ClusterMixture

    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.getVerbMLMFeatures(mode).get.as(())

    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = for {
      verbIds <- features.verbArgSets.get.map(_.apply(verbType).keySet)
      verbMLMFeatures <- features.getVerbMLMFeatures(mode).get.map(_.apply(verbType))
      // featsCached = verbIds.iterator.map(verbId => verbId -> verbMLMFeatures(verbId)).toMap
      // tfidf = TFIDF.Dense.makeTransform(
      //   headProbabilityMass = 0.97f,
      //   priorSmoothingLambda = 0.1f,
      //   priorTruncationHead = 0.99f
      // )(featsCached.values.toList)
      // feats = featsCached.mapVals(tfidf) 
      feats = verbMLMFeatures
    } yield (
      new DirichletMAPClusteringDense(feats, features.mlmFeatureDim, 0.01f),
      new MinEntropyClusteringDense(feats, features.mlmFeatureDim)
    )
  }

  case object NoOp extends LossTerm {
    type FlatParam = DenseMultinomial
    type AgglomParam = MinEntropyClusteringSparse.ClusterMixture

    override def init[VerbType, Arg](features: Features[VerbType, Arg]) = features.verbs.get.as(())

    override def create[VerbType, Arg](
      features: Features[VerbType, Arg], verbType: VerbType
    ) = for {
      verbs <- features.verbs.get.map(_.apply(verbType))
      indexedInstances = verbs.iterator.map { verbId =>
        verbId -> Map(0 -> 1.0)
      }.toMap
    } yield (
      new DirichletMAPClusteringSparse(indexedInstances, 1, 0.01),
      new MinEntropyClusteringSparse(indexedInstances, 1)
    )
  }
}

//   case object VerbSqDist extends FullVerbModel[VectorMeanClustering.ClusterMean] {
//     override def init[VerbType, Instance](features: Features[VerbType, Instance]) = features.elmoVecs.get.as(())
//     override def create[VerbType, Instance](
//       features: Features[VerbType, Instance], verbType: VerbType
//     ) = for {
//       vectors <- features.elmoVecs.get.map(_.apply(verbType))
//     } yield new VectorMeanClustering(vectors.value)
//   }

//   case object AnswerEntropy extends FullArgumentModel[DenseMultinomial, MinEntropyClusteringSparse.ClusterMixture] {
//     override def init[VerbType, Instance](features: Features[VerbType, Instance]) = for {
//       sentences <- features.sentences.get
//       tokenCounts <- features.argSpans.get
//     } yield ()

//     override def create[VerbType, Instance](
//       features: Features[VerbType, Instance], verbType: VerbType
//     ) = for {
//       // val pronouns = Set(
//       //   "he", "him", "it", "she", "her", "they", "them", "we", "us"
//       // ).map(_.lowerCase)
//       // val tokensToSkip = pronouns

//       // TODO TF-IDF thing

//       sentences <- features.sentences.get
//       tokenProbs <- features.argSpans.get.map(
//         _.apply(verbType).value.toList.foldMap { case (argId, spanScores) =>
//           val sentenceTokens = sentences.value(argId.verbId.sentenceId)
//           // val numTokens = spanSets.foldMap(_.foldMap(_.length))
//           // val singleCount = 1.0 / numTokens
//           val tokenPseudocounts = spanScores.toList.foldMap { case (span, score) =>
//             sentenceTokens.slice(span.begin, span.endExclusive)
//               .map(_.lowerCase)
//               .foldMap(t => Map(t -> score))
//             // .filter(t => !tokensToSkip.contains(t))
//           }
//           val total = tokenPseudocounts.values.sum
//           val normalizedTokenCounts = tokenPseudocounts.transform { case (_, s) => s / total }
//           Map(argId -> normalizedTokenCounts)
//         }
//       )
//       tokenVocab = Vocab.make(tokenProbs.unorderedFoldMap(_.keySet))
//       indexedTokenProbs = tokenProbs.map { case (qid, qTokenProbs) =>
//         qid -> qTokenProbs.map { case (tok, pcount) => tokenVocab.getIndex(tok) -> pcount }
//       }
//     } yield (
//       new DirichletMAPClusteringSparse(indexedTokenProbs, tokenVocab.size, 0.01),
//       new MinEntropyClusteringSparse(indexedTokenProbs, tokenVocab.size)
//     )
//   }
// }

// // object VerbClauseEntropy extends FrameInductionModel[MinEntropyClusteringSparse.ClusterMixture] {
// //   override def init[VerbType, Instance](features: Features[VerbType, Instance]) = features.verbArgSets.get.as(())
// //   override def create[VerbType, Instance](
// //     features: Features[VerbType, Instance], verbType: VerbType
// //   ) = for {
// //     clauseCounts <- features.verbArgSets.get.map(
// //       _.apply(verbType).map {
// //         case (verbId, qaPairs) =>
// //           verbId -> qaPairs.keys.toList.foldMap { question =>
// //             Map(question.clauseTemplate -> 1.0)
// //           }
// //       }
// //     )
// //     clauseVocab = Vocab.make(clauseCounts.values.toList.foldMap(_.keySet))
// //     indexedClauseCounts = clauseCounts.map { case (verbId, counts) =>
// //       verbId -> counts.map { case (clause, count) =>
// //         clauseVocab.getIndex(clause) -> count
// //       }
// //     }
// //   } yield new MinEntropyClusteringSparse(indexedClauseCounts, clauseVocab.size)
// // }

// // import breeze.linalg.DenseVector

// // object AnswerNLL extends FrameInductionModel[MixtureOfStatesClustering.StateCounts] {
// //   override def init[VerbType, Instance](features: Features[VerbType, Instance]) = for {
// //     answerNLLInfo <- features.answerNLLs.get
// //   } yield ()

// //   import jjm.ling.en.InflectedForms
// //   import qasrl.ArgumentSlot
// //   import qasrl.Frame
// //   def getQuestionTemplate(templateQ: (ArgStructure, ArgumentSlot)): String = {
// //     val frame = Frame(
// //       verbInflectedForms = InflectedForms.generic,
// //       args = templateQ._1.args,
// //       tense = qasrl.PresentTense,
// //       isPerfect = false,
// //       isPassive = templateQ._1.isPassive,
// //       isNegated = false, isProgressive = false)
// //     frame.questionsForSlot(templateQ._2).head
// //   }

// //   override def create[VerbType, Instance](
// //     features: Features[VerbType, Instance], verbType: VerbType
// //   ) = for {
// //     answerNLLInfo <- features.answerNLLs.get.map(_.apply(verbType))
// //     templateQVocab = Vocab.make(
// //       answerNLLInfo.values.flatten
// //         .map(_._1)
// //         .map(getQuestionTemplate)
// //         .toSet
// //     )
// //     templateQVectorsByQid = answerNLLInfo.map { case (qid, qsWithNLLs) =>
// //       val qsWithNLLsMap = qsWithNLLs
// //         .groupBy(p => getQuestionTemplate(p._1))
// //         .map { case (k, nlls) => k -> nlls.map(_._2).min }
// //       qid -> MixtureOfStatesClustering.StateInstance(
// //         templateQVocab.getIndex(getQuestionTemplate(qid.question.template)),
// //         DenseVector.tabulate(templateQVocab.size)(i =>
// //           qsWithNLLsMap(templateQVocab.getItem(i))
// //         )
// //       )
// //     }
// //   } yield new MixtureOfStatesClustering[QuestionId](
// //     getInstance = templateQVectorsByQid,
// //     vocabSize = templateQVocab.size
// //   )
// // }
