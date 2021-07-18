package qasrl.roles.modeling

import qasrl.roles.clustering.MergeTree

import io.circe.generic.JsonCodec

import monocle.macros._

import cats.implicits._

@JsonCodec sealed trait ClusterSplittingCriterion {
  import ClusterSplittingCriterion._
  def getNumber: Option[Int] = this match { case Number(value) => Some(value); case _ => None }
  def getEntropy: Option[Double] = this match { case Entropy(penalty) => Some(penalty); case _ => None }
  def isNumber: Boolean = getNumber.nonEmpty
  def isEntropy: Boolean = getEntropy.nonEmpty

  def splitTree[A](tree: MergeTree[A], count: A => Double): Vector[MergeTree[A]] = this match {
    case Number(numClusters) => tree
        .clusterSplittingsStreamByMaxLossPerCount(count)
        .dropWhile(_.size < numClusters).take(1)
        .compile.toList.headOption.getOrElse(tree.leaves)
    case Entropy(penalty) =>
      val numItems = tree.unorderedFoldMap(count)
      tree
        .clusterSplittingsStreamByMaxLossPerCount(count)
        .take(100).map { splitting =>
          val mixingLoss = splitting.foldMap { t =>
            val size = t.unorderedFoldMap(count)
            -size * scala.math.log(size / numItems)
          }
          val totalLoss = splitting.foldMap(_.loss) + (penalty * mixingLoss)
          splitting -> totalLoss
        }
        .compile.toList.minBy(_._2)._1
  }
}
object ClusterSplittingCriterion {
  @Lenses @JsonCodec case class Number(value: Int) extends ClusterSplittingCriterion
  @Lenses @JsonCodec case class Entropy(penalty: Double) extends ClusterSplittingCriterion

  val number = GenPrism[ClusterSplittingCriterion, Number].composeIso(
    monocle.Iso[Number, Int](_.value)(Number(_))
  )
  val entropy = GenPrism[ClusterSplittingCriterion, Entropy].composeIso(
    monocle.Iso[Entropy, Double](_.penalty)(Entropy(_))
  )
}
