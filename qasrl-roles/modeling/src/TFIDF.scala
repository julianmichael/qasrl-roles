package qasrl.roles.modeling

import jjm.implicits._

import cats.UnorderedFoldable
import cats.implicits._

object TFIDF extends TFIDFPlatformExtensions {

  def truncate[A](counts: Map[A, Double], headProportion: Double) = {
    val total = counts.unorderedFold
    val headSize = total * headProportion
    val headItems = counts.toVector.sortBy(-_._2)
      .map(p => p.copy(_1 = Set(p._1)))
      .scanLeft(Set[A]() -> 0.0)(_ |+| _)
      .dropWhile(_._2 < headSize)
      .headOption.map(_._1)
      .getOrElse(counts.keySet)

    counts.filter(p => headItems.contains(p._1))
  }

  def addLambda[A](counts: Map[A, Double], lambda: Double) = {
    counts.mapVals(_ + lambda)
  }

  def rebalance[A](counts: Map[A, Double], prior: Map[A, Double]) = {
    val adjusted = counts.transform { case (a, prob) => prob / prior(a) }
    val adjustedTotal = adjusted.unorderedFold
    adjusted.mapVals(_ / adjustedTotal)
  }

  def makeTransform[F[_]: UnorderedFoldable, A](
    headProbabilityMass: Double, priorSmoothingLambda: Double, priorTruncationHead: Double = 1.0
  )(dists: F[Map[A, Double]]): Map[A, Double] => Map[A, Double] = {
    require(priorTruncationHead >= headProbabilityMass)
    val prior = addLambda(dists.unorderedFoldMap(truncate(_, priorTruncationHead)), priorSmoothingLambda)
    (dist: Map[A, Double]) => rebalance(truncate(dist, headProbabilityMass), prior)
  }
}
