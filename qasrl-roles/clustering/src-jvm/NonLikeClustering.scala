package qasrl.roles.clustering

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import breeze.linalg._
import breeze.numerics._

import scala.collection.immutable.Vector

class NonLikeClustering[I, A](
  getInstance: I => A
) extends ClusteringAlgorithm {
  import MinEntropyClusteringSparse._
  type ClusterParam = Map[A, Double]
  type Index = I

  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    param.get(getInstance(index))
      .map(_ - 1.0)
      .filter(_ > 0.0)
      .fold(0.0)(_ / 2)
  }

  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    indices.zip(assignmentProbabilities).foldMap { case (i, prob) =>
      Map(getInstance(i) -> prob)
    }
  }

  // could maybe make more efficient by copying code
  override def estimateParameterHard(
    indices: Vector[Index]
  ): ClusterParam = {
    indices.foldMap(i => Map(getInstance(i) -> 1.0))
  }

  // can do efficient merge by summing counts
  override val mergeParamsEfficient = Some(
    (left: ClusterParam, right: ClusterParam) => {
      left |+| right
    }
  )

  override val mergeLossEfficient = Some(
    (left: ClusterParam, right: ClusterParam) => {
      val param = mergeParamsEfficient.get(left, right)
      param.unorderedFoldMap(pc => scala.math.max(0.0, pc * (pc - 1) / 2))
    }
  )
}
