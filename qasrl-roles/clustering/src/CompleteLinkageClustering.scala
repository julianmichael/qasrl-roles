package qasrl.roles.clustering

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import scala.collection.immutable.Vector

class CompleteLinkageClustering(
  distances: Array[Array[Double]] // fuzzy eq neg log probs
) extends AgglomerativeClusteringAlgorithm {
  type ClusterParam = Set[Int] // DenseVector[Double] // boolean sets
  type Index = Int // DenseVector[Double] // fuzzy eq neg log probs
  // case class Hyperparams(vocabSize: Int)

  // loss of a cluster is max distance between any two elements
  def getInstanceLoss(
    index: Index,
    param: ClusterParam,
  ): Double = {
    param.map(i => distances(i)(index)).max
  }

  def getSingleInstanceParameter(
    index: Int,
  ): ClusterParam = {
    Set(index)
  }

  override val mergeParamsEfficient = Some(
    (left: ClusterParam, right: ClusterParam) => {
      left union right
    }
  )

  // only need to consider pairs that cross between clusters to find the new max pairwise distance
  override def mergeLossFallback(
    left: MergeTree[Int],
    leftParam: ClusterParam,
    right: MergeTree[Int],
    rightParam: ClusterParam
  ): Double = {
    val leftValues = left.values
    val rightValues = right.values
    val param = (leftValues ++ rightValues).toSet
    val newLoss = leftParam.iterator.flatMap { lv =>
      rightParam.iterator.map { rv =>
        distances(lv)(rv)
      }
    }.max
    val loss = List(newLoss, left.loss, right.loss).max
    if(!(newLoss >= left.loss && newLoss >= right.loss)) {
      println("WARNING: clusters seem to be incorrectly merged")
      println(left)
      println(right)
      println(newLoss)
      ???
    }
    loss
  }

  override def getLossChangePriority(
    newLoss: Double,
    leftLoss: Double,
    leftParam: ClusterParam,
    rightLoss: Double,
    rightParam: ClusterParam
  ) = {
    require(newLoss >= leftLoss)
    require(newLoss >= rightLoss)
    newLoss
  }

}
