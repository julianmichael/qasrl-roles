package qasrl.roles.clustering

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.collection.immutable.Vector

class AgglomerativeSetClustering[I, P](
  innerAlg: AgglomerativeClusteringAlgorithm { type Index = I; type ClusterParam = P }
) extends AgglomerativeClusteringAlgorithm {
  type Index = Set[I]
  type ClusterParam = P

  override def getLossChangePriority(
    newLoss: Double,
    leftLoss: Double,
    leftParam: ClusterParam,
    rightLoss: Double,
    rightParam: ClusterParam
  ) = {
    innerAlg.getLossChangePriority(newLoss, leftLoss, leftParam, rightLoss, rightParam)
  }

  def getSingleInstanceParameter(
    index: Index
  ): ClusterParam = {
    innerAlg.estimateParameterHard(index.toVector)
  }

  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    aggregateLosses(index.toVector.map(innerAlg.getInstanceLoss(_, param)))
  }

  // def estimateParameterSoft(
  //   indices: Vector[Index],
  //   assignmentProbabilities: Vector[Double]
  // ): ClusterParam = {
  //   val (allIndices, allAssignmentProbabilities) = {
  //     indices.zip(assignmentProbabilities).flatMap { case (index, prob) =>
  //       index.toVector.map(_ -> prob)
  //     }.unzip
  //   }
  //   innerAlg.estimateParameterSoft(allIndices, allAssignmentProbabilities)
  // }

  override def estimateParameterHard(
    indices: Vector[Index]
  ): ClusterParam = {
    innerAlg.estimateParameterHard(indices.flatten)
  }

  override val mergeParamsEfficient: Option[
    (ClusterParam, ClusterParam) => ClusterParam
  ] = innerAlg.mergeParamsEfficient

  override def mergeParamsFallback(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): ClusterParam = {
    val indices = left.unorderedFold ++ right.unorderedFold
    val param = innerAlg.estimateParameterHard(indices.toVector)
    param
  }

  override val mergeLossEfficient: Option[
    (ClusterParam, ClusterParam) => Double
  ] = innerAlg.mergeLossEfficient

  override def mergeLossFallback(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): Double = {
    val indices = left.unorderedFold ++ right.unorderedFold
    val param = mergeParams(left, leftParam, right, rightParam)
    aggregateLosses(
      indices.toVector.map(innerAlg.getInstanceLoss(_, param))
    )
  }
}
