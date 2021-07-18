package qasrl.roles.clustering

import cats.data.NonEmptyVector
import cats.implicits._

trait CompositeFlatClusteringAlgorithm extends FlatClusteringAlgorithm {
  val _1: FlatClusteringAlgorithm
  val _1Lambda: Double
  val _2: FlatClusteringAlgorithm { type Index = _1.Index }
  val _2Lambda: Double
  type ClusterParam = (_1.ClusterParam, _2.ClusterParam)
  type Index = _1.Index

  override def getSingleInstanceParameter(
    index: Index
  ): ClusterParam = {
    _1.getSingleInstanceParameter(index) ->
      _2.getSingleInstanceParameter(index)
  }

  override def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    (_1Lambda * _1.getInstanceLoss(index, param._1)) +
      (_2Lambda * _2.getInstanceLoss(index, param._2))
  }

  override def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = (
    _1.estimateParameterSoft(indices, assignmentProbabilities),
    _2.estimateParameterSoft(indices, assignmentProbabilities)
  )

  override def estimateParameterHard(
    indices: Vector[Index]
  ): ClusterParam = (
    _1.estimateParameterHard(indices),
    _2.estimateParameterHard(indices)
  )
}
