package qasrl.roles.clustering

import cats.implicits._

trait CompositeAgglomerativeClusteringAlgorithm extends AgglomerativeClusteringAlgorithm {
  val _1: AgglomerativeClusteringAlgorithm
  val _1Lambda: Double
  val _2: AgglomerativeClusteringAlgorithm { type Index = _1.Index }
  val _2Lambda: Double
  type ClusterParam = (_1.ClusterParam, _2.ClusterParam)
  type Index = _1.Index

  def getSingleInstanceParameter(
    index: Index
  ): ClusterParam = {
    _1.getSingleInstanceParameter(index) ->
      _2.getSingleInstanceParameter(index)
  }

  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    (_1Lambda * _1.getInstanceLoss(index, param._1)) +
      (_2Lambda * _2.getInstanceLoss(index, param._2))
  }

  override def mergeParamsEfficient: Option[(ClusterParam, ClusterParam) => ClusterParam] =
    (_1.mergeParamsEfficient, _2.mergeParamsEfficient).mapN((f1, f2) =>
      (x: ClusterParam, y: ClusterParam) => (f1(x._1, y._1), f2(x._2, y._2))
    )

  override def mergeLossEfficient: Option[(ClusterParam, ClusterParam) => Double] =
    (_1.mergeLossEfficient, _2.mergeLossEfficient).mapN((f1, f2) =>
      (x: ClusterParam, y: ClusterParam) => f1(x._1, y._1) + f2(x._2, y._2)
    )

  override def mergeParamsFallback(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): ClusterParam = {
    _1.mergeParams(left, leftParam._1, right, rightParam._1) ->
      _2.mergeParams(left, leftParam._2, right, rightParam._2)
  }

  override def mergeLossFallback(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): Double = {
    (_1Lambda * _1.mergeLoss(left, leftParam._1, right, rightParam._1)) +
      (_2Lambda * _2.mergeLoss(left, leftParam._2, right, rightParam._2))
  }
}
