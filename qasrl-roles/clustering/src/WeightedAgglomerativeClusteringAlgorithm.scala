package qasrl.roles.clustering

import cats.implicits._

// derp. this is where I forget the types. I wonder if there's a nicer way
object WeightedAgglomerativeClusteringAlgorithm {
  case class Component[I](
    algorithm: AgglomerativeClusteringAlgorithm { type Index = I },
    lambda: Double,
    index: Int)
}

class WeightedAgglomerativeClusteringAlgorithm[I](
  terms: Vector[(AgglomerativeClusteringAlgorithm { type Index = I }, Double)]
) extends AgglomerativeClusteringAlgorithm {
  type ClusterParam = Vector[Any]
  type Index = I

  val components = terms.zipWithIndex.map { case ((alg, lambda), index) =>
    WeightedAgglomerativeClusteringAlgorithm.Component(alg, lambda, index)
  }

  override def getSingleInstanceParameter(
    index: Index
  ): ClusterParam = {
    components.map(_.algorithm.getSingleInstanceParameter(index))
  }

  override def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    components.map(c =>
      c.algorithm.getInstanceLoss(
        index,
        param(c.index).asInstanceOf[c.algorithm.ClusterParam]
      ) * c.lambda
    ).sum
  }

  override def estimateParameterHard(
    indices: Vector[Index]
  ): ClusterParam = {
    components.map(c => c.algorithm.estimateParameterHard(indices))
  }

  override def mergeParamsEfficient: Option[(ClusterParam, ClusterParam) => ClusterParam] =
    components.traverse(_.algorithm.mergeParamsEfficient).map(_ =>
      (x: ClusterParam, y: ClusterParam) => (components, x, y).zipped
        .map { case (c, l, r) =>
          c.algorithm.mergeParamsEfficient.get(
            l.asInstanceOf[c.algorithm.ClusterParam],
            r.asInstanceOf[c.algorithm.ClusterParam]
          )
        }.toVector
    )

  override def mergeLossEfficient: Option[(ClusterParam, ClusterParam) => Double] =
    components.traverse(_.algorithm.mergeParamsEfficient).map(_ =>
      (x: ClusterParam, y: ClusterParam) => 
        (components, x, y).zipped.map { case (c, l, r) =>
          c.algorithm.mergeLossEfficient.get(
            l.asInstanceOf[c.algorithm.ClusterParam],
            r.asInstanceOf[c.algorithm.ClusterParam]
          ) * c.lambda
        }.sum
    )

  override def mergeParamsFallback(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): ClusterParam = {
    (components, leftParam, rightParam).zipped.map { case (c, lParam, rParam) =>
      c.algorithm.mergeParams(
        left, lParam.asInstanceOf[c.algorithm.ClusterParam],
        right, rParam.asInstanceOf[c.algorithm.ClusterParam]
      )
    }.toVector
  }

  override def mergeLossFallback(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): Double = {
    (components, leftParam, rightParam).zipped.map { case (c, lParam, rParam) =>
      c.algorithm.mergeLoss(
        left, lParam.asInstanceOf[c.algorithm.ClusterParam],
        right, rParam.asInstanceOf[c.algorithm.ClusterParam]
      ) * c.lambda
    }.sum
  }
}
