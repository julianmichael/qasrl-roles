package qasrl.roles.clustering

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import scala.collection.immutable.Vector

trait ClusteringAlgorithm extends AgglomerativeClusteringAlgorithm with FlatClusteringAlgorithm {
  type ClusterParam
  type Index

  // default to flat clustering algorithm's method, which should be at least as efficient
  // override for efficiency
  override def estimateParameterHard(
    indices: Vector[Index]
  ): ClusterParam = {
    estimateParameterSoft(indices, indices.as(1.0))
  }

  override def mergeParamsFallback(
    left: MergeTree[Index],
    leftParam: ClusterParam,
    right: MergeTree[Index],
    rightParam: ClusterParam
  ): ClusterParam = {
    val indices = left.values ++ right.values
    val param = estimateParameterHard(indices)
    param
  }
}

// trait CompositeClusteringAlgorithm extends ClusteringAlgorithm {
//   val _1: ClusteringAlgorithm
//   val _2: ClusteringAlgorithm
//   type ClusterParam = (_1.ClusterParam, _2.ClusterParam)
//   type Instance = (_1.Instance, _2.Instance)
//   case class Hyperparams(__1: _1.Hyperparams, __2: _2.Hyperparams, lambda: Double)

//   def computeLoss(
//     instance: Instance,
//     param: ClusterParam,
//     hyperparams: Hyperparams
//   ): Double = {
//     (hyperparams.lambda * _1.computeLoss(instance._1, param._1, hyperparams.__1)) +
//       ((1.0 - hyperparams.lambda) * _2.computeLoss(instance._2, param._2, hyperparams.__2))
//   }

//   // for both latent-variable (m-step) and agglomerative clustering
//   def estimateParameter(
//     instances: Vector[Instance],
//     assignmentProbabilities: Vector[Double],
//     hyperparams: Hyperparams
//   ): ClusterParam = {
//     (_1.estimateParameter(instances.map(_._1), assignmentProbabilities, hyperparams.__1),
//      _2.estimateParameter(instances.map(_._2), assignmentProbabilities, hyperparams.__2))
//   }

//   override def getSingleInstanceParameter(
//     instance: Instance,
//     hyperparams: Hyperparams
//   ): ClusterParam = (
//     _1.getSingleInstanceParameter(instance._1, hyperparams.__1),
//     _2.getSingleInstanceParameter(instance._2, hyperparams.__2),
//   )

//   override def estimateParameterHard(
//     instances: Vector[Instance],
//     hyperparams: Hyperparams
//   ): ClusterParam = {
//     (_1.estimateParameterHard(instances.map(_._1), hyperparams.__1),
//      _2.estimateParameterHard(instances.map(_._2), hyperparams.__2))
//   }

//   // should be overridden for efficiency, if possible
//   // TODO only works with summing aggregateLosses and delta
//   override def mergeParams(
//     instances: Vector[Instance],
//     left: MergeTree[Int],
//     leftParam: ClusterParam,
//     right: MergeTree[Int],
//     rightParam: ClusterParam,
//     hyperparams: Hyperparams
//   ): ClusterParam = {
//     val leftMerge = _1.mergeParams(instances.map(_._1), left, leftParam._1, right, rightParam._1, hyperparams.__1)
//     val rightMerge = _2.mergeParams(instances.map(_._2), left, leftParam._2, right, rightParam._2, hyperparams.__2)
//     (leftMerge, rightMerge)
//   }

//   override def mergeLoss(
//     instances: Vector[Instance],
//     left: MergeTree[Int],
//     leftParam: ClusterParam,
//     right: MergeTree[Int],
//     rightParam: ClusterParam,
//     hyperparams: Hyperparams,
//     sanityCheck: Boolean = true
//   ): Double = {
//     val param = mergeParams(instances, left, leftParam, right, rightParam, hyperparams)
//     val newLoss = (left.values ++ right.values).foldMap(i => computeLoss(instances(i), param, hyperparams))
//     if(sanityCheck && !(newLoss >= left.loss && newLoss >= right.loss)) {
//       println("WARNING: clusters seem to be incorrectly merged")
//       println(left)
//       println(right)
//       println(newLoss)
//       ???
//     }
//     newLoss
//   }
// }
