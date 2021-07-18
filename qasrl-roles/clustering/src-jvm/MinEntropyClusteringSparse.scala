package qasrl.roles.clustering

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import breeze.linalg._
import breeze.numerics._

import scala.collection.immutable.Vector

// same as DirichletMAPClustering but only does MLE (no smoothing) and agglomeration
// can also be regarded as "MLE clustering"
object MinEntropyClusteringSparse {
  case class ClusterMixture(counts: DenseVector[Double], total: Double)
}
// TODO maybe sparse vectors ... or dense, since they need to be constructed for cluster params anyway? or maybe that would just use way too much memory
// Doesn't allow for flat clustering, because assigns infinite loss to zero-probability items.
class MinEntropyClusteringSparse[I](
  getInstance: I => Map[Int, Double],
  vocabSize: Int
) extends AgglomerativeClusteringAlgorithm {
  import MinEntropyClusteringSparse._
  type ClusterParam = ClusterMixture
  type Index = I

  // override def getLossChangePriority(
  //   newLoss: Double,
  //   leftLoss: Double,
  //   leftParam: ClusterParam,
  //   rightLoss: Double,
  //   rightParam: ClusterParam
  // ) = {
  //   require(newLoss >= leftLoss + rightLoss) // likelihood should worsen
  //   newLoss / (leftParam.total + rightParam.total) // just use entropy of new cluster
  // }

  def getSingleInstanceParameter(
    index: Index
  ): ClusterParam = {
    val arr = new Array[Double](vocabSize)
    var total = 0.0
    getInstance(index).foreach { case (index, pcount) =>
      arr(index) += pcount
      total = total + pcount
    }
    ClusterMixture(DenseVector(arr), total)
  }

  // loss is entropy * num elements (same as likelihood under MLE in our case)
  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    getInstance(index).iterator.map { case (item, count) =>
      math.log(param.counts(item) / param.total) * count
    }.sum * -1.0
  }

  // just get MLE by counting
  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    val arr = new Array[Double](vocabSize)
    var total = 0.0
    var numInstances = 0.0
    indices.iterator
      .zip(assignmentProbabilities.iterator)
      .filter(_._2 > 0.0) // ignore zero weights
      .foreach { case (index, prob) =>
        numInstances = numInstances + prob
        getInstance(index).foreach { case (index, count) =>
          val pcount = prob * count
          arr(index) += pcount
          total = total + pcount
        }
      }
    ClusterMixture(DenseVector(arr), total)
  }

  // could maybe make more efficient by copying code
  override def estimateParameterHard(
    indices: Vector[Index],
    ): ClusterParam = {
    estimateParameterSoft(indices, indices.as(1.0))
  }

  // can do efficient merge by summing counts
  override val mergeParamsEfficient = Some(
    (left: ClusterParam, right: ClusterParam) => {
      val counts = left.counts + right.counts
      val total = left.total + right.total
      ClusterMixture(counts, total)
    }
  )

  override val mergeLossEfficient = Some(
    (left: ClusterParam, right: ClusterParam) => {
      val param = mergeParamsEfficient.get(left, right)
      param.counts.activeValuesIterator
        .filter(_ > 0.0) // prevent log of 0
        .map(c => c * log(c / param.total)) // count * log probability = log likelihood
        .sum * -1.0
    }
  )
}
