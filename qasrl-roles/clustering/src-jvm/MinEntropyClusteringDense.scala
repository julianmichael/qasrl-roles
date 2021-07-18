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
object MinEntropyClusteringDense {
  case class ClusterMixture(counts: DenseVector[Float], total: Float)
}
// Doesn't allow for flat clustering, because assigns infinite loss to zero-probability items.
class MinEntropyClusteringDense[I](
  getInstance: I => DenseVector[Float],
  vocabSize: Int
) extends AgglomerativeClusteringAlgorithm {
  import MinEntropyClusteringDense._
  type ClusterParam = ClusterMixture
  type Index = I

  def getSingleInstanceParameter(
    index: Index
  ): ClusterParam = {
    val vec = getInstance(index)
    ClusterMixture(vec, sum(vec))
  }

  // loss is entropy * num elements (same as likelihood under MLE in our case)
  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    -1.0 * sum(
      getInstance(index) *:* log(param.counts /:/ param.total)
    ) // no entries should be 0. hope we don't get NaNs
    // expensive nan check. need right for tfidf.
    // param.counts.activeValuesIterator
    //   .filter(_ > 0.0) // prevent log of 0
    //   .map(c => c * log(c / param.total)) // count * log probability = log likelihood
    //   .sum * -1.0
  }

  // just get MLE by counting
  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    val param = DenseVector.zeros[Float](vocabSize)
    var total = 0.0f
    indices.iterator
      .zip(assignmentProbabilities.iterator)
      .filter(_._2 > 0.0) // ignore zero weights
      .foreach { case (index, prob) =>
        val pcounts = getInstance(index) *:* prob.toFloat
        param :+= pcounts
        total = total + sum(pcounts)
      }
    ClusterMixture(param, total)
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
      -1.0 * sum(
        param.counts *:* log(param.counts /:/ param.total)
      ) // no entries should be 0. hope we don't get NaNs

      // expensive(?) nan check. need for tfidf.
      // param.counts.activeValuesIterator
      //   .filter(_ > 0.0) // prevent log of 0
      //   .map(c => c * log(c / param.total)) // count * log probability = log likelihood
      //   .sum * -1.0
    }
  )
}
