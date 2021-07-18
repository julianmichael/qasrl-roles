package qasrl.roles.clustering

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.Multinomial

import scala.collection.immutable.Vector

object VectorMeanClustering {
  case class ClusterMean(mean: DenseVector[Float], size: Double)
}
class VectorMeanClustering[I](
  getInstance: I => DenseVector[Float]
) extends ClusteringAlgorithm {
  import VectorMeanClustering.ClusterMean
  // cluster means keep track of size too so they can be added and for loss calculation
  type ClusterParam = ClusterMean
  type Index = I

  // k-means loss: sum of square distances from mean
  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    val displacement = getInstance(index) - param.mean
    (displacement dot displacement).toDouble / 175.0 // adjust so interpolation is reasonable
  }

  override def getSingleInstanceParameter(
    index: Index
  ): ClusterParam = ClusterMean(getInstance(index), 1)

  // just take the mean of all of the elements in the cluster (in expectation)
  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    val size = assignmentProbabilities.sum
    val mean = DenseVector.zeros[Float](getInstance(indices.head).length)
    indices.iterator.zip(assignmentProbabilities.iterator)
      .foreach { case (index, prob) =>
        mean :+= (getInstance(index) *:* prob.toFloat)
      }
    mean :/= size.toFloat
    ClusterMean(mean, size)
  }

  override def estimateParameterHard(
    indices: Vector[Index]
  ): ClusterParam = {
    val size = indices.size
    val mean = DenseVector.zeros[Float](getInstance(indices.head).length)
    indices.iterator.foreach { index =>
        mean :+= getInstance(index)
      }
    mean :/= size.toFloat
    ClusterMean(mean, size)
  }

  // can efficiently merge by weighing each mean by its cluster size
  override val mergeParamsEfficient = Some(
    (left: ClusterParam, right: ClusterParam) => {
      val size = left.size + right.size
      val mean = (left.mean *:* (left.size.toFloat / size).toFloat) +
        (right.mean *:* (right.size / size).toFloat)
      ClusterMean(mean, size)
    }
  )
}
