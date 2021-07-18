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

// This can't be used for agglomerative clustering in my setup,
// because the loss of a combined cluster can be less than the summed losses of the two,
// breaking monotonicity thanks to the smoothing and concentration effects of the Dirichlet prior.
// In particular, combining identical indices will reduce the total loss
// (instead of keeping it constant, as with MinEntropyClustering).
class DirichletMAPClusteringSparse[I](
  getInstance: I => Map[Int, Double],
  vocabSize: Int,
  clusterConcentrationParameter: Double
) extends FlatClusteringAlgorithm {

  type Index = I
  type ClusterParam = DenseMultinomial

  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    getInstance(index).iterator.map { case (item, pcount) =>
      math.log(param.probabilityOf(item)) * pcount
    }.sum * -1
  }

  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    val pseudoCounts = indices.iterator
      .zip(assignmentProbabilities.iterator)
      .filter(_._2 > 0.0) // ignore zero weights
      .map { case (index, prob) =>
        getInstance(index).transform { case (_, c) => prob * c } // expected counts
      }.foldLeft(Map.empty[Int, Double])(_ |+| _)
    dirichletPosteriorFromSparseNew(
      pseudoCounts, vocabSize, clusterConcentrationParameter
    )
  }
}
