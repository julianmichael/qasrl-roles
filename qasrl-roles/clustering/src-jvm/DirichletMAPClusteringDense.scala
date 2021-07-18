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

// I _think_ this can't be used for agglomerative clustering in my setup,
// because the loss of a combined cluster can be less than the summed losses of the two,
// breaking monotonicity thanks to the smoothing and concentration effects of the Dirichlet prior.
// In particular, combining identical indices will reduce the total loss
// (instead of keeping it constant, as with MinEntropyClustering).
// object DirichletMAPClusteringDense {
//   // case class ClusterMixture(probs: DenseVector[Float])
// }
class DirichletMAPClusteringDense[I](
  getInstance: I => DenseVector[Float],
  vocabSize: Int,
  clusterConcentrationParameter: Float
) extends FlatClusteringAlgorithm {

  type Index = I
  type ClusterParam = DenseVector[Float]

  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    // getInstance(index).iterator.map { case (item, pcount) =>
    //   math.log(param.probabilityOf(item)) * pcount
    // }.sum * -1
    -1.0 * sum(getInstance(index) *:* log(param)) // no entries should be 0. hope we don't get NaNs
  }

  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    val pseudoCounts = DenseVector.zeros[Float](vocabSize)
    indices.iterator
      .zip(assignmentProbabilities.iterator)
      .filter(_._2 > 0.0) // ignore zero weights
      .foreach { case (index, prob) =>
        val pcounts = getInstance(index) *:* prob.toFloat
        pseudoCounts :+= pcounts
      }

    // val pseudoCounts = indices.iterator
    //   .zip(assignmentProbabilities.iterator)
    //   .filter(_._2 > 0.0) // ignore zero weights
    //   .map { case (index, prob) =>
    //     getInstance(index).transform { case (_, c) => prob * c } // expected counts
    //   }.foldLeft(Map.empty[Int, Double])(_ |+| _)

    dirichletPosteriorFromDenseFloat(
      pseudoCounts, clusterConcentrationParameter
    )
  }
}
