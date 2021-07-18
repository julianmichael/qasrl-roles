package qasrl.roles.clustering

import jjm.implicits._

import cats.Foldable
import cats.data.NonEmptyVector
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import scala.collection.immutable.Vector
import scala.annotation.tailrec

import breeze.stats.distributions.Multinomial
import breeze.linalg._
import breeze.numerics._
import freelog.EphemeralTreeLogger

class JointFlatClusteringAlgorithm[I, InnerIndex, InnerParam](
  val innerAlgorithm: FlatClusteringAlgorithm { type Index = InnerIndex; type ClusterParam = InnerParam },
  getSubInstances: I => Vector[InnerIndex],
  numInnerClusters: Int,
  innerHardEMStoppingDelta: Double
  // getLossPenalty: Int => Double // should grow monotonically
) extends FlatClusteringAlgorithm {
  type Index = I
  type ClusterParam = Vector[(Double, InnerParam)]

  val estimateClusterPrior: (Map[Int, Int], Int) => DenseMultinomial =
    (_, numClusters) => Multinomial(DenseVector.ones(numClusters))

  // def estimateInnerClusters(
  //   innerIndices: Vector[InnerIndex],
  // )

  // override for efficiency: just randomly choose clusters
  override def getSingleInstanceParameter(
    index: Index,
  ): ClusterParam = {
    val subInstances = getSubInstances(index)
    val numClusters = math.min(numInnerClusters, subInstances.size)
    scala.util.Random.shuffle(subInstances).take(numClusters).map { i =>
      val param = innerAlgorithm.getSingleInstanceParameter(i)
      val loss = innerAlgorithm.getInstanceLoss(i, param)
      (1.0 / numClusters) -> param
    }
  }

  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    getSubInstances(index).foldMap(innerIndex =>
      param.foldMap { case (prob, param) =>
        prob * innerAlgorithm.getInstanceLoss(innerIndex, param)
      }
    )
  }

  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    ???
    // TODO add instance probabilities in some sensible way to the global EM parameters.
    // because here we want to run a nested EM but with some instances down-weighted according to the probabilities
    // computed in the prior E step.
  }

  // override for efficiency
  override def estimateParameterHard(
    indices: Vector[Index],
  ): ClusterParam = {
    val innerIndices = indices.flatMap(i => getSubInstances(i))
    val initModel = innerAlgorithm.initPlusPlus(innerIndices, numInnerClusters)
    val (clusters, assignments, loss) = innerAlgorithm.runHardEM(
      initModel,
      innerIndices,
      stoppingThreshold = innerHardEMStoppingDelta,
      estimateClusterPrior = estimateClusterPrior
    )(EphemeralTreeLogger.noop[cats.effect.IO, String]).unsafeRunSync()
    // )(qasrl.roles.modeling.loggerUnsafe).unsafeRunSync()
    val prior = estimateClusterPrior(assignments.counts, numInnerClusters)
    val zippedRes = prior.params.toScalaVector.zip(clusters)
    zippedRes
  }
}
