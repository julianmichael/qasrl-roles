package qasrl.roles.modeling

import cats.Foldable
import cats.implicits._

import scala.util.Random

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.stats.distributions.Multinomial
import scala.collection.immutable.Vector

trait PackagePlatformExtensions {

  case class ClusterMean[A](mean: DenseVector[A], total: Double)

  type DenseMultinomial = Multinomial[DenseVector[Double], Int]

  def mean[F[_]: Foldable](fa: F[Double]) = {
    fa.combineAll / fa.size
  }

  // assume normalized
  def sample[F[_]: Foldable](dist: F[Double], rand: Random): Int = {
    dist.toList.zipWithIndex.foldM[Either[Int, *], Double](rand.nextDouble) {
      case (mass, (prob, index)) =>
        if(mass <= prob) Left(index)
        else Right(mass - prob)
    }.left.get // will always be present for prob dist
  }

  def logSumExp(args: DenseVector[Double]): Double = {
    val maxElement = max(args)
    val sumExp = sum(exp(args - maxElement))
    log(sumExp) + maxElement
  }

  def logSumExp[F[_]: Foldable](args: F[Double]): Double = {
    val max = args.maximumOption.getOrElse(0.0)
    val sumExp = args.foldMap(x => scala.math.exp(x - max))
    max + scala.math.log(sumExp)
  }

  def uniform(supportSize: Int) = {
    Vector.fill(supportSize)(1.0 / supportSize)
  }

  // alpha is TOTAL of prior counts
  def dirichletPosteriorFromDense[A](pseudoCounts: Vector[Double], alpha: Double): Vector[Double] = {
    val priorCount = alpha / pseudoCounts.size
    val normalization = pseudoCounts.sum + alpha
    pseudoCounts.map(pc => (pc + priorCount) / normalization)
  }

  def dirichletPosteriorFromDense[A](pseudoCounts: DenseVector[Double], alpha: Double): DenseMultinomial = {
    val priorCount = alpha / pseudoCounts.size
    Multinomial(pseudoCounts + priorCount)
  }

  def dirichletPosteriorFromDenseFloat[A](pseudoCounts: DenseVector[Float], alpha: Float): DenseVector[Float] = {
    val priorCounts = DenseVector.fill[Float](pseudoCounts.size, alpha / pseudoCounts.size)
    val normalizer = sum(pseudoCounts) + alpha
    (pseudoCounts + priorCounts) /:/ normalizer
  }

  // alpha is TOTAL of prior counts
  def dirichletPosteriorFromSparse[A](pseudoCounts: Map[Int, A], supportSize: Int, alpha: Double)(implicit N: Numeric[A]) = {
    val normalization = N.toDouble(pseudoCounts.values.sum) + alpha
    val priorCount = alpha / supportSize
    pseudoCounts.foldLeft(Vector.fill(supportSize)(priorCount / normalization)) {
      case (vec, (idx, count)) =>
        vec.updated(idx, (priorCount + N.toDouble(count)) / normalization)
    }
  }

  // alpha is TOTAL of prior counts
  def dirichletPosteriorFromSparseNew[A](pseudoCounts: Map[Int, A], supportSize: Int, alpha: Double)(implicit N: Numeric[A]) = {
    val priorCount = alpha / supportSize
    Multinomial(
      DenseVector.tabulate[Double](supportSize)(i =>
        N.toDouble(pseudoCounts.getOrElse(i, N.zero)) + priorCount
      )
    )
  }

  def invertOdds(d: DenseMultinomial): DenseMultinomial = {
    val negLogits = -log(d.params)
    val normalizer = logSumExp(negLogits)
    Multinomial(exp(negLogits - normalizer))
  }

  // import cats.effect.concurrent.Ref
  // import cats.effect.IO

  // shim for until i upgrade cats-effect version
  // implicit class RichRef[F[_], A](val ref: Ref[F, A]) extends AnyVal {
  //   def updateAndGet(f: A => A): F[A] = ref.modify { a =>
  //     val newA = f(a)
  //     (newA, newA)
  //   }
  //   def getAndUpdate(f: A => A): F[A] = ref.modify { a =>
  //     (f(a), a)
  //   }
  // }

  import scala.collection.mutable
  implicit class RichMutablePriorityQueue[A](val q: mutable.PriorityQueue[A]) {
    def dequeueOption: Option[A] = if (!q.isEmpty) Some(q.dequeue) else None
    def filterDequeue(p: A => Boolean) = {
      dequeueOption.filter(p).orElse(dequeueOption)
    }
  }
}
