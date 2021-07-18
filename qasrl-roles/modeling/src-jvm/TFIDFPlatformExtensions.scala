package qasrl.roles.modeling

import breeze.linalg.DenseVector
import cats.Foldable
import cats.implicits._
import breeze.linalg.sum

trait TFIDFPlatformExtensions {
  object Dense {

    def truncate[A](counts: DenseVector[Float], headProportion: Float): DenseVector[Float] = {
      val total = sum(counts)
      val headSize = total * headProportion
      val itemsDec = counts.toScalaVector.sortBy(-_).toList
      def getBottomThreshold(items: List[Float], acc: Float): Float = items match {
        case Nil => 0.0f
        case x :: xs =>
          if(acc >= headSize) x else {
            getBottomThreshold(xs, acc + x)
          }
      }
      val threshold = getBottomThreshold(itemsDec, 0.0f)
      counts.map(v => if(v <= threshold) 0.0f else v)
      // counts.filter(p => headItems.contains(p._1))
    }

    def addLambda[A](counts: DenseVector[Float], lambda: Float): DenseVector[Float] = {
      counts + DenseVector.fill[Float](counts.size, lambda)
    }

    def rebalance[A](counts: DenseVector[Float], prior: DenseVector[Float]): DenseVector[Float] = {
      val adjusted = (counts /:/ prior).map(x => if(x.isNaN) 0.0f else x)
      val adjustedTotal = sum(adjusted)
      (adjusted / adjustedTotal).map(x => if(x.isNaN) 0.0f else x)
    }

    def makeTransform[F[_]: Foldable, A](
      headProbabilityMass: Float, priorSmoothingLambda: Float, priorTruncationHead: Float = 1.0f
    )(dists: F[DenseVector[Float]]): DenseVector[Float] => DenseVector[Float] = {
      require(priorTruncationHead >= headProbabilityMass)
      val prior = addLambda(
        dists.toList.map(truncate(_, priorTruncationHead)).reduce(_ + _),
        priorSmoothingLambda
      )

      (dist: DenseVector[Float]) => rebalance(truncate(dist, headProbabilityMass), prior)
    }
  }
}
