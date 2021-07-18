package qasrl.roles.clustering

object MaxPriorEntropyClustering {
  // case class ClusterMixture(counts: DenseVector[Float], total: Float)
}
// Doesn't allow for flat clustering, because assigns infinite loss to zero-probability items.
class MaxPriorEntropyClustering[I](
  numItems: Int
) extends AgglomerativeClusteringAlgorithm {
  // import MinEntropyClusteringDense._
  type ClusterParam = Int
  type Index = I

  def getSingleInstanceParameter(
    index: Index
  ): ClusterParam = 1

  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    scala.math.log(param.toDouble / numItems)
  }

  // just get MLE by counting
  // def estimateParameterSoft(
  //   indices: Vector[Index],
  //   assignmentProbabilities: Vector[Double]
  // ): ClusterParam = {
  //   val param = DenseVector.zeros[Float](vocabSize)
  //   var total = 0.0f
  //   indices.iterator
  //     .zip(assignmentProbabilities.iterator)
  //     .filter(_._2 > 0.0) // ignore zero weights
  //     .foreach { case (index, prob) =>
  //       val pcounts = getInstance(index) *:* prob.toFloat
  //       param :+= pcounts
  //       total = total + sum(pcounts)
  //     }
  //   ClusterMixture(param, total)
  // }

  // could maybe make more efficient by copying code
  override def estimateParameterHard(
    indices: Vector[Index]
  ): ClusterParam = {
    indices.size
  }

  // can do efficient merge by summing counts
  override val mergeParamsEfficient = Some(
    (left: ClusterParam, right: ClusterParam) => {
      left + right
    }
  )

  override val mergeLossEfficient = Some(
    (left: ClusterParam, right: ClusterParam) => {
      (left + right) * scala.math.log((left + right).toDouble / numItems)
    }
  )
}
