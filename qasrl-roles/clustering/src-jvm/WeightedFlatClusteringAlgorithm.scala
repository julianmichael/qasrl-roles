package qasrl.roles.clustering

// derp. this is where I forget the types. I wonder if there's a nicer way
object WeightedFlatClusteringAlgorithm {
  case class Component[I](
    algorithm: FlatClusteringAlgorithm { type Index = I },
    lambda: Double,
    index: Int)
}

class WeightedFlatClusteringAlgorithm[I](
  terms: Vector[(FlatClusteringAlgorithm { type Index = I }, Double)]
) extends FlatClusteringAlgorithm {
  type ClusterParam = Vector[Any]
  type Index = I

  val components = terms.zipWithIndex.map { case ((alg, lambda), index) =>
    WeightedFlatClusteringAlgorithm.Component(alg, lambda, index)
  }

  override def getSingleInstanceParameter(
    index: Index
  ): ClusterParam = {
    components.map(_.algorithm.getSingleInstanceParameter(index))
  }

  override def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double = {
    components.map(
      c => c.algorithm.getInstanceLoss(index, param(c.index).asInstanceOf[c.algorithm.ClusterParam]) * c.lambda
    ).sum
  }

  override def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double]
  ): ClusterParam = {
    components.map(c => c.algorithm.estimateParameterSoft(indices, assignmentProbabilities))
  }

  override def estimateParameterHard(
    indices: Vector[Index]
  ): ClusterParam = {
    components.map(c => c.algorithm.estimateParameterHard(indices))
  }
}
