package qasrl.roles.modeling.eval

import jjm.metrics.Functions

case class ConfStatsPoint(losses: Vector[Double], clusterSizes: Vector[Int], clusterWeights: Vector[Double], weightedPR: WeightedPR) {
  val loss = losses.sum
  def precision = weightedPR.precision
  def recall = weightedPR.recall
  def numClusters = clusterSizes.size
  def numItems = clusterSizes.sum
  def lossesPerItem = losses.zip(clusterSizes).map(Function.tupled(_ / _))
  def lossPerItem = loss / numItems
  def fMeasure(beta: Double) = Functions.weightedHarmonicMean(beta, precision, recall)
  def f1 = Functions.harmonicMean(precision, recall)
}
