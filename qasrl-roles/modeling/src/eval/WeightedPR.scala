package qasrl.roles.modeling.eval

import cats.Monoid
import jjm.metrics.Functions
import jjm.metrics.HasMetrics
import jjm.metrics.MapTree
import jjm.metrics.Metric
import jjm.metrics.WeightedNumbers

case class WeightedPR(
  precisions: WeightedNumbers[Double],
  recalls: WeightedNumbers[Double]
) {
  def pseudocount = precisions.stats.pseudocount
  def precision = precisions.stats.weightedMean
  def recall = recalls.stats.weightedMean
  def f1 = Functions.harmonicMean(precision, recall)
  def prf = {
    val p = precision
    val r = recall
    val f1 = Functions.harmonicMean(p, r)
    (p, r, f1)
  }
  def fMeasure(beta: Double) = Functions.weightedHarmonicMean(beta, precision, recall)
  def normalize = WeightedPR(precisions.normalize, recalls.normalize)
}
object WeightedPR {
  implicit val weightedPRMonoid: Monoid[WeightedPR] = {
    new Monoid[WeightedPR] {
      def empty: WeightedPR = WeightedPR(WeightedNumbers(Vector()), WeightedNumbers(Vector()))
      def combine(x: WeightedPR, y: WeightedPR): WeightedPR = WeightedPR(
        Monoid[WeightedNumbers[Double]].combine(
          x.precisions, y.precisions
        ),
        Monoid[WeightedNumbers[Double]].combine(
          x.recalls, y.recalls
        )
      )
    }
  }
  implicit val weightedPRHasMetrics = new HasMetrics[WeightedPR] {
    def getMetrics(pr: WeightedPR) = MapTree.fromPairs(
      // "pseudocount" -> Metric.double(pr.pseudocount),
      "precision" -> Metric.double(pr.precision),
      "recall" -> Metric.double(pr.recall),
      "f1" -> Metric.double(pr.f1)
    )
  }
}
