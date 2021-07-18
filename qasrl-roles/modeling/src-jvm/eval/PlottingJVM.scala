package qasrl.roles.modeling.eval

import java.nio.file.{Path => NIOPath}

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._

import freelog.EphemeralTreeLogger
import freelog.implicits._

import jjm.metrics.Functions
import jjm.implicits._

import com.cibo.evilplot.plot.renderers.PlotElementRenderer
import com.cibo.evilplot.plot.aesthetics.Colors

import qasrl.roles.modeling.logLevel

object PlottingJVM {

  import com.cibo.evilplot._
  import com.cibo.evilplot.colors._
  import com.cibo.evilplot.geometry._
  import com.cibo.evilplot.numeric._
  import com.cibo.evilplot.plot._
  import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
  import com.cibo.evilplot.plot.aesthetics.Theme
  import com.cibo.evilplot.plot.renderers._

  def plotAllStatsVerbwise[VerbType](
    modelName: String,
    allStats: Map[VerbType, NonEmptyList[ConfStatsPoint]],
    precisionAxisLabel: String,
    recallAxisLabel: String,
    makePath: String => NIOPath)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = Log.infoBranch(s"Plotting metrics for all verbs ($precisionAxisLabel / $recallAxisLabel)") {

    case class PRPoint(
      verbType: VerbType,
      loss: Double,
      numItems: Int,
      numClusters: Int,
      isBestF1: Boolean,
      recall: Double,
      precision: Double) extends Datum2d[PRPoint] {
      val x = recall
      val y = precision
      def f1 = 2 * precision * recall / (precision + recall)

      def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
    }

    // val rand = new scala.util.Random(2643642L)
    // def noise = scala.math.abs(rand.nextGaussian / 200.0)

    val allData = allStats.transform { case (verbType, allStats) =>

      val bestF1 = allStats.map(_.f1).maximum
      allStats.map { case p @ ConfStatsPoint(losses, clusterSizes, _, pr) =>
        PRPoint(verbType, p.loss, clusterSizes.sum, clusterSizes.size, p.f1 == bestF1, pr.recall, pr.precision)
      }
    }

    val maxNumClusters = allData.values.toList.flatMap(_.toList).filter(_.isBestF1).map(_.numClusters).max
    val maxNumInstances = allData.values.toList.map(_.head.numItems).max

    val linePlot = allData.toList.map { case (verbType, data) =>
      val propMaxInstances = data.head.numItems.toDouble / maxNumInstances
      LinePlot(
		    data.toList,
        pathRenderer = Some(PathRenderer.default[PRPoint](strokeWidth = Some(0.2), color = Some(RGBA(0, 0, 0, 1.0))))
	    )
    }
    val bestF1Scatter = allData.toList.map { case (verbType, data) =>
      ScatterPlot(
		    data.toList.filter(_.isBestF1),
        pointRenderer = Some(
          PointRenderer.depthColor[PRPoint](
            depth = _.numClusters.toDouble,
            size = Some(2.0), min = 1.0, max = maxNumClusters,
            coloring = Some(ContinuousColoring.gradient(start = RGBA(0, 0, 0, 1.0), end = RGBA(255, 0, 0, 1.0)))
          )
        )
	    )
    }

    val maxNumClustersScatter = allData.toList.map { case (verbType, data) =>
      ScatterPlot(
		    data.toList.maximaBy(_.numClusters),
        pointRenderer = Some(
          PointRenderer.depthColor[PRPoint](
            depth = _.numClusters.toDouble,
            size = Some(2.0), min = 1.0, max = 5,
            coloring = Some(ContinuousColoring.gradient(start = RGBA(0, 0, 255, 1.0), end = RGBA(0, 255, 0, 1.0)))
          )
        )
	    )
    }

    val plot1 = Overlay.fromSeq(
      linePlot ++ bestF1Scatter
    ).title(s"PropBank argument clustering ($modelName)")
      .xLabel(recallAxisLabel)
      .yLabel(precisionAxisLabel)
      .xbounds(0.0, 1.0).ybounds(0.0, 1.0)
      .xAxis().yAxis()
      .frame().rightLegend()

    val plot2 = Overlay.fromSeq(
      linePlot ++ maxNumClustersScatter
    ).title(s"PropBank argument clustering ($modelName)")
      .xLabel(recallAxisLabel)
      .yLabel(precisionAxisLabel)
      .xbounds(0.0, 1.0).ybounds(0.0, 1.0)
      .xAxis().yAxis()
      .frame().rightLegend()

    // com.cibo.evilplot.JSONUtils.
    IO(plot1.render().write(new java.io.File(makePath("best").toString))) >>
      IO(plot2.render().write(new java.io.File(makePath("precise").toString)))
  }

  case class WeirdLinePoint(
    x: Double,
    y: Double,
    value: Double,
    weight: Double
  ) extends Datum2d[WeirdLinePoint] {
    def withXY(x: Double, y: Double) = this.copy(x = x, y = y)
  }

  def plotWeirdLines(
    data: List[List[WeirdLinePoint]],
    title: String,
    xAxisLabel: String,
    yAxisLabel: String,
    path: NIOPath): IO[Unit] = {
    // val colors = Color.getDefaultPaletteSeq(data.size)

    // val categories = data.keySet.toList
    // val useColoring = CategoricalColoring.themed[String]
    // val colorFunc = useColoring(categories)
    // val radius = size.getOrElse(theme.elements.pointSize)

    // val useColoring = theme.colors.continuousColoring

    val gradient = ContinuousColoring.gradient(HTMLNamedColors.blue, HTMLNamedColors.orange)
    val colorFunc = gradient(Seq(0.7, 1.0))

    val plot = Overlay.fromSeq(
      data.map { lineData =>
        // LinePlot.apply(
        ScatterPlot.apply(
          lineData,
          Some(
            PointRenderer.custom[WeirdLinePoint](
              renderFn = (pt =>
                Disc.centered(pt.weight).filled(colorFunc(pt.value).hsla.copy(opacity = 0.4))
              )
            )
            // PathRenderer.custom(
            //   (plotCtx: PlotContext, items: Seq[WeirdLinePoint]) => ???: Drawable
            // )
          )
        )
      }
    ).title(title)
      .xLabel(xAxisLabel)
      .yLabel(yAxisLabel)
      .xAxis().yAxis()
      .frame().rightLegend()

    IO(plot.render().write(new java.io.File(path.toString)))

    //   data.toList.map { case (scoreName, data) =>
    //     val summedPoints = data.groupByNel(p => p.x -> p.y)
    //       .toList
    //       .map { case (xy, pts) => pts.head.copy(weight = pts.foldMap(_.weight)) }

    //     ScatterPlot(
    //       summedPoints,
    //       pointRenderer = Some(
    //         PointRenderer.custom[WeightedScatterPoint](
    //           renderFn = (pt =>
    //             Disc.centered(pt.weight / 10.0).filled(colorFunc(scoreName).hsla.copy(opacity = 0.5))),
    //           legendCtx = Some(useColoring.legendContext(categories))
    //         )
    //       )
    //     )
    //   }
  }

  case class WeightedScatterPoint(
    x: Double,
    y: Double,
    weight: Double
  ) extends Datum2d[WeightedScatterPoint] {
    def withXY(x: Double, y: Double) = this.copy(x = x, y = y)
  }

  def plotWeightedScatter(
    data: Map[String, List[WeightedScatterPoint]],
    title: String,
    xAxisLabel: String,
    yAxisLabel: String,
    path: NIOPath): IO[Unit] = {
    // val colors = Color.getDefaultPaletteSeq(data.size)

    val categories = data.keySet.toList
    val useColoring = CategoricalColoring.themed[String]
    val colorFunc = useColoring(categories)
    // val radius = size.getOrElse(theme.elements.pointSize)

    val plot = Overlay.fromSeq(
      data.toList.map { case (scoreName, data) =>
        val summedPoints = data.groupByNel(p => p.x -> p.y)
          .toList
          .map { case (xy, pts) => pts.head.copy(weight = pts.foldMap(_.weight)) }

        ScatterPlot(
          summedPoints,
          pointRenderer = Some(
            PointRenderer.custom[WeightedScatterPoint](
              renderFn = (pt =>
                Disc.centered(pt.weight / 10.0).filled(colorFunc(scoreName).hsla.copy(opacity = 0.5))),
              legendCtx = Some(useColoring.legendContext(categories))
            )
          )
        )
      }
    ).title(title)
      .xLabel(xAxisLabel)
      .yLabel(yAxisLabel)
      .xAxis().yAxis()
      .frame().rightLegend()

    IO(plot.render().write(new java.io.File(path.toString)))
  }

  def plotPrecisionRecallCurves(
    tuningResults: Map[String, List[(Double, WeightedPR)]],
    title: String,
    precisionAxisLabel: String,
    recallAxisLabel: String,
    path: NIOPath)(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = IO {

    case class PRPoint(
      category: String,
      value: Double,
      isBestF1: Boolean,
      recall: Double,
      precision: Double) extends Datum2d[PRPoint] {
      val x = recall
      val y = precision
      def f1 = Functions.harmonicMean(precision, recall)

      def withXY(x: Double = this.recall, y: Double = this.precision) = this.copy(recall = x, precision = y)
    }

    // val rand = new scala.util.Random(2643642L)
    // def noise = scala.math.abs(rand.nextGaussian / 200.0)
    def makePoints(category: String, prs: List[(Double, WeightedPR)]) = {
      val bestF1 = prs.map(_._2.f1).max
      prs.map { case (t, pr) => PRPoint(category, t, pr.f1 == bestF1, pr.recall, pr.precision) }
    }

    val allData = tuningResults.toList
      .sortBy(-_._2.map(_._2.f1).max)
      .map { case (cat, items) => cat -> makePoints(cat, items) }

    // val maxNumClusters = allData.flatMap(_.toList).filter(_.isBestF1).map(_.numClusters).max
    // val maxNumInstances = allData.values.toList.map(_.head.numItems).max
    val colors = Color.getDefaultPaletteSeq(allData.size)

    val plot = Overlay.fromSeq(
      colors.zip(allData).map { case (color, (category, data)) =>
        LinePlot.series(data, category, color, strokeWidth = Some(1.0))
      } ++
        colors.zip(allData).map { case (color, (category, data)) =>
          ScatterPlot(
		        data.toList.filter(_.isBestF1),
            pointRenderer = Some(
              PointRenderer.default[PRPoint](
                color = Some(color), pointSize = Some(2.0)
              )
            )
	        )
        }
    ).title(title)
      .xLabel(recallAxisLabel)
      .yLabel(precisionAxisLabel)
      .xbounds(0.0, 1.0).ybounds(0.0, 1.0)
      .xAxis().yAxis()
      .frame().rightLegend()

    plot.render().write(new java.io.File(path.toString))
  }
}
