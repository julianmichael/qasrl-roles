package qasrl.roles.modeling.eval

import com.cibo.evilplot.plot._
import com.cibo.evilplot.plot.aesthetics.Theme
import com.cibo.evilplot.plot.renderers.PlotRenderer

import com.cibo.evilplot.colors.{Color, Coloring, ScaledColorBar, ContinuousColoring, GradientMode}
import com.cibo.evilplot.geometry.{Drawable, Extent, Rect}
import com.cibo.evilplot.numeric.Bounds

object Heatmap2 {

  val defaultColorCount: Int = 10

  final case class HeatmapRenderer(
    data: Seq[Seq[Double]],
    colorBar: ScaledColorBar
  )(implicit theme: Theme)
      extends PlotRenderer {

    override def legendContext: LegendContext = LegendContext.continuousGradientFromColorBar(colorBar)

    val coloring = ContinuousColoring.gradient(
      colorBar.colorSeq, Some(colorBar.zMin), Some(colorBar.zMax), GradientMode.Natural
    )

    def render(plot: Plot, plotExtent: Extent)(implicit theme: Theme): Drawable = {
      val xtransformer = plot.xtransform(plot, plotExtent)
      val ytransformer = plot.ytransform(plot, plotExtent)
      val rowCount = data.size
      val colorFn = coloring(data.flatten)

      data.zipWithIndex.map {
        case (row, yIndex) =>
          row.zipWithIndex.map {
            case (value, xIndex) =>
              val y = ytransformer(yIndex)
              val x = xtransformer(xIndex)
              val width = xtransformer(xIndex + 1) - x
              val height = -(ytransformer(yIndex + 1) - y)
              Rect(width, height).filled(colorFn(value)).translate(x, y - height)
          }.group
      }.group
    }
  }

  /** Create a heatmap using a ScaledColorBar.
    * @param data The heatmap data.
    * @param colorBar The color bar to use.
    */
  def apply(
    data: Seq[Seq[Double]],
    colorBar: ScaledColorBar
  )(implicit theme: Theme): Plot = {
    val xbounds = Bounds(0, data.foldLeft(0)((a, s) => math.max(a, s.size)))
    val ybounds = Bounds(0, data.size)
    Plot(
      xbounds = xbounds,
      ybounds = ybounds,
      xfixed = true,
      yfixed = true,
      renderer = HeatmapRenderer(data, colorBar)
    )
  }

  /** Create a heatmap using a color sequence.
    * @param data The heatmap data.
    * @param colorCount The number of colors to use from the sequence.
    * @param colors The color sequence.
    */
  def apply(
    data: Seq[Seq[Double]],
    colorCount: Int = defaultColorCount,
    colors: Seq[Color] = Seq.empty
  )(implicit theme: Theme): Plot = {
    val colorStream = if (colors.nonEmpty) colors else theme.colors.stream
    val flattenedData = data.flatten
    val minValue = flattenedData.reduceOption[Double](math.min).getOrElse(0.0)
    val maxValue = flattenedData.reduceOption[Double](math.max).getOrElse(0.0)
    val colorBar = ScaledColorBar(colorStream.take(colorCount), minValue, maxValue)
    apply(data, colorBar)
  }

  def apply(data: Seq[Seq[Double]], coloring: Option[Coloring[Double]])(
    implicit theme: Theme): Plot = {
    val flattenedData = data.flatten
    val minValue = flattenedData.reduceOption[Double](math.min).getOrElse(0.0)
    val maxValue = flattenedData.reduceOption[Double](math.max).getOrElse(0.0)
    val useColoring = coloring.getOrElse(theme.colors.continuousColoring)
    val colorFunc = useColoring(flattenedData)
    val colorBar =
      ScaledColorBar(flattenedData.map(point => colorFunc.apply(point)), minValue, maxValue)
    apply(data, colorBar)

  }
}
