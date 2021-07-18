package qasrl.roles.browse

import com.cibo.evilplot.geometry._

import com.cibo.evilplot.colors.HTMLNamedColors
import org.scalajs.dom.raw.CanvasRenderingContext2D

import japgolly.scalajs.react.vdom.svg_<^._
import japgolly.scalajs.react.vdom.VdomElement

// SVG drawer for evilplot. Does NOT work. lol

sealed trait SVGRenderStep {
  import SVGRenderStep._
  def isLeaf = this match {
    case Leaf(_) => true
    case _ => false
  }
  def getElement = this match {
    case Leaf(element) => Some(element)
    case _ => None
  }
}
object SVGRenderStep {
  case class Leaf(element: VdomElement) extends SVGRenderStep
  case class Node(cons: Seq[VdomElement] => VdomElement) extends SVGRenderStep
}

class SVGRenderContext extends RenderContext {
  import SVGRenderStep.{Leaf, Node}

  def getSVG(width: Double, height: Double) = {
    require(state.forall(_.isLeaf))

    <.svg(
      ^.viewBox := s"0 0 $width $height"
    )(state.map(_.getElement.get): _*)
  }

  var state: List[SVGRenderStep] = Nil
  var nextGradientId: Int = 0

  def push(element: VdomElement) = {
    state = SVGRenderStep.Leaf(element) :: state
  }

  def pop() = {
    val (siblings, remaining) = state.partition(_.isLeaf)
    val sibs = siblings.map(_.getElement.get)
    remaining match {
      case Node(cons) :: rest => state = Leaf(cons(sibs.reverse)) :: rest
      case _ => ???
    }
  }

  def using(parent: Seq[VdomElement] => VdomElement)(f: => Unit) = {
    state = SVGRenderStep.Node(parent) :: state
    f
    pop()
  }

  // var state: VdomElement

  // val canvas: CanvasRenderingContext2D

  def clear() = {
    state = Nil
    nextGradientId = 0
  }
  // canvas.clearRect(0, 0, this.canvas.canvas.width, this.canvas.canvas.height)

  def draw(line: Line): Unit = push {
    <.line(
      ^.x1 := 0,
      ^.y1 := line.strokeWidth / 2.0,
      ^.x2 := line.length,
      ^.y2 := line.strokeWidth / 2.0,
      ^.strokeWidth := line.strokeWidth
      // ^.stroke := line.
    )
  }
  // canvas.lineWidth = line.strokeWidth
  // canvas.beginPath()
  // canvas.moveTo(0, line.strokeWidth / 2.0)
  // canvas.lineTo(line.length, line.strokeWidth / 2.0)
  // canvas.closePath()
  // canvas.stroke()

  def draw(path: Path): Unit = push {
    val pointStrs = path.points.map(p => s"${p.x},${p.y}")
    val pathStr = s"M ${pointStrs.head} " + pointStrs.tail.map(x => s"L $x ").mkString + "Z"
    <.path(
      ^.strokeLinejoin := "round",
      ^.strokeWidth := path.strokeWidth,
      ^.d := pathStr
    )
  }
  //   CanvasOp(canvas) {
  //   canvas.lineJoin = "round"
  //   canvas.beginPath()
  //   canvas.moveTo(path.points.head.x, path.points.head.y)
  //   canvas.lineWidth = path.strokeWidth
  //   path.points.tail.foreach { point =>
  //     canvas.lineTo(point.x, point.y)
  //   }
  //   canvas.stroke()
  // }

  def draw(polygon: Polygon): Unit = push {
    <.polygon(
      ^.points := polygon.boundary.map(p => s"${p.x},${p.y}").mkString(" ")
    )
  }
  // CanvasOp(canvas) {
  //   canvas.beginPath()
  //   canvas.moveTo(polygon.boundary.head.x, polygon.boundary.head.y)
  //   polygon.boundary.tail.foreach { point =>
  //     canvas.lineTo(point.x, point.y)
  //   }
  //   canvas.fill()
  // }

  def draw(rect: Rect): Unit = push {
    <.rect(0, 0, rect.width, rect.height)
  }
    // canvas.fillRect(0, 0, rect.width, rect.height)

  def draw(rect: BorderRect): Unit = push {
    <.rect(0, 0, rect.width, rect.height)
  }
    // canvas.strokeRect(0, 0, rect.width, rect.height)

  def draw(disc: Disc): Unit = push {
    <.circle(
      ^.cx := 0, ^.cy := 0,
      ^.r := disc.radius
    )
  }

  // CanvasOp(canvas) {
  //   canvas.beginPath()
  //   canvas.arc(disc.radius, disc.radius, disc.radius, 0, 2 * Math.PI)
  //   canvas.closePath()
  //   canvas.fill()
  // }

  def draw(wedge: Wedge): Unit = ???

  // CanvasOp(canvas) {
  //   canvas.translate(wedge.radius, wedge.radius)
  //   canvas.beginPath()
  //   canvas.moveTo(0, 0)
  //   canvas.arc(0, 0, wedge.radius, 0, 2 * Math.PI * wedge.degrees / 360.0)
  //   canvas.closePath()
  //   canvas.fill()
  // }

  def draw(translate: Translate): Unit = using(
    <.g(^.transform := s"translate(${translate.x} ${translate.y})").apply
  )(translate.r.draw(this))

    // canvas.translate(translate.x, translate.y)
    // translate.r.draw(this)

  def draw(affine: Affine): Unit = using(
    <.g(
      ^.transform := "matrix(" +
        List(
          affine.affine.scaleX,
          affine.affine.shearX,
          affine.affine.shearY,
          affine.affine.scaleY,
          affine.affine.shiftX,
          affine.affine.shiftY
        ).mkString(" ") + ")"
    ).apply
  )(affine.r.draw(this))

  // CanvasOp(canvas) {
  //   canvas.transform(
  //     affine.affine.scaleX,
  //     affine.affine.shearX,
  //     affine.affine.shearY,
  //     affine.affine.scaleY,
  //     affine.affine.shiftX,
  //     affine.affine.shiftY
  //   )
  //   affine.r.draw(this)
  // }

  def draw(scale: Scale): Unit = using(
    <.g(^.transform := s"scale(${scale.x} ${scale.y})").apply
  )(scale.r.draw(this))

  // CanvasOp(canvas) {
  //   canvas.scale(scale.x, scale.y)
  //   scale.r.draw(this)
  // }

  def draw(rotate: Rotate): Unit = {
    val x = rotate.extent.width / 2
    val y = rotate.extent.height / 2
    using(
      <.g(^.transform := s"rotate(${rotate.degrees} ${x} ${y})").apply
    )(rotate.r.draw(this))
  }

  // def draw(rotate: Rotate): Unit = CanvasOp(canvas) {
  //   canvas.translate(-1 * rotate.minX, -1 * rotate.minY)
  //   canvas.rotate(math.toRadians(rotate.degrees))
  //   canvas.translate(rotate.r.extent.width / -2, rotate.r.extent.height / -2)
  //   rotate.r.draw(this)
  // }

  // com.cibo.evilplot.colors.Color

  def draw(style: Style): Unit = using(
    <.g(^.fill := style.fill.repr).apply
  )(style.r.draw(this))

  // CanvasOp(canvas) {
  //   canvas.fillStyle = style.fill.repr
  //   style.r.draw(this)
  // }

  def draw(style: StrokeStyle): Unit = using(
    <.g(^.stroke := style.fill.repr).apply
  )(style.r.draw(this))


  // CanvasOp(canvas) {
  //   canvas.strokeStyle = style.fill.repr
  //   style.r.draw(this)
  // }

  def draw(weight: StrokeWeight): Unit = using(
    <.g(^.strokeWidth := weight.weight).apply
  )(weight.r.draw(this))

  // CanvasOp(canvas) {
  //   canvas.lineWidth = weight.weight
  //   weight.r.draw(this)
  // }

  def draw(lineDash: LineDash): Unit = using(
    <.g(
      ^.strokeDashoffset := lineDash.style.offset,
      ^.strokeDasharray := lineDash.style.dashPattern.mkString(" ")
    ).apply
  )(lineDash.r.draw(this))

  // CanvasOp(canvas) {
  //   import scalajs.js.JSConverters._
  //   canvas.setLineDash(lineDash.style.dashPattern.toJSArray)
  //   canvas.lineDashOffset = lineDash.style.offset
  //   lineDash.r.draw(this)
  // }

  def draw(text: Text): Unit = {

    // Adjust the size of the font to fill the requested extent.
    // text.size assumes that the text will fill text.extent, but
    // in reality, it will fill baseExtent.
    // So we need to scale the size to fill text.extent.
    val baseExtent = TextMetrics.measure(text)
    val scalex = text.extent.width / baseExtent.width
    val scaley = text.extent.height / baseExtent.height

    push(
      <.g(
        ^.transform := s"scale(${scalex} ${scaley})",
        <.text(
          ^.fontSize := text.size + "px",
          ^.fontFamily := text.fontFace,
          ^.dominantBaseline := "text-top",
          text.msg
        )
      )
    )

    // CanvasOp(canvas) {
    //   canvas.scale(scalex, scaley)

    //   TextMetrics.withStyle(text.size, text.fontFace) { c =>
    //     c.fillText(text.msg, 0, 0)
    //   }(canvas)
    // }
  }

  def draw(gradient: GradientFill): Unit = {
    val thisGradientId = s"svg-plot-gradient-$nextGradientId"
    nextGradientId = nextGradientId + 1
    gradient.fill match {
      case lg: LinearGradient =>
        using(
          <.linearGradient(
            ^.id := thisGradientId,
            ^.gradientUnits := "userSpaceOnUse", // TODO
            ^.x1 := lg.x0, ^.y1 := lg.y0,
            ^.x2 := lg.x1, ^.y2 := lg.y1,
            lg.stops.toVdomArray(stop =>
              <.stop(
                ^.offset := stop.offset, // TODO check if need multiply for pct
                ^.stopColor := stop.color.repr
              )
            ),
            ^.fill := s"url('#$thisGradientId')"
          ).apply
        )(gradient.r.draw(this))

        // val gradientFill = canvas.createLinearGradient(lg.x0, lg.y0, lg.x1, lg.y1)
        // lg.stops.foreach { stop =>
        //   gradientFill.addColorStop(stop.offset, stop.color.repr)
        // }
        // canvas.fillStyle = gradientFill
        // gradient.r.draw(this)

      case rg: RadialGradient =>
        ???
        // val gradientFill = canvas.createRadialGradient(rg.x0, rg.y0, 0, rg.x1, rg.y1, rg.r0)
        // rg.stops.foreach { stop =>
        //   gradientFill.addColorStop(stop.offset, stop.color.repr)
        // }
        // canvas.fillStyle = gradientFill
        gradient.r.draw(this)
    }

    // gradient.fill match {
    //   case lg: LinearGradient =>
    //     val gradientFill = canvas.createLinearGradient(lg.x0, lg.y0, lg.x1, lg.y1)
    //     lg.stops.foreach { stop =>
    //       gradientFill.addColorStop(stop.offset, stop.color.repr)
    //     }
    //     canvas.fillStyle = gradientFill
    //     gradient.r.draw(this)

    //   case rg: RadialGradient =>
    //     val gradientFill = canvas.createRadialGradient(rg.x0, rg.y0, 0, rg.x1, rg.y1, rg.r0)
    //     rg.stops.foreach { stop =>
    //       gradientFill.addColorStop(stop.offset, stop.color.repr)
    //     }
    //     canvas.fillStyle = gradientFill
    //     gradient.r.draw(this)
    // }
  }

  def draw(interaction: Interaction): Unit = {
    // System.err.println("Interactions not supported!")
    interaction.r.draw(this)
  }
}
