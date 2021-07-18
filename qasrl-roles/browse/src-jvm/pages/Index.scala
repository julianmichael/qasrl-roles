package qasrl.roles.browse.pages
import scalatags.Text.all._
import qasrl.roles.browse._
import scalacss.internal.{Css, Renderer}
import scalatags.Text.TypedTag

// @package qasrl.roles.browse.pages
// @import qasrl.roles.browse._
// @param config: PageConfig

import io.circe.syntax._

object Index {
  def apply(config: PageConfig): scalatags.Text.all.Frag = 
    html(lang := "en")(
      head(
        meta(charset := "utf-8"),
        meta(
          name := "viewport",
          content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
        config.bootstrapLink,
        tag("title")("Roleset Explorer | Inducing Semantic Roles Without Syntax")
      ),
      body(
        div(id := SharedConstants.mainDivElementId)(
          p("Loading JavaScript...")
        ),
        input(
          `type` := "hidden",
          value := config.docApiUrl,
          id := SharedConstants.docApiUrlElementId
        ),
        input(
          `type` := "hidden",
          value := config.verbApiUrl,
          id := SharedConstants.verbApiUrlElementId
        ),
        input(
          `type` := "hidden",
          value := config.dataSetting.toString,
          id := SharedConstants.dataSettingElementId
        ),
        input(
          `type` := "hidden",
          value := config.featureApiUrl,
          id := SharedConstants.featureApiUrlElementId
        ),
        input(
          `type` := "hidden",
          value := io.circe.Printer.noSpaces.print(config.mode.asJson),
          id := SharedConstants.devFlagElementId
        ),
        canvas(
          display := "none",
          id := "measureBuffer"
        ),
        config.bootstrapScripts,
        script(src := "/" + config.jsDepsPath),
        script(src := "/" + config.jsPath)
      )
    )
  def sourcePath = "pages/Index.scalatex"
}
// @import scalacss.internal.{Css, Renderer}
// @import scalatags.Text.TypedTag
// @def scalatagsStyleTagRenderer(implicit s: Renderer[String]): Renderer[TypedTag[String]] = {
//   // workaround to prevent the default style tag renderer from escaping quotes in the CSS
//   new Renderer[TypedTag[String]] {
//     override def apply(css: Css) = scalatags.Text.all.tag("style")(`type` := "text/css", raw(s(css)))
//   }
// }


