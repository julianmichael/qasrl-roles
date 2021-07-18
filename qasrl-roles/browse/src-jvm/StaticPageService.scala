package qasrl.roles.browse

import qasrl.roles.modeling.DataSetting
import qasrl.roles.modeling.RunMode

import cats.effect.IO
import cats.effect.Sync
import cats.effect.ContextShift
import cats.implicits._

import org.http4s._
import org.http4s.headers._
import org.http4s.circe._
import org.http4s.implicits._

import java.nio.file.Path
import java.nio.file.Paths

import scala.concurrent.ExecutionContext
import cats.effect.Blocker

object StaticPageService {

  // TODO just including local thingy here too because easier to copy & paste. maybe change.

  sealed trait LinkType; case object CSSLink extends LinkType; case object JSLink extends LinkType
  case class LinkForDownload(
    remoteUrl: String,
    localLocation: String,
    integrity: String,
    linkType: LinkType
  ) {
    import scalatags.Text.all._
    def makeTag(isLocal: Boolean) = linkType match {
      case CSSLink => link(
        rel := "stylesheet",
        href := (if(isLocal) localLocation else remoteUrl),
        Option(attr("integrity") := integrity).filter(_ => !isLocal),
        Option(attr("crossorigin") := "anonymous").filter(_ => !isLocal)
      )
      case JSLink => script(
        src := (if(isLocal) localLocation else remoteUrl),
        Option(attr("integrity") := integrity).filter(_ => !isLocal),
        Option(attr("crossorigin") := "anonymous").filter(_ => !isLocal)
      )
    }
  }

  val (bootstrapLink, bootstrapScripts) = {
    val bootstrapLink = LinkForDownload(
      "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css",
      "css/bootstrap.min.css",
      "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm",
      CSSLink
    )
    val scriptLinks = List(
      LinkForDownload(
        "https://code.jquery.com/jquery-3.2.1.slim.min.js",
        "scripts/jquery-3.2.1.slim.min.js",
        "sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN",
        JSLink
      ),
      LinkForDownload(
        "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js",
        "scripts/popper.min.js",
        "sha384-ApNbgh9B+Y1QKtv3Rn7W3mgPxhU9K/ScQsAP7hUibX39j7fakFPskvXusvfa0b4Q",
        JSLink
      ),
      LinkForDownload(
        "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js",
        "scripts/bootstrap.min.js",
        "sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl",
        JSLink
      ),
      )
    (
      bootstrapLink,
      scriptLinks
    )
  }

  def makeService(
    domain: String,
    docApiSuffix: String,
    verbApiSuffix: String,
    featureApiSuffix: String,
    dataSetting: DataSetting,
    mode: RunMode,
    jsDepsPath: Path,
    jsPath: Path,
    port: Option[Int],
    useHttps: Boolean
  )(implicit ec: ExecutionContext, s: Sync[IO], cs: ContextShift[IO]) = {
    val jsDepsSuffix = "deps.js"
    val jsSuffix = jsPath.getFileName.toString
    val jsMapSuffix = jsSuffix + ".map"

    val blocker = Blocker.liftExecutionContext(ec)

    val scheme = if(useHttps) "https" else "http"
    val baseUrl = domain + port.foldMap(p => s":$p")

    val config = {
      import scalatags.Text.all._
      PageConfig(
        docApiUrl = s"$scheme://$baseUrl/$docApiSuffix",
        verbApiUrl = s"$scheme://$baseUrl/$verbApiSuffix",
        featureApiUrl = s"$scheme://$baseUrl/$featureApiSuffix",
        bootstrapLink = bootstrapLink.makeTag(false),
        bootstrapScripts = div(bootstrapScripts.map(_.makeTag(false))),
        dataSetting = dataSetting,
        mode = mode,
        jsDepsPath = jsDepsSuffix,
        jsPath = jsSuffix
      )
    }

    val indexStr = pages.Index(config).render.trim

    import org.http4s.dsl.io._

    HttpRoutes.of[IO] {
      case req @ GET -> Root / `jsDepsSuffix` =>
        StaticFile.fromString(jsDepsPath.toString, blocker, Some(req))
          .getOrElseF(NotFound())
      case req @ GET -> Root / `jsSuffix` =>
        StaticFile.fromString(jsPath.toString, blocker, Some(req))
          .getOrElseF(NotFound())
      case req @ GET -> Root / `jsMapSuffix` =>
        StaticFile.fromString(jsPath.toString + ".map", blocker, Some(req))
          .getOrElseF(NotFound())
      case GET -> _ => Ok(indexStr)
          .map(_.withContentType(`Content-Type`(MediaType.text.`html`)))
    }
  }

}
