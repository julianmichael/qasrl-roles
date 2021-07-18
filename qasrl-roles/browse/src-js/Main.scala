package qasrl.roles.browse

import qasrl.roles.modeling.DataSetting
import qasrl.roles.modeling.RunMode

import japgolly.scalajs.react._

import cats.~>
import cats.Id
import cats.Order
import cats.implicits._

import org.scalajs.dom
import org.scalajs.dom.experimental

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import qasrl.bank._
import qasrl.bank.service._

import io.circe.{Encoder, Decoder}

import scala.concurrent.Future

import jjm.DotKleisli
import jjm.DotMap
import jjm.OrWrapped
import jjm.io.HttpUtil
import jjm.ui._
import jjm.implicits._

object Main {
  def runApp[
    VerbType: Encoder : Decoder : VerbTypeRendering,
    Arg: Encoder : Decoder : Order : ArgRendering[VerbType, *]
  ]: Unit = {
    FrameBrowserStyles.addToDocument()

    import scala.concurrent.ExecutionContext.Implicits.global

    val docApiEndpoint: String = dom.document
      .getElementById(SharedConstants.docApiUrlElementId)
      .getAttribute("value")

    type DelayedFuture[A] = () => Future[A]
    val wrapCallback = λ[DelayedFuture ~> AsyncCallback](f =>
      AsyncCallback.fromFuture(f())
    )

    val initialFeatureCache = DotMap.empty[Id, FeatureReq[VerbType, Arg]]

    val featureApiEndpoint: String = dom.document
      .getElementById(SharedConstants.featureApiUrlElementId)
      .getAttribute("value")
    val featureService = jjm.Memo.memoizeDotFuture(
      HttpUtil.makeHttpPostClient[FeatureReq[VerbType, Arg]](featureApiEndpoint),
      initialFeatureCache
    ).andThenK(OrWrapped.mapK(wrapCallback))

    val verbApiEndpoint: String = dom.document
      .getElementById(SharedConstants.verbApiUrlElementId)
      .getAttribute("value")

    val initialVerbCache = DotMap.empty[Id, VerbFrameService.Request[ClusterModelSpec, VerbType, Arg]]

    val verbFrameService = VerbFrameService(
      jjm.Memo.memoizeDotFuture(
        HttpUtil.makeHttpPostClient[VerbFrameService.Request[ClusterModelSpec, VerbType, Arg]](verbApiEndpoint),
        initialVerbCache
      ).andThenK(OrWrapped.mapK(wrapCallback))
    )

    // val query = NavQuery.fromString(dom.window.location.pathname.tail)

    val runMode = io.circe.parser.decode[RunMode](
      dom.document.getElementById(SharedConstants.devFlagElementId).getAttribute("value")
    ).right.get

    val UI = new FrameBrowserUI[VerbType, Arg]

    UI.Component(
      UI.Props(
        verbFrameService, featureService,
        // query,
        runMode
      )
    ).renderIntoDOM(
      dom.document.getElementById(SharedConstants.mainDivElementId)
    )
  }

  def main(args: Array[String]): Unit = {
    val dataSetting: DataSetting = DataSetting.fromString(
      dom.document
      .getElementById(SharedConstants.dataSettingElementId)
      .getAttribute("value")
    ).get

    dataSetting match {
      case d @ DataSetting.Qasrl => runApp[d.VerbType, d.Arg]
      case d @ DataSetting.Ontonotes5(_) => runApp[d.VerbType, d.Arg]
      case d @ DataSetting.CoNLL08(_) => runApp[d.VerbType, d.Arg]
    }
  }
}
