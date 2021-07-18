package qasrl.roles.modeling

import cats.Id
import cats.effect.IO
import cats.implicits._

import java.nio.file.Files
import java.nio.file.Path

import qasrl.ArgStructure
import qasrl.ArgumentSlot
import qasrl.data.Dataset

import freelog.TreeLogger

import jjm.metrics.HasMetrics.ops._

trait PackagePlatformExtensions {

  // import cats.effect.Blocker
  // import fs2.io.file

  val createDir = (path: Path) => {
    // val blocker = Blocker[IO]
    // file.exists(blocker, path).ifM(
    //   file.createDirectories(blocker, path), IO.unit
    // )
    IO(!Files.exists(path))
      .ifM(IO(Files.createDirectories(path)), IO.unit)
  }

  def fileExists(path: Path) = IO(Files.exists(path))

  // val createDir = (path: Path) => IO(!Files.exists(path))
  //   .ifM(IO(Files.createDirectories(path)), IO.unit)

  def getSubdirs(path: Path): IO[List[Path]] = IO {
    import scala.collection.JavaConverters._
    new java.io.File(path.toString).listFiles.iterator
      .filter(_.isDirectory)
      .map(f => path.resolve(f.getName))
      .toList
  }

  def filterDatasetNonDense(dataset: Dataset) = {
    dataset.filterQuestionLabels(questionLabelIsValidNonDense)
      .cullQuestionlessVerbs
      .cullQuestionlessSentences
      .cullVerblessSentences
  }

  def readDataset(path: Path): IO[Dataset] = IO.fromTry(
    qasrl.bank.Data.readQasrlDataset(path)
  )

  import jjm.metrics._

  lazy val sortSpec = {
    import Metric._
    import MapTree.SortQuery._
    val double = (mv: Metric) => mv match {
      case MetricMetadata(s) => 0.0
      case MetricBool(x) => if(x) 1.0 else 0.0
      case MetricInt(x) => x.toDouble
      case MetricDouble(x) => x
      case MetricIntOfTotal(x, _) => x.toDouble
    }
    val inc = value[String](double)
    val dec = value[String](double andThen (_ * -1))
    List(
      "predictions" :: "f1" :: inc,
      "full question" :: "f1" :: inc,
      "full question" :: "acc-lb" :: inc,
      "num predicted" :: inc,
      "mean" :: inc
    )
  }

  def getMetricsString[M: HasMetrics](m: M) =
    m.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec)
}

