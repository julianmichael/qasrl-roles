package qasrl.roles.modeling.features

import qasrl.roles.modeling._

import qasrl.roles.modeling.util.VectorFileUtil

import java.nio.file._

import jjm.NonMergingMap
import jjm.ling.ESpan
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.io.Cell
import jjm.io.FileCached
import jjm.io.FileUtil
import jjm.implicits._

import cats.Order
import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

import fs2.Stream

import io.circe.generic.JsonCodec
import io.circe.{Encoder, Decoder}

import freelog.EphemeralTreeLogger
import freelog.implicits._
import java.net.URL
import cats.effect.Blocker

abstract class PropBankFeatures[Arg](
  mode: RunMode,
  val assumeGoldVerbSense: Boolean)(
  implicit cs: ContextShift[IO],
  Log: EphemeralTreeLogger[IO, String]
) extends Features[String, Arg](mode)(implicitly[Encoder[String]], implicitly[Decoder[String]], cs, Log) {

  override def getIfPropBank: Option[PropBankFeatures[Arg]] = Some(this)

  override def getVerbLemma(verbType: String): String = {
    if(assumeGoldVerbSense) verbType.takeWhile(_ != '.')
    else verbType
  }

  // val inflectionDictionary
  import jjm.ling.en.Inflections

  val wiktionaryInflectionsURL = "https://www.dropbox.com/s/1wpsydqsuf9jm8v/en_verb_inflections.txt.gz?dl=1"
  def allInflectionLists(path: Path) = FileCached[List[InflectedForms]](
    path = path,
    read = path => FileUtil.readJsonLines[InflectedForms](path).compile.toList,
    write = (path, inflections) => FileUtil.writeJsonLines(path)(inflections))(
    Stream.resource(Blocker[IO]).flatMap { blocker =>
      val urlStream = IO(new URL(wiktionaryInflectionsURL).openConnection.getInputStream)
      fs2.io.readInputStream(urlStream, 4096, blocker, true)
        .through(fs2.compression.gunzip(4096))
        .flatMap(_.content)
        .through(fs2.text.utf8Decode)
        .through(fs2.text.lines)
        .filter(_.trim.nonEmpty)
        .map { line =>
          val f = line.trim.split("\\t")
          InflectedForms.fromStrings(
            stem = f(0),
            presentSingular3rd = f(1),
            presentParticiple = f(2),
            past = f(3),
            pastParticiple = f(4)
          )
        }
    }.compile.toList
  )

  lazy val verbInflectedFormsByStem = Cell {
    cacheDir.flatMap(dir => allInflectionLists(dir.resolve("en_verb_inflections.txt.gz")).get)
      .map(_.groupBy(_.stem))
  }

  lazy val verbInflectedFormLists: IO[String => List[InflectedForms]] =
    verbInflectedFormsByStem.get.map(m => verbType => m.apply(getVerbLemma(verbType).lowerCase))


  def renderVerbType(verbType: String): String = verbType

  // don't store the models in the same dir, because they cluster different kinds of things
  override def modelDir = super.modelDir.map(
    _.resolve(if(assumeGoldVerbSense) "by-sense" else "by-lemma")
  ).flatTap(createDir)

  override def modelTuningDir = super.modelTuningDir.map(
    _.resolve(if(assumeGoldVerbSense) "by-sense" else "by-lemma")
  ).flatTap(createDir)

  def verbSenseLabels: CachedVerbFeats[String]

  def argRoleLabels: CachedArgFeats[PropBankRoleLabel]
}
