package qasrl.roles.modeling.features

import qasrl.roles.modeling.ArgumentId
import qasrl.roles.modeling.DataSplit
import qasrl.roles.modeling.RunMode
import qasrl.roles.modeling.VerbId

import jjm.io.Cell
import jjm.io.FileCached

import cats.Applicative
import cats.Monad
import cats.Monoid
import cats.effect.IO
import cats.implicits._

import java.nio.file.Path

import freelog.TreeLogger

case class RunData[+A](
  train: IO[A],
  dev: IO[A],
  test: IO[A])(
  implicit mode: RunMode
) {

  def get = apply(mode.dataSplit)

  def all = List(train, dev, test).sequence

  def apply(split: DataSplit): IO[A] = split match {
    case DataSplit.Train => train
    case DataSplit.Dev => dev
    case DataSplit.Test => test
  }

  def map[B](f: A => B) = RunData(
    train map f,
    dev map f,
    test map f
  )

  def flatMap[B](f: A => IO[B]) = RunData(
    train >>= f,
    dev >>= f,
    test >>= f
  )
  // def >>=[B](f: A => IO[B]) = flatMap(f)

  def zip[B](that: RunData[B]) = RunData(
    this.train product that.train,
    this.dev product that.dev,
    this.test product that.test
  )

  def toCell(
    implicit mode: RunMode,
    // monoid: Monoid[A],
    Log: TreeLogger[IO, String]) = new RunDataCell(
    new Cell(train),
    new Cell(dev),
    new Cell(test),
  )

  def toFileCachedCell[B >: A](
    getCachePath: String => IO[Path])(
    read: Path => IO[B], write: (Path, B) => IO[Unit])(
    implicit mode: RunMode,
    // monoid: Monoid[A],
    Log: TreeLogger[IO, String]) = {
    def doCache(runName: String, a: IO[B]) = {
      Cell(
        getCachePath(runName) >>= (path =>
          FileCached.get[B](
            path = path,
            read = read,
            write = write)(
            a
          )
        )
      )
    }
    new RunDataCell(
      doCache("train", train),
      doCache("dev", dev),
      doCache("test", test)
    )
  }
}
object RunData {
  def strings(implicit mode: RunMode) = RunData(
    IO.pure("train"), IO.pure("dev"), IO.pure("test")
  )
  def apply[A](train: A, dev: A, test: A)(implicit mode: RunMode): RunData[A] = RunData(
    IO.pure(train), IO.pure(dev), IO.pure(test)
  )
  def splits(implicit mode: RunMode) = RunData[DataSplit](
    IO.pure(DataSplit.Train), IO.pure(DataSplit.Dev), IO.pure(DataSplit.Test)
  )

  implicit def runDataMonoid[A](
    implicit mode: RunMode, A: Monoid[A]
  ): Monoid[RunData[A]] = new Monoid[RunData[A]] {
    def empty: RunData[A] = {
      val e = IO.pure(A.empty)
      RunData(e, e, e)
    }

    def combine(x: RunData[A], y: RunData[A]): RunData[A] = {
      RunData(
        train = (x.train, y.train).mapN(_ |+| _),
        dev = (x.dev, y.dev).mapN(_ |+| _),
        test = (x.test, y.test).mapN(_ |+| _)
      )
    }
  }


  implicit def runDataApplicative(
    implicit mode: RunMode
  ): Applicative[RunData] = new Applicative[RunData] {

    def pure[A](a: A): RunData[A] = {
      val fa = IO.pure(a)
      RunData(fa, fa, fa)
    }

    def ap[A, B](ff: RunData[A => B])(fa: RunData[A]): RunData[B] = RunData(
      train = ff.train <*> fa.train,
      dev = ff.dev <*> fa.dev,
      test = ff.test <*> fa.test
    )

    override def map[A, B](fa: RunData[A])(f: A => B): RunData[B] = fa.map(f)

    override def product[A, B](fa: RunData[A], fb: RunData[B]): RunData[(A, B)] = fa.zip(fb)
  }
}

class RunDataCell[+A](
  train: Cell[A],
  dev: Cell[A],
  test: Cell[A])(
  implicit mode: RunMode, Log: TreeLogger[IO, String]
) {

  def apply(split: DataSplit): IO[A] = split match {
    case DataSplit.Train => train.get
    case DataSplit.Dev => dev.get
    case DataSplit.Test => test.get
  }

  def data = RunData(train.get, dev.get, test.get)

  def all: IO[List[A]] = data.all

  def get = data.get
}
object RunDataCell
