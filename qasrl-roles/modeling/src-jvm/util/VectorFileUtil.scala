package qasrl.roles.modeling.util

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.{ContextShift, ExitCode, IO, IOApp, Resource, Timer}

import fs2.Stream

import java.nio.file.{Path => NIOPath}
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService

import freelog.Logger
import freelog.LogLevel

import qasrl.roles.modeling.logLevel
import cats.effect.Blocker

object VectorFileUtil {

  import java.nio.ByteBuffer
  import java.nio.ByteOrder
  import breeze.linalg._
  import scala.concurrent.duration._

  val bufferNumBytes = 4 * 4096

  def readDenseFloatVectors(path: NIOPath, dim: Int)(
    implicit cs: ContextShift[IO], t: Timer[IO]
  ) = {
    Stream.resource(Blocker[IO]).flatMap { blocker =>
      fs2.io.file.readAll[IO](path, blocker, bufferNumBytes)
        .groupWithin(4, 1.minute) // should always do 4 chunk
        .map { c =>
          val bytes = c.toBytes
          ByteBuffer.wrap(bytes.values, bytes.offset, bytes.length).order(ByteOrder.LITTLE_ENDIAN).getFloat
        }
        .groupWithin(dim, 1.minute) // should always do dim chunk
        .map { c =>
          val floats = c.toFloats
          DenseVector.create[Float](floats.values, floats.offset, 1, floats.length)
        }
    }
  }

  def readDenseFloatVectorsNIO(path: NIOPath, dim: Int)(
    implicit Log: Logger[IO, String]
  ) = for {
    bytes <- IO(java.nio.file.Files.readAllBytes(path))
    _ <- Log.warn(
      s"Number of bytes in embedding file (${bytes.length}) is not divisible by 4"
    ).whenA(bytes.length % 4 != 0)
    numFloats = bytes.length / 4
    _ <- Log.warn(
      s"Number of floats in embedding file (${numFloats}) is not divisible by embedding dimension ($dim)"
    ).whenA(numFloats % dim != 0)
    // val numVectors = numFloats / dim
    result <- IO {
      val floats = new Array[Float](numFloats)
      ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).asFloatBuffer.get(floats) // put data in float array
      var offset = 0
      var res = List.empty[DenseVector[Float]]
      while(offset < numFloats) {
        res = DenseVector.create(floats, offset, 1, dim) :: res
        offset = offset + dim
      }
      res.reverse
    }
    _ <- Log.info(s"Read ${result.size} vectors of dimensionality $dim.")
  } yield result
}
