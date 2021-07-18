package qasrl.roles.modeling.eval

import jjm.metrics._
// import qfirst.metrics.HasMetrics.ops._
// import qfirst.metrics.Functions

import cats.Show
import cats.effect.IO
import cats.implicits._

import qasrl.roles.modeling.logLevel
import freelog.EphemeralTreeLogger
import freelog.implicits._

object Csv {

  import com.github.tototoshi.csv._

  def writePrfComparisonCsv[A : Show, B : Show](
    path: java.nio.file.Path,
    firstHeader: String,
    secondHeader: String,
    stats1: Map[A, Map[B, WeightedPR]],
    stats2: Map[A, Map[B, WeightedPR]])(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[Unit] = Log.infoBranch(s"Writing PRF comparison to $path") {
    for {
      _ <- IO.unit
      headers = List(
        firstHeader, secondHeader,
        s"Frequency",
        s"Delta F1",
        s"Delta Smoothed (10) F1",
        s"Delta Smoothed (100) F1"
      )
      keys <- stats1.toList.infoBarFoldMapM("Constructing keys") {
        case (a, bs) => IO(bs.keySet.map(a -> _))
      }
      mean1 = stats1.values.toList.foldMap(_.values.toList.combineAll)
      mean2 = stats2.values.toList.foldMap(_.values.toList.combineAll)
      rows <- keys.toList.infoBarTraverse("Constructing rows") { case (a, b) =>
        IO {
          val prf1 = stats1(a)(b)
          val prf2 = stats2(a)(b)

          List(
            a.show, b.show,
            prf1.pseudocount.toInt,
            prf2.f1 - prf1.f1,
            Evaluation.calculateSmoothedF1(prf2, mean2, 10) -
              Evaluation.calculateSmoothedF1(prf1, mean1, 10),
            Evaluation.calculateSmoothedF1(prf2, mean2, 100) -
              Evaluation.calculateSmoothedF1(prf1, mean1, 100)
          )
        }
      }
      file <- IO(new java.io.File(path.toString))
      writer <- IO(CSVWriter.open(file))
      _ <- IO(writer.writeRow(headers))
      _ <- IO(rows.foreach(writer.writeRow))
      _ <- IO(writer.close())
    } yield ()
  }

  def writePrfTableCsv[A : Show, B : Show](
    path: java.nio.file.Path,
    firstHeader: String,
    secondHeader: String,
    stats: Map[A, Map[B, WeightedPR]]
  ): IO[Unit] = IO {

    def prfCells(prf: WeightedPR, all: WeightedPR) = {
      val total = all.pseudocount
      val totalError = 1.0 - all.f1

      val (p, r, f1) = prf.prf
      val pcount = prf.pseudocount
      def smoothedF1(n: Int) = Evaluation.calculateSmoothedF1(prf, all, n.toDouble)

      val percent = pcount / total
      val error = 1.0 - f1
      val weightedError = error * percent
      val percentError = weightedError / totalError
      List(
        f"$p%.2f",
        f"$r%.2f",
        f"$f1%.2f",
        pcount.toInt.toString,
        f"$percent%.4f",
        f"$error%.4f",
        f"$weightedError%.4f",
        f"$percentError%.4f",
        f"${smoothedF1(10)}%.2f",
        f"${smoothedF1(50)}%.2f",
        f"${smoothedF1(100)}%.2f"
      )
    }

    val headers = List(
      firstHeader,
      secondHeader,
      "Precision",
      "Recall",
      "F1",
      "Count",
      "Percent",
      "Error",
      "Weighted Error",
      "Pct Error",
      "Smoothed F1 (10)",
      "Smoothed F1 (50)",
      "Smoothed F1 (100)"
    )

    val all = stats.values.toList.foldMap(_.values.toList.combineAll)
    val rows = List(
      "All" :: "All" :: prfCells(all, all)
    ) ++ stats.toList.flatMap {
      case (a, bs) =>
        bs.map { case (b, prf) =>
          List(a.show, b.show) ++ prfCells(prf, all)
        }
      }

    val file = new java.io.File(path.toString)

    val writer = CSVWriter.open(file)
    writer.writeRow(headers)
    rows.foreach(writer.writeRow)
    writer.close()
  }
}
