package qasrl.roles.modeling.eval

import jjm.metrics._

import cats.Show
import cats.implicits._

object Html {

  import scalatags.Text.all._

  def prfTableHtml[A : Show](stats: Map[A, WeightedPR]) = {

    def prfCells(prf: WeightedPR, all: WeightedPR) = {
      val total = all.pseudocount
      val totalError = 1.0 - all.f1

      val (p, r, f1) = prf.prf
      val pcount = prf.pseudocount
      val smoothedF1_1 =  Evaluation.calculateSmoothedF1(prf, all, 1.0)
      val smoothedF1_5 =  Evaluation.calculateSmoothedF1(prf, all, 5.0)
      val smoothedF1_10 = Evaluation.calculateSmoothedF1(prf, all, 10.0)
      val smoothedF1_25 = Evaluation.calculateSmoothedF1(prf, all, 25.0)

      val percent = pcount / total
      val error = 1.0 - f1
      val weightedError = error * percent
      val percentError = weightedError / totalError
      List(
        td(f"$p%.2f"),
        td(f"$r%.2f"),
        td(f"$f1%.2f"),
        td(pcount.toInt),
        td(f"$percent%.4f"),
        td(f"$error%.4f"),
        td(f"$weightedError%.4f"),
        td(f"$percentError%.4f"),
        td(f"$smoothedF1_1%.2f"),
        td(f"$smoothedF1_5%.2f"),
        td(f"$smoothedF1_10%.2f"),
        td(f"$smoothedF1_25%.2f")
      )
    }


    def prfTable(
      metricName: String,
      metricByLabel: Map[A, WeightedPR]
    ) = {
      val all = metricByLabel.values.toList.combineAll
      table(
        `class` := "pure-table sortable",
        thead(
          tr(
            td(),
            td("Prec."),
            td("Rec."),
            td("F1"),
            td("Count"),
            td("Percent"),
            td("Error"),
            td("Weighted Error"),
            td("Pct Error"),
            td("Smoothed F1(1)"),
            td("Smoothed F1(5)"),
            td("Smoothed F1(10)"),
            td("Smoothed F1(25)"),
          )
        ),
        tbody(
          tr(td("All"))(prfCells(all, all): _*)
        )(
          metricByLabel.toList.sortBy(-_._2.pseudocount).map { case (label, prf) =>
            tr(td(label.show))(prfCells(prf, all): _*)
          }: _*
        )
      )
    }

    html(
      head(
        link(
          rel := "stylesheet",
          href := "https://unpkg.com/purecss@2.0.3/build/pure-min.css"
        ),
        script(
          src := "https://www.kryogenix.org/code/browser/sorttable/sorttable.js"
        ),
        scalatags.Text.tags2.style(
          """tbody tr:nth-child(odd) { background-color: #eee; }"""
        )
        // style()
      ),
      body(
        prfTable("B3 P/R/F1", stats)
      )
    )
  }
}
