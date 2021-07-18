package qasrl.roles.modeling.eval

import qasrl.roles.modeling._

import jjm.Duad
import jjm.implicits._

import cats.Order
import cats.implicits._
import cats.effect.IO

import freelog.EphemeralTreeLogger
import freelog.implicits._

object EvalUtils {

  val conll08RoleIndices = List(
    "A0", "A1", "A2", "A3", "A4",
    "AM-LOC", "AM-MNR",
    "AM-ADV",
    "AM-PNC",
    "AM-CAU",
    "AM-TMP",
    "AM-DIS",
    "AM-DIR", "AM-EXT",
    "AM-MOD", "AM-NEG",
    "AM-PRD",
    "A5", "AA",
  ).zipWithIndex.toMap
  val conll08RoleOrder = Order.whenEqual(
    Order.by[String, Int](x => conll08RoleIndices.getOrElse(x, 100)),
    Order[String]
  )

  // TODO: enhance into more general cooccurrence utility?
  // first step: take weighted groups to make the pmi/npmi calculation more straightforward.
  // second step: maybe put together into a general cooccurrence utility.

  // sourceDistribution is a probability distribution over sources where the pair appears.
  // TODO: change this to compute sample covariance correctly (with n-1 denom)
  case class NPMIResult[A](
    count: Double,
    prob: Double,
    pmi: Double,
    npmi: Double,
    covariance: Double,
    correlation: Double,
    sourceDistribution: Map[A, Double]
  )
  object NPMIResult {
    private[EvalUtils] def never[A](covariance: Double, correlation: Double) = NPMIResult[A](
      count = 0.0,
      prob = 0.0,
      pmi = Double.NegativeInfinity,
      npmi = -1.0,
      covariance = covariance,
      correlation = correlation,
      sourceDistribution = Map()
    )
  }

  def calculateHeterogeneousNPMIsLoggingEfficient[Source, X: Order, Y: Order, N: Numeric](
    groupings: Vector[(Source, Double, Map[X, Double], Map[Y, Double])])(
    implicit N: Numeric[N],
    Log: freelog.SequentialEphemeralTreeLogger[IO, String]
  ): IO[Map[(X, Y), NPMIResult[Source]]] = {
    import freelog.implicits._
    import scala.math.{pow, log, sqrt}
    for {
      // transform to probs. TODO: use a type for it in the input.
      groups <- groupings.traceBarTraverse("Normalizing input distributions") {
        case (source, weight, xs, ys) =>
          cats.effect.IO(
            (source, weight,
             xs.normalize.withDefaultValue(0.0),
             ys.normalize.withDefaultValue(0.0))
          )
      }
      result <- {
        val xLabels = groups.foldMap(_._3.keySet)
        val yLabels = groups.foldMap(_._4.keySet)
        val groupWeights = groups.map(_._2)
        val total = groupWeights.combineAll
        val xMarginalProbs = groups.foldMap { case (_, weight, xProbs, _) =>
          xProbs.mapVals(_ * weight)
        }.mapVals(_ / total)
        val yMarginalProbs = groups.foldMap { case (_, weight, _, yProbs) =>
          yProbs.mapVals(_ * weight)
        }.mapVals(_ / total)
        val pairs = for(x <- xLabels; y <- yLabels) yield (x, y)

        // joint of (x, y) when we choose instance according to weight and sample x and y at random.
        groups.infoBarFoldMapM("Computing joint probabilities") { case (source, weight, xProbs, yProbs) =>
          cats.effect.IO {
            xProbs.toList.foldMap { case (x, xProb) =>
              yProbs.map { case (y, yProb) =>
                (x, y) -> Map(source -> (xProb * yProb * weight))
              }
            }.mapVals(_.mapVals(_ / total))
          }
        }.flatMap { jointProbsWithSources =>
          val xStdevs = xMarginalProbs.map { case (x, marginal) =>
            val selfJointProb = groups.foldMap { case(_, w, p, _) => p(x) * p(x) * w } / total
            x -> sqrt(selfJointProb - pow(marginal, 2))
          }
          val yStdevs = yMarginalProbs.map { case (y, marginal) =>
            val selfJointProb = groups.foldMap { case(_, w, _, p) => p(y) * p(y) * w } / total
            y -> sqrt(selfJointProb - pow(marginal, 2))
          }
          Log.info(s"${pairs.size} pairs to compute.") >>
          pairs.toList.infoBarTraverse("Computing NPMIs") { case (x, y) =>
            cats.effect.IO {
              // joint of (x, y) when we choose an x at random and then choose a random y in x's cluster
              val sources = jointProbsWithSources.getOrElse((x, y), Map())
              val jointProb = sources.unorderedFold
              val independentProb = xMarginalProbs(x) * yMarginalProbs(y)
              val covariance = jointProb - independentProb
              val correlation = covariance / (xStdevs(x) * yStdevs(y))
              val result = if(jointProb == 0.0) NPMIResult.never[Source](covariance, correlation) else {
                assert(independentProb != 0.0)
                val logJointProb = log(jointProb)
                val pmi = logJointProb - log(independentProb)
                val npmi = pmi / (-logJointProb)
                NPMIResult(
                  count = jointProb * total,
                  prob = jointProb,
                  pmi = pmi,
                  npmi = npmi,
                  covariance = covariance,
                  correlation = correlation,
                  sourceDistribution = sources)
              }
              (x, y) -> result
            }
          }.map(_.toMap)
        }
      }
    } yield result
  }

  def calculateNPMIsLoggingEfficient[Source, A: Order, N: Numeric](
    groupings: Vector[(Source, Map[A, N])])(
    implicit N: Numeric[N],
    Log: freelog.SequentialEphemeralTreeLogger[cats.effect.IO, String]
  ): cats.effect.IO[Map[Duad[A], NPMIResult[Source]]] = {
    import freelog.implicits._
    val groups = groupings.map(_._2.mapVals(N.toDouble))
    import scala.math.{pow, log, sqrt}
    val labels = groups.foldMap(_.keySet)
    val groupSizes = groups.map(_.unorderedFold)
    val total = groupSizes.combineAll
    val totalPairs = groupSizes.foldMap(pow(_, 2))
    val marginals = groups.foldMap { counts =>
      val size = counts.unorderedFold
      counts.mapVals(_ * size)
    }
    val pairs = for(x <- labels; y <- labels) yield Duad(x, y)

    val marginalProbs = groups.combineAll.mapVals(_ / total)

    // joint of (x, y) when we choose an x at random and then choose a random y in x's cluster
    groupings.infoBarFoldMapM("Computing joint probabilities") { case (source, _goldCounts) =>
      val goldCounts = _goldCounts.mapVals(N.toDouble)
      cats.effect.IO {
        val groupTotal = goldCounts.unorderedFold
        goldCounts.toList.foldMap { case (left, leftCount) =>
          val leftProb = leftCount / total
          goldCounts.filter(_._1 >= left).map { case (right, rightCount) =>
            val rightProb = rightCount / groupTotal
            Duad(left, right) -> Map(source -> (leftProb * rightProb))
          }
        }
      }
    }.flatMap { jointProbsWithSources =>
      val stdevs = marginalProbs.map { case (a, marginal) =>
        val selfJointProb = jointProbsWithSources(Duad(a, a)).unorderedFold
        a -> sqrt(selfJointProb - pow(marginal, 2))
      }
      Log.info(s"${pairs.size} pairs to compute.") >>
      pairs.toList.infoBarTraverse("Computing NPMIs") { pair =>
        cats.effect.IO {
          // joint of (x, y) when we choose an x at random and then choose a random y in x's cluster
          val sources = jointProbsWithSources.getOrElse(pair, Map())
          val jointProb = sources.unorderedFold
          val independentProb = {
            marginalProbs(pair.min) * marginalProbs(pair.max)
          }
          val covariance = jointProb - independentProb
          val correlation = covariance / (stdevs(pair.min) * stdevs(pair.max))
          val result = if(jointProb == 0.0) NPMIResult.never[Source](covariance, correlation) else {
            assert(independentProb != 0.0)
            val logJointProb = log(jointProb)
            val pmi = logJointProb - log(independentProb)
            val npmi = pmi / (-logJointProb)
            NPMIResult(
              count = jointProb * total,
              prob = jointProb,
              pmi = pmi,
              npmi = npmi,
              covariance = covariance,
              correlation = correlation,
              sourceDistribution = sources)
          }
          pair -> result
        }
      }.map(_.toMap)
    }
  }

  def calculateNPMIs[A: Order, N: Numeric](
    groupings: Vector[Map[A, N]]
  ): Map[Duad[A], Double] = {
    val groups = groupings.map(_.mapVals(implicitly[Numeric[N]].toDouble))
    import scala.math.{pow, log}
    val labels = groups.foldMap(_.keySet)
    val groupSizes = groups.map(_.unorderedFold)
    val total = groupSizes.combineAll
    val totalPairs = groupSizes.foldMap(pow(_, 2))
    val marginals = groups.foldMap { counts =>
      val size = counts.unorderedFold
      counts.mapVals(_ * size)
    }
    val pairs = for(x <- labels; y <- labels) yield Duad(x, y)

    val marginalProbs = groups.combineAll.mapVals(_ / total)

    pairs.iterator.map { pair =>
      // joint of (x, y) when we choose an x at random and then choose a random y in x's cluster
      val prob = groups.foldMap { goldCounts =>
        val leftCounts = goldCounts.getOrElse(pair.min, 0.0)
        val rightCounts = goldCounts.getOrElse(pair.max, 0.0)
        (leftCounts * rightCounts).toDouble / goldCounts.unorderedFold
      } / total
      val independentProb = {
        marginalProbs(pair.min) * marginalProbs(pair.max)// / (totalPairs * totalPairs)
      }
      val npmi = if(prob == 0.0) {
        if(independentProb == 0.0) {
          assert(false) // should never happen
          0.0
        } else -1.0
      } else {
        val logJointProb = log(prob)
        val pmi = logJointProb - log(independentProb)
        pmi / (-logJointProb)
      }
      pair -> npmi
    }.toMap
  }

  // here the "independent" case gets to assume the two are drawn from the same verb,
  // basically forming a stronger baseline.
  def calculateAggregateNPMIs[A: Order](
    clusterings: Vector[Vector[Map[A, Int]]]
  ): Map[Duad[A], NPMIResult[Nothing]] = {
    val allClusterings = clusterings.flatten
    val localCounts = clusterings.map(_.unorderedFold)
    val total = allClusterings.foldMap(_.unorderedFold)
    val labels = allClusterings.foldMap(_.keySet)
    val pairs = for(x <- labels; y <- labels) yield Duad(x, y)

    pairs.iterator.map { pair =>
      // generative process: pick one point at random (left counts / total),
      // then pick another in its same cluster (right counts / cluster size)
      val jointProb = allClusterings.foldMap { goldCounts =>
        val leftCounts = goldCounts.getOrElse(pair.min, 0)
        val rightCounts = goldCounts.getOrElse(pair.max, 0)
        val subtotal = goldCounts.unorderedFold
        if(subtotal == 0) 0.0 else {
          (leftCounts * rightCounts).toDouble / subtotal
        }
      } / total

      // generative process: pick one point at random (left counts / total),
      // then look at the probability of picking the same label randomly among that verb (marginal / verb total).
      // incidentally, it looks exactly like the above, after collapsing all sub-verb clusterings. huh
      val independentProb = localCounts.foldMap { goldCounts =>
        val leftCounts = goldCounts.getOrElse(pair.min, 0)
        val rightCounts = goldCounts.getOrElse(pair.max, 0)
        val subtotal = goldCounts.unorderedFold
        if(subtotal == 0) 0.0 else {
          (leftCounts * rightCounts).toDouble / subtotal
        }
      } / total

      val npmi = if(jointProb == 0.0) NPMIResult.never[Nothing](0.0, 0.0) else {
        // import scala.math.log
        val logJointProb = scala.math.log(jointProb)
        val pmi = logJointProb - scala.math.log(independentProb)
        pmi / -logJointProb

        NPMIResult[Nothing](
          count = jointProb * total,
          prob = jointProb,
          pmi = pmi,
          npmi = pmi / -logJointProb,
          covariance = 0.0, // TODO
          correlation = 0.0, // TODO
          sourceDistribution = Map() // TODO
        )
      }
      pair -> npmi
    }.toMap
  }
}
