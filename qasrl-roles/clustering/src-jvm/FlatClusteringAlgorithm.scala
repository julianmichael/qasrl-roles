package qasrl.roles.clustering

import jjm.implicits._

import cats.Foldable
import cats.data.NonEmptyVector
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

import scala.collection.immutable.Vector
import scala.annotation.tailrec

import breeze.stats.distributions.Multinomial
import breeze.linalg._
import breeze.numerics._

trait FlatClusteringAlgorithm {
  type ClusterParam
  type Index

  // override for efficiency
  def getSingleInstanceParameter(
    index: Index,
  ): ClusterParam = {
    estimateParameterHard(Vector(index))
  }

  def getInstanceLoss(
    index: Index,
    param: ClusterParam
  ): Double

  def estimateParameterSoft(
    indices: Vector[Index],
    assignmentProbabilities: Vector[Double],
  ): ClusterParam

  // override for efficiency
  def estimateParameterHard(
    indices: Vector[Index],
  ): ClusterParam = {
    estimateParameterSoft(indices, indices.as(1.0))
  }

  // // override eg for max
  // def aggregateLosses(
  //   losses: Vector[Double]
  // ): Double = losses.sum

  // // override eg for max
  // def getLossChangePriority(
  //   newLoss: Double,
  //   leftLoss: Double,
  //   rightLoss: Double
  // ) = newLoss - leftLoss - rightLoss

  // TODO: k-means|| init as well, and maybe random init?
  // TODO: perhaps make matrices out of getInstanceLoss for more efficiency

  def initPlusPlus(
    indices: Vector[Index],
    numClusters: Int)(
  ): Vector[ClusterParam] = {
    val rand = new scala.util.Random()
    assert(numClusters >= 1)
    // TODO: maybe not do unique instances? maybe take them as an input? idk
    val uniqueInstances = indices.toVector.groupBy(x => x).keys.toVector
    if(uniqueInstances.size <= numClusters) {
      uniqueInstances.map(getSingleInstanceParameter)
    } else {
      val firstClusterIndex = rand.nextInt(uniqueInstances.size.toInt)
      val firstCluster = getSingleInstanceParameter(uniqueInstances(firstClusterIndex))

      val initMinLosses = DenseVector(uniqueInstances.map(getInstanceLoss(_, firstCluster)).toArray)

      val (params, minLosses) = initPlusPlusAux(uniqueInstances, Set(), Vector(firstCluster), numClusters - 1, initMinLosses, rand)
      if(minLosses.activeValuesIterator.exists(v => v.isNaN || v.isInfinite)) {
        System.err.println(s"Warning: NaN/Infinite loss items remain after initializing $numClusters clusters for ${indices.size} elements")
        System.err.println(minLosses.activeValuesIterator.map(x => f"$x%.2f").mkString(","))
      }
      params
    }
  }

  @tailrec
  private[this] def initPlusPlusAux(
    indices: Vector[Index],
    chosenInstances: Set[Index], curParams: Vector[ClusterParam], numClustersLeft: Int, curMinLosses: DenseVector[Double],
    rand: scala.util.Random
  ): (Vector[ClusterParam], DenseVector[Double]) = {
    if(numClustersLeft <= 0) (curParams -> curMinLosses) else {
      val infiniteLossIndexIndices = curMinLosses.activeIterator
        .collect { case (index, minLoss) if minLoss.isPosInfinity => index }
        .toVector
      // already-picked instances shouldn't have infinite loss
      require(infiniteLossIndexIndices.forall(i => !chosenInstances.contains(indices(i))))
      val newCenterIndex = if(infiniteLossIndexIndices.nonEmpty) {
        indices(infiniteLossIndexIndices(rand.nextInt(infiniteLossIndexIndices.size)))
      } else {
        val newCenterProbs = if(sum(curMinLosses) == 0.0) {
          Multinomial(DenseVector.ones[Double](curMinLosses.size))
        } else Multinomial(curMinLosses)
        var newIndex = indices(newCenterProbs.draw)
        // XXX I expect this to produce an infinite loop when we have too few instances... is that right? why doesn't this happen already?
        while(chosenInstances.contains(newIndex)) {
          newIndex = indices(newCenterProbs.draw)
        }
        newIndex
      }
      // System.err.println(s"cur min losses: $curMinLosses")
      // instances.foreach(i => println(i.toString.take(200)))
      val newCluster = getSingleInstanceParameter(newCenterIndex)
      val clusterLosses = DenseVector(indices.map(getInstanceLoss(_, newCluster)).toArray)
      val newMinLosses = min(curMinLosses, clusterLosses)
      initPlusPlusAux(indices, chosenInstances + newCenterIndex, curParams ++ Vector(newCluster), numClustersLeft - 1, newMinLosses, rand)
    }
  }

  def softEStep(
    indices: Vector[Index],
    model: Vector[ClusterParam],
    temperature: Double = 1.0,
    clusterPrior: DenseMultinomial
  ): (Vector[DenseMultinomial], Vector[Double]) = {
    val clusterLogProbs = log(clusterPrior.params /:/ clusterPrior.sum)
    indices.map { index =>
      // compute negative log likelihood of the data under each assignment
      val negLogLikelihoods = DenseVector(model.map(p => getInstanceLoss(index, p)).toArray) - clusterLogProbs
      // assign to clusters using a temperature adjustment on the likelihoods
      val probs = {
        val logits = negLogLikelihoods *:* (-1.0 / temperature)
        exp(logits - logSumExp(logits))
      }
      // compute loss by expectation of NLL over the assignment
      val totalLoss = sum(probs *:* negLogLikelihoods)
      Multinomial(probs) -> totalLoss
    }.unzip
  }

  def softMStep(
    numClusters: Int,
    indices: Vector[Index],
    assignments: Vector[DenseMultinomial]
  ): Vector[ClusterParam] = {
    (0 until numClusters).toVector.map { clusterIndex =>
      val clusterProbs = assignments.map(_.probabilityOf(clusterIndex))
      estimateParameterSoft(indices, clusterProbs)
    }
  }

  import cats.Monad
  import cats.effect.IO
  import cats.effect.concurrent.Ref

  import freelog.EphemeralTreeLogger
  import freelog.implicits._

  def runSoftEM(
    initModel: Vector[ClusterParam],
    indices: Vector[Index],
    stoppingThreshold: Double,
    temperatureSchedule: Int => Double = (n: Int) => 1.0,
    estimateClusterPrior: DenseVector[Double] => DenseMultinomial = v => Multinomial(DenseVector.ones(v.size)))(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[(Vector[ClusterParam], Vector[DenseMultinomial], Double)] = {
    val numClusters = initModel.size
    val (initAssignments, initStepLosses) = softEStep(indices, initModel, temperatureSchedule(0), estimateClusterPrior(DenseVector.ones(numClusters)))
    for {
      assignmentsAndLosses <- Ref[IO].of(initAssignments -> initStepLosses)
      losses <- Ref[IO].of(List(mean(initStepLosses)))
      model <- Ref[IO].of(initModel)
      stepNum <- Ref[IO].of(0)
      shouldContinue = losses.get.map {
        case last :: secondLast :: _ => (secondLast - last) > stoppingThreshold
        case _ => true
      }
      _ <- Log.traceBranch("Running Soft EM") {
        Monad[IO].whileM_(shouldContinue) {
          for {
            _ <- Log.rewind
            curStepNum <- stepNum.updateAndGet(_ + 1)
            curAssignments <- assignmentsAndLosses.get.map(_._1)
            curModel <- model.updateAndGet(_ => softMStep(numClusters, indices, curAssignments))
            clusterPrior = estimateClusterPrior(
              curAssignments.map(_.params).reduce(_ + _) /:/ curAssignments.foldMap(_.sum)
            )
            (newAssignments, newLosses) <- assignmentsAndLosses.updateAndGet(_ =>
              softEStep(indices, curModel, temperatureSchedule(curStepNum), clusterPrior)
            )
            newLoss = mean(newLosses)
            _ <- losses.update(newLoss :: _)
            _ <- Log.trace {
              val empiricalPrior = newAssignments.map(_.params).reduce(_ + _) /:/ newAssignments.foldMap(_.sum)
              val priorString = empiricalPrior.toScalaVector.sortBy(-_).take(10).map(x => f"$x%.3f").mkString(", ") + " ..."
              s"""Step $curStepNum
                 |Loss: $newLoss
                 |Prior: $priorString
               """.trim.stripMargin
            }
          } yield ()
        }
      }
      finalModel <- model.get
      finalAssignments <- assignmentsAndLosses.get.map(_._1)
      finalLoss <- losses.get.map(_.head)
    } yield (finalModel, finalAssignments, finalLoss)
  }

  // def runSoftEM(
  //   initModel: Vector[ClusterParam],
  //   indices: Vector[Index],
  //   stoppingThreshold: Double,
  //   estimateClusterPrior: (Map[Int, Int], Int) => DenseMultinomial = (_, numClusters) => Multinomial(DenseVector.ones(numClusters)))(
  //   implicit Log: EphemeralTreeLogger[IO, String]
  // ): (Vector[ClusterParam], Vector[DenseMultinomial], Double) = {
  //   def sendLog(x: String) = Log.trace(x).unsafeRunSync()
  //   IO {
  //     var (assignments, stepLosses) = softEStep(indices, initModel, temperatureSchedule(0))
  //     var losses: List[Double] = List(mean(stepLosses))
  //     var model: Vector[ClusterParam] = initModel
  //     var stepNum = 1
  //     def getDelta = (losses.get(1), losses.get(0)).mapN(_ - _)
  //     def shouldContinue = getDelta.forall(_ > stoppingThreshold)
  //     while(shouldContinue) {
  //       model = softMStep(model.size, indices, assignments)
  //       val p = softEStep(indices, model, temperatureSchedule(stepNum))
  //       assignments = p._1
  //       stepLosses = p._2
  //       val loss = mean(stepLosses)
  //       losses = loss :: losses
  //       stepNum = stepNum + 1

  //       sendLog("=== Stepping ===")
  //       val prior = assignments.map(a => a.params / a.sum).reduce(_ + _) / assignments.size.toDouble
  //       sendLog(s"Prior: " + prior.toScalaVector.sortBy(-_).take(30).map(x => f"$x%.3f").mkString(", "))
  //       sendLog(s"Loss: $loss")
  //     }
  //     (model, assignments, losses.head)
  //   }.unsafeRunSync()
  // }


  def hardEStep(
    indices: Vector[Index],
    model: Vector[ClusterParam],
    clusterPrior: DenseMultinomial)(
    implicit rand: scala.util.Random
  ): (Vector[Int], Vector[Double]) = {
    indices.map { i =>
      val clusterLosses = model.zipWithIndex.map { case (clusterParam, clusterIndex) =>
        getInstanceLoss(i, clusterParam) - clusterPrior.logProbabilityOf(clusterIndex)
      }
      val best = clusterLosses.zipWithIndex.minimaBy(_._1)
      best(rand.nextInt(best.size)).swap
    }.unzip
  }

  def hardMStep(
    numClusters: Int,
    indices: Vector[Index],
    assignments: Vector[Int]
  ): Vector[ClusterParam] = {
    (0 until numClusters).toVector.map { clusterIndex =>
      estimateParameterHard(
        assignments.zipWithIndex.filter(_._1 == clusterIndex).map(p => indices(p._2))
      )
    }
  }

  def runHardEM(
    initModel: Vector[ClusterParam],
    indices: Vector[Index],
    stoppingThreshold: Double,
    estimateClusterPrior: (Map[Int, Int], Int) => DenseMultinomial = (_, numClusters) => Multinomial(DenseVector.ones(numClusters)))(
    implicit Log: EphemeralTreeLogger[IO, String]
  ): IO[(Vector[ClusterParam], Vector[Int], Double)] = {
    implicit val rand = new scala.util.Random()
    val numClusters = initModel.size

    if(numClusters == 0) IO.pure(Vector(), Vector(), 0.0) else {
      val (initAssignments, initLosses) = hardEStep(
        indices,
        initModel,
        estimateClusterPrior(initModel.indices.map(_ -> 1).toMap, initModel.size)
      )
      for {
        assignmentsAndLosses <- Ref[IO].of(initAssignments -> initLosses)
        losses <- Ref[IO].of(List(mean(initLosses)))
        model <- Ref[IO].of(initModel)
        stepNum <- Ref[IO].of(1)
        shouldContinue = losses.get.map {
          case last :: secondLast :: _ => (secondLast - last) > stoppingThreshold
          case _ => true
        }
        _ <- Log.traceBranch("Running Hard EM") {
          Monad[IO].whileM_(shouldContinue) {
            for {
              _ <- Log.rewind
              curAssignments <- assignmentsAndLosses.get.map(_._1)
              curModel <- model.updateAndGet(_ => hardMStep(numClusters, indices, curAssignments))
              // curModel <- model.updateAndGet(_ => hardMStep(numClusters, indices, curAssignments))
              clusterPrior = estimateClusterPrior(curAssignments.counts, numClusters)
              (newAssignments, newLosses) <- assignmentsAndLosses.updateAndGet(_ => hardEStep(indices, curModel, clusterPrior))
              newLoss = mean(newLosses)
              _ <- losses.update(newLoss :: _)
              curStepNum <- stepNum.getAndUpdate(_ + 1)
              _ <- Log.trace {
                val prior = newAssignments.counts.values.toVector.map(_ / newAssignments.size.toDouble)
                val diversity = exp(prior.foldMap(p => -p*log(p)))
                val priorString = prior.sortBy(-_).take(10).map(x => f"$x%.3f").mkString(", ") + " ..."
                s"""Step $curStepNum
                |Loss: $newLoss
                |Diversity: $diversity
                |Prior: $priorString
              """.trim.stripMargin
              }
            } yield ()
          }
        }
        finalModel <- model.get
        finalAssignments <- assignmentsAndLosses.get.map(_._1)
        finalLoss <- losses.get.map(_.head)
      } yield (finalModel, finalAssignments, finalLoss)
    }
  }
}
