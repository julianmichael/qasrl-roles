package qasrl.roles.clustering

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

object MixtureOfUnigrams {

  type Counts = Map[Int, Int]
  // assume sum to 1
  type Dist = Vector[Double]

  // prior.size == frames.size
  // all frames(i).size equal
  @JsonCodec case class UnigramMixtureModel(
    prior: Dist,
    clusters: Vector[Dist]) {
    def numClusters = prior.size
    def numItems = clusters.head.size
  }
  object UnigramMixtureModel {
    def initRandom(numClusters: Int, numItems: Int, rand: Random): UnigramMixtureModel = {
      val clusterInitNums = (1 to numItems).toVector
      val numsTotal = clusterInitNums.sum
      val clusterInitProbs = clusterInitNums.map(_.toDouble / numsTotal)
      UnigramMixtureModel(
        prior = uniform(numClusters),
        clusters = Vector.fill(numClusters)(rand.shuffle(clusterInitProbs))
      )
    }

    def initClever(instances: List[Counts], numClusters: Int, numItems: Int, clusterConcentrationParameter: Double, rand: Random) = {
      assert(numClusters >= 1)
      val firstCluster = dirichletPosteriorFromSparse(instances(rand.nextInt(instances.size)), numItems, clusterConcentrationParameter)
      val initModel = UnigramMixtureModel(
        prior = Vector(1.0),
        clusters = Vector(firstCluster))
      val uniqueInstances = instances.groupBy(x => x).keys.toList
      initCleverAux(uniqueInstances, numItems, clusterConcentrationParameter, initModel, numClusters - 1, rand)
    }
    def initCleverAux(instances: List[Counts], numItems: Int, clusterConcentrationParameter: Double, prevModel: UnigramMixtureModel, numClustersLeft: Int, rand: Random): UnigramMixtureModel = {
      if(numClustersLeft <= 0) prevModel else {
        val (_, nlls) = softEStep(instances, prevModel)
        val totalNLL = nlls.sum
        val normalizedNLLs = nlls.map(_ / nlls.sum)
        val newCenterIndex = sample(normalizedNLLs, rand)
        val newCenter = dirichletPosteriorFromSparse(instances(newCenterIndex), numItems, clusterConcentrationParameter)
        val newNumClusters = prevModel.numClusters + 1
        val newModel = UnigramMixtureModel(
          prior = Vector.fill(newNumClusters)(1.0 / newNumClusters),
          clusters = prevModel.clusters :+ newCenter
        )
        initCleverAux(instances.take(newCenterIndex) ++ instances.drop(newCenterIndex + 1), numItems, clusterConcentrationParameter, newModel, numClustersLeft - 1, rand)
      }
    }
  }

  def softEStep(
    instances: List[Counts],
    model: UnigramMixtureModel
  ): (List[Dist], List[Double]) = {
    val (assignments, nlls) = instances.map { instance =>
      val unnormClusterLogProbs = model.prior.indices.map { clusterNum =>
        instance.iterator.map { case (itemNum, itemCount) =>
          itemCount * (math.log(model.prior(clusterNum)) + math.log(model.clusters(clusterNum)(itemNum)))
        }.sum
      }.toVector
      val logLikelihood = logSumExp(unnormClusterLogProbs)
      val clusterProbs = unnormClusterLogProbs.map(logProb => math.exp(logProb - logLikelihood))
      val negLogLikelihood = -math.log(logLikelihood)
      clusterProbs -> (-logLikelihood)
    }.unzip
    assignments -> nlls
  }

  def softMStep(
    numItems: Int,
    instances: List[Counts],
    assignments: List[Dist],
    priorConcentrationParameter: Double,
    clusterConcentrationParameter: Double
  ): UnigramMixtureModel = {
    // add-1 smooth prior
    val clusterPseudoCounts = assignments.transpose.map(_.sum).toVector
    val prior = dirichletPosteriorFromDense(clusterPseudoCounts, priorConcentrationParameter)
    // val prior = assignments.transpose.map(clusterCounts => (clusterCounts.sum + 1) / (assignments.size + assignments.head.size)).toVector
    val iaPairs = instances.zip(assignments)
    val clusters = assignments.head.indices.map { clusterNum =>
      val pseudoCounts = iaPairs.foldMap { case (instance, assignment) =>
        instance.map { case (itemNum, count) =>
          itemNum -> (assignment(clusterNum) * count)
        }
      }
      dirichletPosteriorFromSparse(pseudoCounts, numItems, clusterConcentrationParameter)
    }.toVector
    UnigramMixtureModel(prior, clusters)
  }

  def runSoftEM(
    initModel: UnigramMixtureModel,
    instances: List[Counts],
    priorConcentrationParameter: Double,
    clusterConcentrationParameter: Double,
    stoppingThreshold: Double,
    shouldLog: Boolean = true
  ): (UnigramMixtureModel, List[Vector[Double]], Double) = {
    var (assignments, stepLosses) = softEStep(instances, initModel)
    var losses: List[Double] = List(mean(stepLosses))
    var model: UnigramMixtureModel = initModel
    def getDelta = (losses.get(1), losses.get(0)).mapN(_ - _)
    def shouldContinue = getDelta.forall(_ > stoppingThreshold)
    while(shouldContinue) {
      model = softMStep(model.numItems, instances, assignments, priorConcentrationParameter, clusterConcentrationParameter)
      val p = softEStep(instances, model)
      assignments = p._1
      stepLosses = p._2
      val loss = mean(stepLosses)
      losses = loss :: losses
      if(shouldLog) {
        println("=== Stepping ===")
        println(s"Prior: " + model.prior.sortBy(-_).take(30).map(x => f"$x%.3f").mkString(", "))
        println(s"Loss: $loss")
      }
    }
    if(shouldLog) {
      println("=== Stopped ===")
    }
    (model, assignments, losses.head)
  }
}
