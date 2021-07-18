package qasrl.roles.clustering

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Random

import io.circe.generic.JsonCodec

// Actually this is LDA lol
object LDA {

  type Counts = Map[Int, Int]
  // assume sum to 1
  type Dist = Vector[Double]

  // prior.size == frames.size
  // all frames(i).size equal
  @JsonCodec case class LDAModel(
    priors: Vector[Dist], // topic (verb), cluster (frame)
    clusters: Vector[Dist]) { // cluster (frame), clause
    def numClusters = priors.size
    def numItems = clusters.head.size
  }
  object LDAModel {
    def initRandom(numDocuments: Int, numClusters: Int, numItems: Int, rand: Random): LDAModel = {
      val clusterInitNums = (1 to numItems).toVector
      val numsTotal = clusterInitNums.sum
      val clusterInitProbs = clusterInitNums.map(_.toDouble / numsTotal)
      val uniformPrior = Vector.fill(numClusters)(1.0 / numClusters)
      LDAModel(
        priors = Vector.fill(numDocuments)(uniformPrior),
        clusters = Vector.fill(numClusters)(rand.shuffle(clusterInitProbs))
      )
    }

    def initClever(documents: Vector[Vector[Counts]], numClusters: Int, numItems: Int, clusterConcentrationParameter: Double, rand: Random) = {
      val unigramMixtureModel = MixtureOfUnigrams.UnigramMixtureModel.initClever(documents.flatten.toList, numClusters, numItems, clusterConcentrationParameter, rand)
      val uniformPrior = Vector.fill(numClusters)(1.0 / numClusters)
      LDAModel(
        priors = documents.as(uniformPrior),
        clusters = unigramMixtureModel.clusters)
    }
  }

  def softEStep(
    documents: Vector[Vector[Counts]],
    model: LDAModel
  ): (Vector[Vector[Dist]], Vector[Vector[Double]]) = {
    documents.zip(model.priors).map { case (document, docPrior) =>
      document.map { token =>
        val unnormClusterLogProbs = model.clusters.zipWithIndex.map { case (cluster, clusterNum) =>
          token.iterator.map { case (itemNum, itemCount) =>
            itemCount * (math.log(docPrior(clusterNum)) + math.log(cluster(itemNum)))
          }.sum
        }
        val tokenLogLikelihood = logSumExp(unnormClusterLogProbs)
        val clusterProbs = unnormClusterLogProbs.map(logProb => math.exp(logProb - tokenLogLikelihood))
        clusterProbs -> (-tokenLogLikelihood)
      }.unzip
    }.unzip
  }

  def softMStep(
    numItems: Int,
    documents: Vector[Vector[Counts]],
    assignments: Vector[Vector[Dist]],
    priorConcentrationParameter: Double,
    clusterConcentrationParameter: Double
  ): LDAModel = {
    val priors = assignments.map(docAssignments =>
      dirichletPosteriorFromDense(
        docAssignments.transpose.map(clusterCounts =>
          (clusterCounts.sum + 1) / (docAssignments.size + docAssignments.head.size)
        ).toVector,
        priorConcentrationParameter
      )
    )
    val iaPairs = documents.flatten.zip(assignments.flatten)
    val clusters = assignments.head.head.indices.map { clusterNum =>
      val pseudoCounts = iaPairs.foldMap { case (instance, assignment) =>
        instance.map { case (itemNum, count) =>
          itemNum -> (assignment(clusterNum) * count)
        }
      }
      dirichletPosteriorFromSparse(pseudoCounts, numItems, clusterConcentrationParameter)
    }.toVector
    LDAModel(priors, clusters)
  }

  def runSoftEM(
    initModel: LDAModel,
    documents: Vector[Vector[Counts]],
    priorConcentrationParameter: Double,
    clusterConcentrationParameter: Double,
    stoppingThreshold: Double,
    shouldLog: Boolean = true
  ): (LDAModel, Vector[Vector[Dist]], Double) = {
    var (assignments, stepLosses) = softEStep(documents, initModel)
    var losses: List[Double] = List(mean(stepLosses.flatten))
    var model: LDAModel = initModel
    def getDelta = (losses.get(1), losses.get(0)).mapN(_ - _)
    def shouldContinue = getDelta.forall(_ > stoppingThreshold)
    while(shouldContinue) {
      model = softMStep(model.numItems, documents, assignments, priorConcentrationParameter, clusterConcentrationParameter)
      val p = softEStep(documents, model)
      assignments = p._1
      stepLosses = p._2
      val loss = mean(stepLosses.flatten)
      losses = loss :: losses
      if(shouldLog) {
        println("=== Stepping ===")
        val avgPrior = model.priors.transpose.map(x => mean(x.toVector)).toVector
        println(s"Average Prior: " + avgPrior.sortBy(-_).take(30).map(x => f"$x%.3f").mkString(", "))
        println(s"Loss: $loss")
      }
    }
    if(shouldLog) {
      println("=== Stopped ===")
    }
    (model, assignments, losses.head)
  }
}
