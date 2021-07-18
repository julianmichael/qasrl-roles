package qasrl.roles.modeling

import qasrl.roles.clustering.MergeTree

import jjm.implicits._
import cats.implicits._

import io.circe.generic.JsonCodec

@JsonCodec case class VerbClusterModel[VerbType, Arg](
  verbType: VerbType,
  verbClustering: Clustering.Verb,
  argumentClustering: Clustering.Argument[Arg]
) {
  val numVerbInstances = verbClustering.size
}
object VerbClusterModel

@JsonCodec case class Clustering[Instance](
  clusterTreeOpt: Option[MergeTree[Set[Instance]]],
  extraClusters: Map[String, Set[Instance]] = Map.empty[String, Set[Instance]]
) {
  def size = extraClusters.unorderedFoldMap(_.size) + clusterTreeOpt.foldMap(_.unorderedFoldMap(_.size))

  def nonEmpty = clusterTreeOpt.exists(_.exists(_.nonEmpty)) || extraClusters.values.exists(_.nonEmpty)

  def split[A](f: Instance => A): Map[A, Clustering[Instance]] = {
    val treesOpt = clusterTreeOpt.map(_.group(_.groupBy(f)))
    val extras = extraClusters
      .mapVals(_.groupBy(f)).toList
      .foldMap { case (label, groups) =>
        groups.mapVals(argSet => Map(label -> argSet))
      }
    val keys = treesOpt.foldMap(_.keySet) ++ extras.keySet

    keys.iterator.map(key =>
      key -> Clustering(treesOpt.flatMap(_.get(key)), extras.get(key).combineAll)
    ).toMap
  }
}
object Clustering {
  type Argument[Arg] = Clustering[ArgumentId[Arg]]
  type Verb = Clustering[VerbId]
}
