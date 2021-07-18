package qasrl.roles.browse

import qasrl.roles.modeling.ClusterSplittingCriterion

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

@Lenses @JsonCodec case class ClusterSplittingSpec(
  verbCriterion: ClusterSplittingCriterion,
  argumentCriterion: ClusterSplittingCriterion
)
object ClusterSplittingSpec

