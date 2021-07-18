package qasrl.roles

import jjm.DotKleisli

package object browse {
  type FeatureService[F[_], VerbType, Arg] = DotKleisli[F, FeatureReq[VerbType, Arg]]
}
