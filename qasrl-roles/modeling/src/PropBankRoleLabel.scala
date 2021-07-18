package qasrl.roles.modeling

import jjm.datasets.PropBankPredicate
import jjm.ling.ESpan
import jjm.implicits._

import io.circe.generic.JsonCodec

@JsonCodec case class PropBankRoleLabel(
  framesetId: String,
  role: String
) {
  override def toString = s"$framesetId.$role"
}
object PropBankRoleLabel {

  // the pred itself, discourse markers, negations, and auxiliaries we don't care about
  def roleLabelIsIrrelevant(l: String) = {
    l == "V" || l.contains("DIS") || l.contains("NEG") || l.contains("MOD") ||
      l.contains("C-") || l.contains("R-") ||
      l == "rel"// || l == "Support"
  }

  def isArgRelevant(predIndex: Int, pred: PropBankPredicate, roleLabel: String, argSpan: ESpan) =
    !roleLabelIsIrrelevant(roleLabel) &&
      !Auxiliaries.auxiliaryVerbs.contains(pred.lemma.lowerCase) &&
      !argSpan.contains(predIndex)

  import cats.Order
  import cats.Show
  import cats.implicits._

  implicit val propBankRoleLabelShow = Show.fromToString[PropBankRoleLabel]
  implicit val propBankRoleLabelOrder = Order.by[PropBankRoleLabel, String](_.toString)
}
