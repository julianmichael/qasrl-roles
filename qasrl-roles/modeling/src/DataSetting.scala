package qasrl.roles.modeling

import jjm.ling.ESpan
import jjm.ling.en.InflectedForms

import qasrl.labeling.ClausalQuestion

sealed trait DataSetting {
  type VerbType; type Arg
}
object DataSetting {

  case object Qasrl extends DataSetting {
    type VerbType = InflectedForms; type Arg = ClausalQuestion
    override def toString = "qasrl"
  }
  case class Ontonotes5(assumeGoldVerbSense: Boolean) extends DataSetting {
    type VerbType = String; type Arg = ESpan
    override def toString = {
      val senseLemma = if(assumeGoldVerbSense) "sense" else "lemma"
      s"ontonotes-$senseLemma"
    }
  }
  case class CoNLL08(assumeGoldVerbSense: Boolean) extends DataSetting {
    type VerbType = String; type Arg = Int
    override def toString = {
      val senseLemma = if(assumeGoldVerbSense) "sense" else "lemma"
      s"conll08-$senseLemma"
    }
  }

  def all = List[DataSetting](
    Qasrl, Ontonotes5(false), Ontonotes5(true),
    CoNLL08(false), CoNLL08(true)
  )

  def fromString(x: String): Option[DataSetting] = {
    all.find(_.toString == x)
  }
}
