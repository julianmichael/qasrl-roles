package qasrl.roles.browse

import qasrl.roles.modeling.DataSetting
import qasrl.roles.modeling.RunMode

import scalatags.Text.all.Frag
import java.nio.file.Path

case class PageConfig(
  docApiUrl: String,
  verbApiUrl: String,
  featureApiUrl: String,
  bootstrapLink: Frag,
  bootstrapScripts: Frag,
  dataSetting: DataSetting,
  mode: RunMode,
  jsDepsPath: String,
  jsPath: String
)
