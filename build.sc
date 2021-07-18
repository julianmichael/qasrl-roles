import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import mill.api.DummyInputStream
import mill.eval.Result
import coursier.maven.MavenRepository

import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

import ammonite.ops._

val thisScalaVersion = "2.12.12"
val thisScalaJSVersion = "1.4.0"

// locally published lib
val evilplotVersion = "0.8.1-SNAPSHOT"

// my libs
val jjmVersion = "0.2.1-SNAPSHOT"
val qasrlVersion = "0.3.1-SNAPSHOT"
val freelogVersion = "0.1.0"

val catsCollectionsVersion = "0.9.1"
val mouseVersion = "0.26.2"

// compiler plugins
val macroParadiseVersion = "2.1.1"
val kindProjectorVersion = "0.11.3"
val splainVersion = "0.3.4"
val betterMonadicForVersion = "0.3.1"

// cats libs
val kittensVersion = "2.2.1"
val declineVersion = "1.3.0"

// non-cats
val breezeVersion = "0.13.2"
val scalaCsvVersion = "1.3.6"
val fastparseVersion = "2.3.1"

// jvm webby libs
val scalatagsVersion = "0.9.3"

// jvm libs
val ammoniteOpsVersion = "1.1.2"
val logbackVersion = "1.2.3"

// js libs
val scalajsDomVersion = "1.1.0"
val scalajsJqueryVersion = "1.0.0"
// val scalajsReactVersion = "1.2.3"
// val scalajsReactVersion = "1.3.1"
val scalacssVersion = "0.7.0"

// test libs
val scalatestVersion = "3.0.8"
val scalacheckVersion = "1.14.0"
val disciplineVersion = "1.0.0"

val munitVersion = "0.7.21"
val munitCatsEffectVersion = "0.11.0"

import $file.scripts.SimpleJSDepsBuild, SimpleJSDepsBuild.SimpleJSDeps

trait CommonModule extends ScalaModule with ScalafmtModule {

  def platformSegment: String

  override def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )

  override def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-Ypartial-unification",
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    // ivy"io.tryp:::splain:$splainVersion",
    ivy"org.scalamacros:::paradise:$macroParadiseVersion",
    ivy"org.typelevel:::kind-projector:$kindProjectorVersion",
    ivy"com.olegpy::better-monadic-for:$betterMonadicForVersion"
  )

  // add back in when necessary
  // def repositories = super.repositories ++ Seq(
  //   MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  // )

  override def ivyDeps = Agg(
    ivy"org.julianmichael::jjm-core::$jjmVersion",
    ivy"org.julianmichael::jjm-io::$jjmVersion",
    ivy"org.typelevel::mouse::$mouseVersion",
    ivy"org.typelevel::kittens::$kittensVersion",
    ivy"org.typelevel::cats-collections-core::$catsCollectionsVersion",
    // ivy"org.typelevel::alleycats-core::$catsVersion",
    ivy"org.julianmichael::qasrl::$qasrlVersion",
    ivy"org.julianmichael::qasrl-bank::$qasrlVersion",
    ivy"org.julianmichael::qasrl-bank-service::$qasrlVersion",
    ivy"com.github.japgolly.scalacss::core::$scalacssVersion"
  )
}

trait CommonMainModule extends CommonModule {
  def scalaVersion = thisScalaVersion
  def millSourcePath = super.millSourcePath / RelPath.up

  trait CommonTestModule extends CommonModule with TestModule {
    override def ivyDeps = Agg(
      ivy"org.scalameta::munit::$munitVersion",
      ivy"org.typelevel::munit-cats-effect-2::$munitCatsEffectVersion",
    )
    def testFrameworks = Seq("munit.Framework")
  }
}

trait JvmModule extends CommonMainModule {
  def platformSegment = "jvm"

  // for using runMain in commands
  def runMainFn = T.task { (mainClass: String, args: Seq[String]) =>
    import mill.modules.Jvm
    import mill.eval.Result
    try Result.Success(
      Jvm.runSubprocess(
        mainClass,
        runClasspath().map(_.path),
        forkArgs(),
        forkEnv() ++ Seq("JAVA_OPTS" -> "-Xmx12g"),
        args,
        workingDir = ammonite.ops.pwd
      )
    ) catch {
      case e: InteractiveShelloutException =>
        Result.Failure("subprocess failed")
    }
  }

  trait Tests extends super.Tests with CommonTestModule {
    def platformSegment = "jvm"
  }
}

trait FullJvmModule extends JvmModule {
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.monovore::decline::$declineVersion",
    ivy"com.monovore::decline-effect::$declineVersion",
    ivy"ch.qos.logback:logback-classic:$logbackVersion"
  )
}

trait JsModule extends CommonMainModule with ScalaJSModule {
  def scalaJSVersion = T(thisScalaJSVersion)
  def platformSegment = "js"
  trait Tests extends super.Tests with CommonTestModule {
    def scalaJSVersion = T(thisScalaJSVersion)
    def platformSegment = "js"
    def moduleKind = T(mill.scalajslib.api.ModuleKind.CommonJSModule)
  }
}

trait FullJsModule extends JsModule with SimpleJSDeps {
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.julianmichael::jjm-ui::$jjmVersion",
    ivy"org.scala-js::scalajs-dom::$scalajsDomVersion",
    ivy"be.doeraene::scalajs-jquery::$scalajsJqueryVersion",
    ivy"com.github.japgolly.scalacss::ext-react::$scalacssVersion"
  )
  override def jsDeps = Agg(
    "https://code.jquery.com/jquery-2.1.4.min.js",
    "https://cdnjs.cloudflare.com/ajax/libs/react/15.6.1/react.js",
    "https://cdnjs.cloudflare.com/ajax/libs/react/15.6.1/react-dom.js"
  )
}

object `qasrl-roles` extends Module {

  override def millSourcePath = build.millSourcePath / "qasrl-roles"

  object clustering extends Module {
    object js extends JsModule {
      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.julianmichael::freelog::$freelogVersion"
      )
    }
    object jvm extends FullJvmModule {
      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.julianmichael::freelog::$freelogVersion",
        ivy"org.scalanlp::breeze:$breezeVersion",
        ivy"org.scalanlp::breeze-natives:$breezeVersion"
      )
    }
  }

  object modeling extends Module {
    object js extends JsModule {
      def moduleDeps = Seq(clustering.js)

      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.julianmichael::jjm-datasets::$jjmVersion",
        ivy"org.julianmichael::freelog::$freelogVersion",
        ivy"com.cibo::evilplot::$evilplotVersion"
      )
    }
    object jvm extends FullJvmModule {
      def moduleDeps = Seq(clustering.jvm)

      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.julianmichael::jjm-datasets::$jjmVersion",
        ivy"org.julianmichael::freelog::$freelogVersion",
        ivy"com.cibo::evilplot::$evilplotVersion",
        ivy"org.scalanlp::breeze:$breezeVersion",
        ivy"org.scalanlp::breeze-natives:$breezeVersion",
        ivy"com.lihaoyi::scalatags:$scalatagsVersion",
        ivy"com.github.tototoshi::scala-csv:$scalaCsvVersion"
      )
    }
  }

  object browse extends Module {
    object jvm extends FullJvmModule { def moduleDeps = Seq(modeling.jvm) }
    object js extends FullJsModule { def moduleDeps = Seq(modeling.js) }

    def serve(args: String*) = T.command {
      val jsPath = js.fastOpt().path.toString
      val jsDepsPath = js.aggregatedJSDeps().path.toString
      val runMain = jvm.runMainFn()
      runMain(
        "qasrl.roles.browse.Serve",
        List(
          "--jsDeps", jsDepsPath,
          "--js", jsPath
        ) ++ args)
    }
  }
}
