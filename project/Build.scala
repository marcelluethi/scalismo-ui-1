import sbt._
import sbt.Keys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._
import sbtbuildinfo.Plugin._

object BuildSettings {
  val buildOrganization = "ch.unibas.cs.gravis"
  val buildVersion = "0.6.1-withHighDPI"
  val buildScalaVersion = "2.10.5"
  val publishURL = Resolver.file("file", new File("/export/contrib/statismo/repo/private"))

  val buildSettings = Defaults.defaultSettings ++ Seq(
    scalacOptions ++= Seq("-encoding", "UTF-8", "-Xlint", "-deprecation", "-unchecked", "-feature"),
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    crossScalaVersions := Seq("2.10.5", "2.11.7", "2.12.4"),
    shellPrompt := ShellPrompt.buildShellPrompt) ++ buildInfoSettings ++ Seq(
    sourceGenerators in Compile <+= buildInfo,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion),
    buildInfoPackage := "scalismo.ui")

}

// Shell prompt which show the current project,
// git branch and build version
object ShellPrompt {
  object devnull extends ProcessLogger {
    def info(s: => String): Unit = {}
    def error(s: => String): Unit = {}
    def buffer[T](f: => T): T = f
  }
  def currBranch = (
    ("git status -sb" lines_! devnull headOption)
    getOrElse "-" stripPrefix "## ")

  val buildShellPrompt = {
    (state: State) =>
      {
        val currProject = Project.extract(state).currentProject.id
        s"$currProject:$currBranch:${BuildSettings.buildVersion}> "
      }
  }
}

object Resolvers {
  private val sonatypeSnapshots = "Sonatype SNAPSHOTs" at "https://oss.sonatype.org/content/repositories/snapshots/"
  private val sonatypeRelease = "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
  private val imagej = "imagej.releases" at "http://maven.imagej.net/content/repositories/releases"
  private val twitter = "twitter" at "http://maven.twttr.com/"
  private val scalismoPublic = "Scalismo (public)" at "http://shapemodelling.cs.unibas.ch/repository/public"
  private val scalismoPrivate = "Scalismo (private)" at "https://statismo.cs.unibas.ch/repository/private"
  val stkResolvers = Seq(scalismoPublic, scalismoPrivate, sonatypeSnapshots, sonatypeRelease, imagej, twitter)
}

object Creds {
  val scalismoPrivate = Credentials(Path.userHome / ".ivy2" / ".credentials-statismo-private")
}

object Dependencies {
  val scalismo = "ch.unibas.cs.gravis" %% "scalismo" % "0.10.+"
  val scalismoNative = "ch.unibas.cs.gravis" % "scalismo-native-all" % "3.0.+"
  val scalatest = "org.scalatest" %% "scalatest" % "3.0+" % "test"
  // the following two dependencies are transitively obtained through other dependencies
  //val scalaReflect = "org.scala-lang" % "scala-reflect" % buildScalaVersion
  //val scalaSwing = "org.scala-lang" % "scala-swing" % buildScalaVersion
  val scalaAsync = "org.scala-lang.modules" %% "scala-async" % "0.9.+"
  val scalaInterpreterPane = "de.sciss" %% "scalainterpreterpane" % "1.8.1"
}

object STKBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  // Sub-project specific dependencies
  val commonDeps = Seq(
    scalismo,
    scalismoNative,
    scalatest,
    // the following two dependencies are transitively obtained through other dependencies.
    //scalaReflect,
    //scalaSwing,
    scalaAsync,
    scalaInterpreterPane)

  lazy val scalismo_ui = Project(
    "scalismo-ui",
    file("."),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= commonDeps,
      resolvers ++= stkResolvers,
      credentials += Creds.scalismoPrivate,
      publishTo := Some(publishURL),
      EclipseKeys.withSource := true))

}
