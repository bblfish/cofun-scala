import sbt._
import sbt.Keys._

object ApplicationBuild extends Build  {

  val macroParadise = compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
  val resetAllAttrs = "org.scalamacros" %% "resetallattrs" % "1.0.0-M1"
  val si27212fix = compilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0")

  val scalaz = "org.scalaz" %% "scalaz-core" % "7.3.0-SNAPSHOT"


  lazy val root = (project in file(".")).
     settings(
       organization := "net.bblfish",
       name := "scalaz-play",
       scalaVersion := "2.11.8",
       resolvers += Resolver.sonatypeRepo("snapshots"), //for scalaz
       libraryDependencies ++= Seq(
         scalaz,
//         specs2Core % Test, specs2Scalacheck % Test, scalacheck % Test,
         macroParadise, kindProjector, resetAllAttrs, si27212fix
       ),
       initialCommands in (Test, console) := """ammonite.repl.Main.run("")""",
       scalacOptions ++= Seq(
         "-deprecation",
         "-encoding", "UTF-8",
         "-feature",
         "-language:_"
       )
     )
}