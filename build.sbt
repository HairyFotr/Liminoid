scalaVersion := "2.11.4"

javaOptions += "-Djava.library.path=lib"

seq(lwjglSettings: _*)

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

classpathTypes += "maven-plugin" // for javacpp?

lwjgl.version := "2.9.1" // newer than in plugin

libraryDependencies <++= scalaVersion(v => Seq(
  "org.scala-lang" % "scala-actors" % v,
  //"org.bytedeco" % "javacv" % "0.9",
  //"org.bytedeco" % "javacpp" % "0.9", // TODO exclude a bunch of stuff
  //"org.bytedeco.javacpp-presets" % "opencv" % "2.4.9-0.9" classifier "" classifier "linux-x86_64",
  "org.l33tlabs.twl" % "pngdecoder" % "1.0",
  "org.scream3r" % "jssc" % "2.8.0",
  "javazoom" % "jlayer" % "1.0.1"
))

fork := true

javaOptions ++= Seq(
  "-Xmx3G",
  "-XX:MaxGCPauseMillis=15")

scalacOptions ++= Seq(
  "-optimize", "-target:jvm-1.7",
  "-Yinline", "-Yclosure-elim")

initialCommands := """
  import java.io.File
  import math._
  import org.ljudmila.liminoid._
  import Utils._
"""

watchSources += baseDirectory.value / "Settings.txt"
