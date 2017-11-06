scalaVersion := "2.12.3"

javaOptions += "-Djava.library.path=./lib"

Seq(lwjglSettings: _*)

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

classpathTypes += "maven-plugin" // for javacpp?

lwjgl.version := "2.9.3" // newer than in plugin

libraryDependencies ++= Seq(
  //"org.bytedeco" % "javacv" % "0.9",
  //"org.bytedeco" % "javacpp" % "0.9", // TODO exclude a bunch of stuff
  //"org.bytedeco.javacpp-presets" % "opencv" % "2.4.9-0.9" classifier "" classifier "linux-x86_64",
  "org.l33tlabs.twl" % "pngdecoder" % "1.0",
  "org.scream3r" % "jssc" % "2.8.0",
  "javazoom" % "jlayer" % "1.0.1"
)

fork := true

javaOptions ++= Seq(
  "-Xmx4G",
  "-XX:MaxGCPauseMillis=16"
)

scalacOptions += "-opt:l:inline"

initialCommands := """
  import java.io.File
  import math._
  import org.ljudmila._
  import org.ljudmila.liminoid._
  import Utils._
"""

watchSources += baseDirectory.value / "Settings.txt"
