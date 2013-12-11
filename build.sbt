scalaVersion := "2.10.3"

name := "Liminoid"

javaOptions += "-Djava.library.path=lib"

seq(lwjglSettings: _*)

libraryDependencies <++= scalaVersion(v => Seq(
  "org.scala-lang" % "scala-actors" % v,
  //"org.rxtx" % "rxtxcomm" % "2.0-7pre1"
  "org.rxtx" % "rxtx" % "2.1.7"))

//resolvers += "linter" at "http://hairyfotr.github.io/linteRepo/releases"

//addCompilerPlugin("com.foursquare.lint" %% "linter" % "0.1-SNAPSHOT")

//scalacOptions += "-Xplugin:/home/hairy/dev/linter/target/scala-2.10/linter_2.10-0.1-SNAPSHOT.jar"

javaOptions += "-Xmx1G"

fork := true

scalacOptions ++= Seq(
  //"-target:jvm-1.7",
  "-J-Xmx1G",
  "-optimise", "-Yinline", "-Yinline-warnings", "-Yclosure-elim",
  "-feature", "-deprecation", "-unchecked",
  "-Xlint",
  "-Ywarn-adapted-args",
  //"-Ywarn-all",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  //"-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  //"-Ywarn-numeric-widen",
  "-Ywarn-value-discard")

org.scalastyle.sbt.ScalastylePlugin.Settings

seq(findbugsSettings : _*)
