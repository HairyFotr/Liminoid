import de.johoop.findbugs4sbt._

scalaVersion := "2.11.0"

name := "Liminoid"

javaOptions += "-Djava.library.path=lib"

seq(lwjglSettings: _*)

libraryDependencies <++= scalaVersion(v => Seq(
  "org.scala-lang" % "scala-actors" % v//,
  //"org.rxtx" % "rxtxcomm" % "2.0-7pre1"
  //"org.rxtx" % "rxtx" % "2.1.7")
))

javaOptions += "-Xmx4G"

fork := true

scalacOptions ++= Seq(
  //"-target:jvm-1.7",
  "-J-Xmx4G",
  "-optimise", "-Yinline", "-Yclosure-elim",
  "-feature", /*"-deprecation",*/ "-unchecked",
  //"-Xlint",
  "-Ywarn-adapted-args",
  //"-Ywarn-all",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  //"-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-infer-any",//2.11
  //"-Xstrict-inference",//2.11
  //"-Ywarn-numeric-widen",
  "-Ywarn-value-discard")

//resolvers += "linter" at "http://hairyfotr.github.io/linteRepo/releases"

//addCompilerPlugin("com.foursquare.lint" %% "linter" % "0.1-SNAPSHOT")

scalacOptions += "-Xplugin:/home/hairy/dev/linter/target/scala-2.11/linter_2.11-0.1-SNAPSHOT.jar"

//addCompilerPlugin("org.brianmckenna" % "wartremover_2.11.0-RC3" % "0.8")

//scalacOptions += "-P:wartremover:only-warn-traverser:org.brianmckenna.wartremover.warts.Unsafe"

//scalacOptions += "-P:wartremover:traverser:org.brianmckenna.wartremover.warts.Unsafe"

org.scalastyle.sbt.ScalastylePlugin.Settings

// Findbugs (optionally put findbugs plugins (such as fb-contrib and findsecbugs) jars into ~/.findbugs/plugin)

findbugsSettings

//findbugsEffort := Effort.Maximum

findbugsReportPath <<= baseDirectory { base => Some(base / "findbugs.xml") }


