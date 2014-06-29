name := "Liminoid"

scalaVersion := "2.11.1"

javaOptions += "-Djava.library.path=lib"

seq(lwjglSettings: _*)

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

libraryDependencies <++= scalaVersion(v => Seq(
  "org.scala-lang" % "scala-actors" % v//,
  //"org.rxtx" % "rxtxcomm" % "2.0-7pre1"
  //"org.rxtx" % "rxtx" % "2.1.7")
))

fork := true

javaOptions += "-Xmx4G"

scalacOptions ++= Seq(
  "-J-Xmx4G",
  "-optimise",
  "-Yinline",
  "-Yclosure-elim")
