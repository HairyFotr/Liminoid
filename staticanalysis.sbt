import de.johoop.findbugs4sbt._
import de.johoop.cpd4sbt.CopyPasteDetector._
import de.johoop.cpd4sbt.{ ReportType => CPDReportType }

scalacOptions ++= Seq(
  "-feature",
  //"-deprecation",
  "-unchecked",
  "-Xlint",
  //"-Xstrict-inference",
  "-Ywarn-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  //"-Ywarn-numeric-widen",
  "-Ywarn-value-discard")

// Linter
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1-SNAPSHOT")

//scalacOptions += "-Xplugin:/home/hairy/dev/linter/target/scala-2.11/linter_2.11-0.1-SNAPSHOT.jar"

scalacOptions += "-P:linter:disable:UseHypot"

// Scapegoat
// add this to project/plugins.sbt
//addSbtPlugin("com.sksamuel.scapegoat" %% "sbt-scapegoat" % "<version>")
//scapegoatDisabledInspections := Seq("VarUse", "NullParameter", "NullAssignment", "WildcardImport")

// Scalastyle
scalastyleConfig <<= baseDirectory { base => base / "sca" / "scalastyle-config.xml" }

watchSources += baseDirectory.value / "sca" / "scalastyle-config.xml"

// Findbugs (optionally put findbugs plugins (such as fb-contrib and findsecbugs) jars into ~/.findbugs/plugin)
findbugsSettings

findbugsReportPath <<= baseDirectory { base => Some(base / "sca" / "findbugsoutput.xml") }

// CPD
cpdSettings

cpdTargetPath <<= baseDirectory { base => base / "sca" }

cpdReportName := "cpdoutput.txt"

cpdReportType := CPDReportType.Simple
