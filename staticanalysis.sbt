scalacOptions ++= Seq(
   "-feature"
  ,"-deprecation"
  ,"-unchecked"
  ,"-Xlint"
//,"-Xcheckinit" //Adds runtime checks
  ,"-Xstrict-inference"
  ,"-Ywarn-adapted-args"
  ,"-Ywarn-dead-code"
  ,"-Ywarn-inaccessible"
  ,"-Ywarn-infer-any"
  ,"-Ywarn-nullary-override"
  ,"-Ywarn-nullary-unit"
//,"-Ywarn-numeric-widen"
  ,"-Ywarn-value-discard"
)

// Clippy
//clippyColorsEnabled := true

// Linter
//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1-SNAPSHOT")
scalacOptions += "-Xplugin:/home/hairy/dev/linter/target/scala-2.12/linter_2.12-0.1-SNAPSHOT.jar"
scalacOptions += "-P:linter:disable:UseHypot"

// Abide
//libraryDependencies ++= Seq("abide-core"/*, "abide-extra", "abide-akka"*/).map("com.typesafe" %% _ % "0.1-SNAPSHOT" % "abide")

// Wartremover
//wartremoverWarnings ++= Warts.unsafe
//wartremoverWarnings ++= wartremover.Warts.allBut(Wart.Any, Wart.Nothing, Wart.Equals, Wart.Null, Wart.While, Wart.Return, Wart.Throw, Wart.Overloading, Wart.Var, Wart.ImplicitParameter, Wart.ToString, Wart.StringPlusAny, Wart.NonUnitStatements, Wart.DefaultArguments, Wart.MutableDataStructures, Wart.AsInstanceOf, Wart.IsInstanceOf, Wart.LeakingSealed, Wart.OptionPartial, Wart.FinalCaseClass/*Yes, but not in Warning*/, Wart.EitherProjectionPartial/*TODO*/, Wart.Option2Iterable/*TODO*/, Wart.TraversableOps/*TODO*/, Wart.Recursion/*?*/, Wart.PublicInference/*Crashes*//*, Wart.NoNeedForMonad Crashes*/)
//wartremoverWarnings ++= Seq(/*TODO ContribWart.SomeApply,*/ ContribWart.SealedCaseClass)

// Scapegoat
//scapegoatVersion := "1.3.0"
//scapegoatDisabledInspections := Seq("VarUse", "NullParameter", "NullAssignment", "WildcardImport")

// Scalastyle
scalastyleConfig := baseDirectory.value / "project" / "scalastyle-config.xml"
watchSources += baseDirectory.value / "project" / "scalastyle-config.xml"

// Findbugs (optionally put findbugs plugins (such as fb-contrib and findsecbugs) jars into ~/.findbugs/plugin)
//findbugsEffort := FindbugsEffort.Maximum
//findbugsReportPath := Some(baseDirectory.value / "target" / "findbugsreport.xml")

// CPD
//cpdTargetPath := baseDirectory.value / "target"
//cpdReportName := "cpdreport.txt"
//cpdReportType := CpdReportType.Simple
//cpdOutputType := CpdOutputType.Console
//cpdMinimumTokens := 50
