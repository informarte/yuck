val gitHeadCommitSha = settingKey[String]("The current git commit SHA")
gitHeadCommitSha := Process("git rev-parse HEAD").lines.head
val currentGitBranch = settingKey[String]("The current git branch")
currentGitBranch := Process("git rev-parse --abbrev-ref HEAD").lines.head
val today = settingKey[String]("The current date")
today := Process("date +%Y%m%d").lines.head

name := "yuck"
description := "Yuck is a constraint-based local-search solver with FlatZinc interface."
startYear := Some(2013)
// For creating a Debian package, the version must start with a digit.
version := today.value
maintainer := "Michael Marte <informarte@freenet.de>"
packageSummary := "FlatZinc interpreter"
packageDescription := description.value

EclipseKeys.withSource := true
EclipseKeys.withJavadoc := true
EclipseKeys.eclipseOutput := Some("target/eclipse")

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-opt:l:method",
    "-opt:l:inline",
    "-opt-inline-from:**",
    "-deprecation",
    "-unchecked",
    "-feature"
)

scalaSource in Compile := baseDirectory.value / "src/main"
javaSource in Compile := baseDirectory.value / "src/main"
scalaSource in Test := baseDirectory.value / "src/test"
javaSource in Test := baseDirectory.value / "src/test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.1.0"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
libraryDependencies += "io.spray" %% "spray-json" % "1.3.3"

// See https://github.com/sbt/junit-interface/issues/66 for why it does not work!
testOptions in Test := Seq(Tests.Filter(s => s.endsWith("UnitTestSuite")))

fork := true
javaOptions += "-Xmx2g"

mainClass in (Compile, run) := Some("yuck.flatzinc.runner.FlatZincRunner")
mainClass in (Compile, packageBin) := Some("yuck.flatzinc.runner.FlatZincRunner")

enablePlugins(JavaAppPackaging)
enablePlugins(UniversalPlugin)

mappings in Universal +=
    (baseDirectory.value / "doc" / "copyright" -> "doc/copyright")
mappings in Universal ++=
    (baseDirectory.value / "doc" / "licenses")
    .listFiles
    .toStream
    .map(file => file -> ("doc/licenses/" + file.getName))
mappings in Universal ++=
    (baseDirectory.value / "resources" / "mzn" / "lib" / "yuck")
    .listFiles
    .toStream
    .map(file => file -> ("mzn/" + file.getName))
