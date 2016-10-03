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

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
    "-target:jvm-1.6", // https://groups.google.com/forum/#!topic/scala-user/-BPBXRkukaQ
    "-optimise",
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
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.10.1"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"

// See https://github.com/sbt/junit-interface/issues/66 for why it does not work!
testOptions in Test := Seq(Tests.Filter(s => s.endsWith("UnitTestSuite")))

fork := true
javaOptions += "-Xmx2g"

mainClass in (Compile, run) := Some("yuck.flatzinc.runner.FlatZincRunner")
mainClass in (Compile, packageBin) := Some("yuck.flatzinc.runner.FlatZincRunner")

enablePlugins(JavaAppPackaging)
enablePlugins(DebianPlugin)

debianNativeBuildOptions in Debian := Nil // dpkg-deb's default compression (currently xz)

debianPackageDependencies in Debian ++= Seq("openjdk-6-jre | java6-runtime", "bash (>= 2.05a-11)")
linuxPackageMappings in Debian +=
    packageMapping(
        baseDirectory.value / "Debian" / "changelog" -> "/usr/share/doc/yuck/changelog.gz",
        baseDirectory.value / "Debian" / "changelog.Debian" -> "/usr/share/doc/yuck/changelog.Debian.gz")
    .withPerms("0644")
    .gzipped
linuxPackageMappings in Debian +=
    packageMapping(baseDirectory.value / "Debian" / "copyright" -> "/usr/share/doc/yuck/copyright")
    .withPerms("0644")
linuxPackageMappings in Debian ++=
    (baseDirectory.value / "licenses")
    .listFiles
    .toStream
    .map(file => packageMapping(file -> ("/usr/share/doc/yuck/licenses/" + file.getName)).withPerms("0644"))
linuxPackageMappings in Debian ++=
    (baseDirectory.value / "resources" / "mzn" / "lib" / "yuck")
    .listFiles
    .toStream
    .map(file => packageMapping(file -> ("/usr/share/yuck/mzn/" + file.getName)).withPerms("0644"))
