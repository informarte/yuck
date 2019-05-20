import scala.sys.process._

val gitHeadCommitSha = settingKey[String]("The current git commit SHA")
gitHeadCommitSha := Process("git rev-parse HEAD").lineStream.head
val currentGitBranch = settingKey[String]("The current git branch")
currentGitBranch := Process("git rev-parse --abbrev-ref HEAD").lineStream.head
val today = settingKey[String]("The current date")
today := new java.text.SimpleDateFormat("yyyyMMdd").format(new java.util.Date)

def createMscFile(baseDir: java.io.File, version: String, exePath: String, mznLibPath: String): java.io.File = {
    val source = new java.io.File(baseDir / "resources" / "mzn", "yuck.msc.in")
    val sink = java.io.File.createTempFile("yuck-", ".msc")
    val writer = new java.io.FileWriter(sink)
    for (line <- scala.io.Source.fromFile(source).getLines) {
        writer.write(
            "%s\n".format(
                line.replace("VERSION", version)
                    .replace("EXE_PATH", exePath)
                    .replace("MZN_LIB_PATH", mznLibPath)))
    }
    writer.close
    sink
}

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

scalaVersion := "2.12.8"

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
libraryDependencies += "org.mockito" % "mockito-core" % "2.27.0" % "test"
libraryDependencies += "com.conversantmedia" % "rtree" % "1.0.5"
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

val yuckMscFileForUniversalPackage = taskKey[java.io.File]("Create yuck.msc file for universal package")
yuckMscFileForUniversalPackage :=
    createMscFile(baseDir = baseDirectory.value, version = today.value, exePath = "../bin/yuck", mznLibPath = "lib")

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
    .map(file => file -> ("mzn/lib/" + file.getName))
mappings in Universal += {
    val file = yuckMscFileForUniversalPackage.value
    file -> "mzn/yuck.msc"
}

enablePlugins(DebianPlugin)

val yuckMscFileForDebianPackage = taskKey[java.io.File]("Create yuck.msc file for Debian package")
yuckMscFileForDebianPackage :=
    createMscFile(baseDir = baseDirectory.value, version = today.value, exePath = "/usr/bin/yuck", mznLibPath = "/usr/share/yuck/mzn/lib")

debianPackageDependencies in Debian ++= Seq("default-jre-headless (>= 1.8)")
debianSection in Debian := "universe/interpreters"
linuxPackageMappings in Debian += {
    val file = yuckMscFileForDebianPackage.value
    packageMapping(file -> "/usr/share/minizinc/solvers/yuck.msc").withPerms("0644")
}

enablePlugins(BuildInfoPlugin)
buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoPackage := "yuck"
