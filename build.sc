// build.sc
import mill._
import mill.define.Sources
import scalalib._
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import mill.contrib.buildinfo.BuildInfo

object yuck extends ScalaModule with BuildInfo {

    def git(args: String*) = os.proc("git" :: args.toList).call().out.lines.head
    def retrieveGitCommitHash() = T.command {git("rev-parse", "HEAD")}
    def gitCommitHash = T {retrieveGitCommitHash()}
    def retrieveGitCommitDate() = T.command {git("log", "-1", "--pretty=format:%cd", "--date=format:%Y%m%d")}
    def gitCommitDate = T {retrieveGitCommitDate()}
    def retrieveGitBranch() = T.command {git("rev-parse", "--abbrev-ref", "HEAD")}
    def gitBranch = T {retrieveGitBranch()}
    def shortVersion = T {gitCommitDate()}
    def longVersion = T {"%s-%s-%s".format(gitCommitDate(), gitBranch().replaceAll("/", "-"), gitCommitHash().take(8))}
    def version = T {if (gitBranch() == "master") shortVersion() else longVersion()}
    override def buildInfoPackageName = Some("yuck")
    override def buildInfoMembers: T[Map[String, String]] = T {
        Map(
            "gitBranch" -> gitBranch(),
            "gitCommitDate" -> gitCommitDate(),
            "gitCommitHash" -> gitCommitHash(),
            "version" -> version()
        )
    }

    override def scalaVersion = "2.13.2"
    override def millSourcePath = os.pwd
    override def sources = T.sources {millSourcePath / "src" / "main"}
    override def resources = T.sources()
    override def javacOptions = Seq("-source", "1.8", "-target", "1.8")
    override def scalacOptions = Seq("-target:jvm-1.8", "-deprecation", "-unchecked", "-feature")

    override def ivyDeps = Agg(
        ivy"com.conversantmedia:rtree:1.0.5",
        ivy"com.github.scopt::scopt:3.7.1",
        ivy"io.spray::spray-json:1.3.5",
        ivy"org.jgrapht:jgrapht-core:1.4.0",
        ivy"org.scala-lang.modules::scala-parser-combinators:1.1.2"
    )

    override def forkArgs = Seq("-server", "-Xmx2G", "-XX:+UseParallelGC", "-XX:+AggressiveHeap")
    override def mainClass = Some("yuck.flatzinc.runner.FlatZincRunner")

    def fullClasspath = T {(localClasspath() ++ upstreamAssemblyClasspath()).map(_.path)}
    def fullClasspathAsString = T {fullClasspath().mkString(":")}

    object test extends Tests {

        override def millSourcePath = os.pwd
        override def sources = T.sources {millSourcePath / "src" / "test"}
        override def resources = T.sources()

        override def testFrameworks = Seq("com.novocode.junit.JUnitFramework")

        override def ivyDeps = Agg(
            ivy"junit:junit:4.12",
            ivy"org.jgrapht:jgrapht-io:1.4.0",
            ivy"org.scalamock::scalamock:4.4.0"
        )
        override def runIvyDeps = Agg(
            ivy"com.novocode:junit-interface:0.11"
        )

        override def forkArgs = Seq("-server", "-Xmx2G", "-XX:+UseParallelGC", "-XX:+AggressiveHeap")
        override def mainClass = Some("yuck.util.testing.YuckTestRunner")

        def fullClasspath = T {(localClasspath() ++ upstreamAssemblyClasspath()).map(_.path)}
        def fullClasspathAsString = T {fullClasspath().mkString(":")}
    }

}
