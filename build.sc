// build.sc

import mill._
import mill.scalalib._
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import mill.contrib.buildinfo.BuildInfo

object yuck extends ScalaModule with BuildInfo {

    def git(args: String*) = os.proc("git" :: args.toList).call().out.lines().head
    def gitCommitHash = T.input {git("rev-parse", "HEAD")}
    def gitCommitDate = T {git("log", "-1", "--pretty=format:%cd", "--date=format:%Y%m%d")}
    def gitBranch = T {git("rev-parse", "--abbrev-ref", "HEAD")}
    def shortVersion = T {gitCommitDate()}
    def longVersion = T {
        "%s-%s-%s".format(gitCommitDate(), gitBranch().replaceAll("/", "-"), gitCommitHash().take(8))}
    def version = T {if (gitBranch() == "master") shortVersion() else longVersion()}

    override def buildInfoPackageName = "yuck"
    override def buildInfoMembers = Seq(
        BuildInfo.Value("gitBranch", gitBranch()),
        BuildInfo.Value("gitCommitDate", gitCommitDate()),
        BuildInfo.Value("gitCommitHash", gitCommitHash()),
        BuildInfo.Value("version", version()))
    override def buildInfoStaticCompiled = true

    override def scalaVersion = "3.4.2"
    override def millSourcePath = os.pwd
    override def sources = T.sources {millSourcePath / "src" / "main"}
    override def resources = T.sources()
    override def javacOptions = Seq("-source", "11", "-target", "11")
    override def scalacOptions = Seq("-release", "11", "-deprecation", "-unchecked", "-feature")

    override def ivyDeps = Agg(
        ivy"com.conversantmedia:rtree:1.0.5",
        ivy"com.github.scopt::scopt:4.1.0",
        ivy"org.jgrapht:jgrapht-core:1.5.2",
        ivy"org.scala-lang.modules::scala-parser-combinators:2.4.0"
    )

    val basicJvmConfiguration = Seq("-Djava.lang.Integer.IntegerCache.high=10000", "-XX:+UseParallelGC")
    val jvmHeapSize = Option(System.getenv("YUCK_HEAP_SIZE")).getOrElse("2G")
    val jvmConfiguration = Seq("-Xmx%s".format(jvmHeapSize)) ++ basicJvmConfiguration
    override def forkArgs = jvmConfiguration

    override def mainClass = Some("yuck.flatzinc.runner.FlatZincRunner")

    def fullClasspath = T {(localClasspath() ++ upstreamAssemblyClasspath()).map(_.path)}
    def fullClasspathAsString = T {fullClasspath().mkString(":")}

    object test extends ScalaTests {

        override def millSourcePath = os.pwd
        override def sources = T.sources {millSourcePath / "src" / "test"}
        override def resources = T.sources()

        override def ivyDeps = Agg(
            ivy"junit:junit:4.13.2",
            ivy"io.spray::spray-json:1.3.6".withDottyCompat(test.this.scalaVersion()),
            ivy"org.jgrapht:jgrapht-io:1.5.2",
            ivy"org.mockito:mockito-core:3.12.4"
        )

        override def testFramework = "com.novocode.junit.JUnitFramework"

        val jvmHeapSize = Option(System.getenv("YUCK_TEST_HEAP_SIZE")).getOrElse("2G")
        val jvmConfiguration = Seq("-Xmx%s".format(jvmHeapSize), "-XX:+AggressiveHeap") ++ basicJvmConfiguration
        override def forkArgs = jvmConfiguration

        override def mainClass = Some("yuck.test.util.YuckTestRunner")

        def fullClasspath = T {(localClasspath() ++ upstreamAssemblyClasspath()).map(_.path)}
        def fullClasspathAsString = T {fullClasspath().mkString(":")}
    }

    def documentation = T.source {millSourcePath / "doc"}
    def miniZincBindings = T.source {millSourcePath / "resources" / "mzn" / "lib" / "yuck"}
    def miniZincSolverConfigurationTemplate = T.source {millSourcePath / "resources" / "mzn" / "yuck.msc.in"}
    def unixStartScriptTemplate = T.source {millSourcePath / "resources" / "bin" / "yuck.in"}
    def winStartScriptTemplate = T.source {millSourcePath / "resources" / "bin" / "yuck.bat.in"}
    def debianControlFileTemplate = T.source {millSourcePath / "resources" / "debian" / "control.in"}

    def corePackage = T {
        val packageName = "yuck-".concat(version())
        val packageDir = T.dest / packageName
        os.remove.all(packageDir)
        for (dependency <- upstreamAssemblyClasspath().map(_.path)) {
            os.copy.into(dependency, packageDir / "lib", createFolders = true, copyAttributes = true)
        }
        val yuckJarName = packageName.concat(".jar")
        os.copy(jar().path, packageDir / "lib" / yuckJarName, createFolders = true, copyAttributes = true)
        os.copy(documentation().path, packageDir / "doc", createFolders = true, copyAttributes = true)
        os.copy(miniZincBindings().path, packageDir / "mzn" / "lib", createFolders = true, copyAttributes = true)
        val classPathComponents = (upstreamAssemblyClasspath().iterator.map(_.path.last).toSeq :+ yuckJarName)
        val unixClassPath = classPathComponents.map(jarName => "$LIB_DIR/".concat(jarName)).mkString(":")
        val basicMapping = Map("#YUCK_JAVA_OPTS#" -> basicJvmConfiguration.mkString(" "), "#MAIN_CLASS#" -> mainClass().get)
        fillTemplateIn(
            unixStartScriptTemplate().path,
            packageDir / "bin" / "yuck",
            basicMapping + ("#CLASS_PATH#" -> unixClassPath))
        makeExecutable(packageDir / "bin" / "yuck")
        val winClassPath = classPathComponents.map(jarName => "%LIB_DIR%\\".concat(jarName)).mkString(";")
        fillTemplateIn(
            winStartScriptTemplate().path,
            packageDir / "bin" / "yuck.bat",
            basicMapping + ("#CLASS_PATH#" -> winClassPath))
        PathRef(packageDir)
    }

    def universalPackage = T {
        val packageName = "yuck-".concat(version())
        val packageDir = T.dest / packageName
        os.remove.all(packageDir)
        os.copy(corePackage().path, packageDir, createFolders = true, copyAttributes = true)
        fillTemplateIn(
            miniZincSolverConfigurationTemplate().path,
            packageDir / "mzn" / "yuck.msc",
            Map("#VERSION#" -> version(), "#EXE_PATH#" -> "../bin/yuck", "#MZN_LIB_PATH#" -> "lib"))
        val archiveName = packageName.concat(".zip")
        os.proc("zip", "-r", archiveName, packageName).call(cwd = T.dest)
        PathRef(T.dest / archiveName)
    }

    def debianPackage = T {
        val packageName = "yuck-".concat(version())
        val packageDir = T.dest / packageName
        os.remove.all(packageDir)
        os.copy(corePackage().path, packageDir / "usr" / "share" / "yuck", createFolders = true, copyAttributes = true)
        fillTemplateIn(
            miniZincSolverConfigurationTemplate().path,
            packageDir / "usr" / "share" / "minizinc" / "solvers" / "yuck.msc",
            Map("#VERSION#" -> version(), "#EXE_PATH#" -> "/usr/bin/yuck", "#MZN_LIB_PATH#" -> "/usr/share/yuck/mzn/lib"))
        fillTemplateIn(
            debianControlFileTemplate().path,
            packageDir / "DEBIAN" / "control",
            Map("#VERSION#" -> version()))
        os.makeDir.all(packageDir / "usr" / "bin")
        os.symlink(packageDir / "usr" / "bin" / "yuck", os.Path("/usr/share/yuck/bin/yuck"))
        os.proc("dpkg-deb", "--build", packageName).call(cwd = T.dest)
        PathRef(T.dest / (packageName.concat(".deb")))
    }

    private def fillTemplateIn(template: os.Path, target: os.Path, mapping: Map[String, String]): Unit = {
        os.write.append(
            target,
            mapping.foldLeft(os.read(template))({case (s, (k, v)) => s.replace(k, v)}),
            createFolders = true)
    }

    private def makeExecutable(path: os.Path): Unit = {
        import java.nio.file.Files
        import java.nio.file.attribute.PosixFilePermission
        val perms = Files.getPosixFilePermissions(path.toNIO)
        perms.add(PosixFilePermission.GROUP_EXECUTE)
        perms.add(PosixFilePermission.OWNER_EXECUTE)
        perms.add(PosixFilePermission.OTHERS_EXECUTE)
        Files.setPosixFilePermissions(path.toNIO, perms)
    }

}
