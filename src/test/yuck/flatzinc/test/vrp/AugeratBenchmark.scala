package yuck.flatzinc.test.vrp

import java.io.File

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.flatzinc.test.util.*
import yuck.test.util.ParallelParameterizedTestRunner

/**
 * Runs the Augerat CVRP benchmark
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[ParallelParameterizedTestRunner])
final class AugeratBenchmark(task: ZincTestTask) extends ZincBasedTest {

    @Test
    def solve(): Unit = {
        super.solve(task.copy(dataAssignments = Map(("MaxKToMinKRatio", "1"))))
    }

}

/**
 * Generates test tasks from the Augerat CVRP benchmark instances
 *
 * @author Michael Marte
 */
object AugeratBenchmark extends VrpTestTaskFactory {

    override protected def modelFilter(file: File) = List("cvrp_yuck.mzn").contains(file.getName)

    override protected def instanceFilter(file: File) = super.instanceFilter(file) && file.getPath.contains("Augerat")

    // retrieved on Nov 8th 2020 from https://neo.lcc.uma.es/vrp/known-best-results/
    private val AugeratAResults = Map(
        ("A-n32-k5", 784), ("A-n33-k5", 661), ("A-n33-k6", 742), ("A-n34-k5", 778), ("A-n36-k5", 799),
        ("A-n37-k5", 669), ("A-n37-k6", 949), ("A-n38-k5", 730), ("A-n39-k5", 822), ("A-n39-k6", 831),
        ("A-n44-k7", 937), ("A-n45-k6", 944), ("A-n45-k7", 1146), ("A-n46-k7", 914), ("A-n48-k7", 1073),
        ("A-n53-k7", 1010), ("A-n54-k7", 1167), ("A-n55-k9", 1073), ("A-n60-k9", 1354), ("A-n61-k9", 1034),
        ("A-n62-k8", 1288), ("A-n63-k9", 1616), ("A-n63-k10", 1314), ("A-n64-k9", 1401), ("A-n65-k9", 1174),
        ("A-n69-k9", 1159), ("A-n80-k10", 1763))
        .map{case (instanceName, value) => ("Augerat/A/" + instanceName, ObjectiveValue(value, true))}

    // retrieved on Nov 8th 2020 from https://neo.lcc.uma.es/vrp/known-best-results/
    private val AugeratBResults = Map(
        ("B-n31-k5", 672), ("B-n34-k5", 788), ("B-n35-k5", 955), ("B-n38-k6", 805), ("B-n39-k5", 549),
        ("B-n41-k6", 829), ("B-n43-k6", 742), ("B-n44-k7", 909), ("B-n45-k5", 751), ("B-n45-k6", 678),
        ("B-n50-k7", 741), ("B-n50-k8", 1312), ("B-n51-k7", 1018), ("B-n52-k7", 747), ("B-n56-k7", 707),
        ("B-n57-k7", 1144), ("B-n57-k9", 1598), ("B-n63-k10", 1496), ("B-n64-k9", 861), ("B-n66-k9", 1316),
        ("B-n67-k10", 1032), ("B-n68-k9", 1272),("B-n78-k10", 1221))
        .map{case (instanceName, value) => ("Augerat/B/" + instanceName, ObjectiveValue(value, true))}

    // retrieved on Nov 8th 2020 from https://neo.lcc.uma.es/vrp/known-best-results/
    private val AugeratPResults = Map(
        ("P-n16-k8", 450), ("P-n19-k2", 212), ("P-n20-k2", 216), ("P-n21-k2", 211), ("P-n22-k2", 216),
        ("P-n22-k8", 590), ("P-n23-k8", 529), ("P-n40-k5", 458), ("P-n45-k5", 510), ("P-n50-k7", 554),
        ("P-n50-k8", 629), ("P-n50-k10", 696), ("P-n51-k10", 741), ("P-n55-k7", 568), ("P-n55-k10", 694),
        ("P-n55-k15", 945), ("P-n60-k10", 744), ("P-n60-k15", 968), ("P-n65-k10", 792), ("P-n70-k10", 827),
        ("P-n76-k4", 593), ("P-n76-k5", 627), ("P-n101-k4", 681))
        .map{case (instanceName, value) => ("Augerat/P/" + instanceName, ObjectiveValue(value, true))}

    override protected val Results = AugeratAResults ++ AugeratBResults ++ AugeratPResults

    // keep models aligned
    private def verifyAgainstCpModel(task: ZincTestTask) = task.copy(verificationModelName = "cvrp_cp")

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(amendKnownBestResult).map(verifyAgainstCpModel).map(Array(_)).asJava

}
