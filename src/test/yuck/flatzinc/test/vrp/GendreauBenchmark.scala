package yuck.flatzinc.test.vrp

import java.io.File

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.flatzinc.test.util.*
import yuck.test.util.ParallelParameterizedTestRunner

/**
 * Runs the Gendreau TSPTW benchmark
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[ParallelParameterizedTestRunner])
final class GendreauBenchmark(task: ZincTestTask) extends ZincBasedTest {

    @Test
    def solve(): Unit = {
        super.solve(task)
    }

}

/**
 * Generates test tasks from the Gendreau TSPTW benchmark instances
 *
 * @author Michael Marte
 */
object GendreauBenchmark extends VrpTestTaskFactory {

    override protected def modelFilter(file: File) = List("tsptw_yuck.mzn").contains(file.getName)

    override protected def instanceFilter(file: File) = super.instanceFilter(file) && file.getPath.contains("Gendreau")

    // retrieved on Oct 31th 2020 from http://lopez-ibanez.eu/tsptw-instances,
    // optimality results were obtained with Gecode
    override protected val Results = Map(
        ("n20w120.001", (267, true)), ("n20w120.002", (218, true)), ("n20w120.003", (303, true)),
        ("n20w120.004", (300, true)), ("n20w120.005", (240, true)), ("n20w140.001", (176, true)),
        ("n20w140.002", (272, true)), ("n20w140.003", (236, true)), ("n20w140.004", (255, true)),
        ("n20w140.005", (225, true)), ("n20w160.001", (241, true)), ("n20w160.002", (201, true)),
        ("n20w160.003", (201, true)), ("n20w160.004", (203, true)), ("n20w160.005", (245, true)),
        ("n20w180.001", (253, true)), ("n20w180.002", (265, true)), ("n20w180.003", (271, true)),
        ("n20w180.004", (201, true)), ("n20w180.005", (193, true)), ("n20w200.001", (233, true)),
        ("n20w200.002", (203, true)), ("n20w200.003", (249, true)), ("n20w200.004", (293, true)),
        ("n20w200.005", (227, true)), ("n40w120.001", (434, false)), ("n40w120.002", (445, false)),
        ("n40w120.003", (357, false)), ("n40w120.004", (303, false)), ("n40w120.005", (350, false)),
        ("n40w140.001", (328, false)), ("n40w140.002", (383, false)), ("n40w140.003", (398, false)),
        ("n40w140.004", (342, false)), ("n40w140.005", (371, false)), ("n40w160.001", (348, false)),
        ("n40w160.002", (337, false)), ("n40w160.003", (346, false)), ("n40w160.004", (288, false)),
        ("n40w160.005", (315, false)), ("n40w180.001", (337, false)), ("n40w180.002", (347, false)),
        ("n40w180.003", (279, false)), ("n40w180.004", (354, false)), ("n40w180.005", (335, false)),
        ("n40w200.001", (330, false)), ("n40w200.002", (303, false)), ("n40w200.003", (339, false)),
        ("n40w200.004", (301, false)), ("n40w200.005", (296, false)), ("n60w120.001", (384, false)),
        ("n60w120.002", (427, false)), ("n60w120.003", (407, false)), ("n60w120.004", (490, false)),
        ("n60w120.005", (547, false)), ("n60w140.001", (423, false)), ("n60w140.002", (462, false)),
        ("n60w140.003", (427, false)), ("n60w140.004", (488, false)), ("n60w140.005", (460, false)),
        ("n60w160.001", (560, false)), ("n60w160.002", (423, false)), ("n60w160.003", (434, false)),
        ("n60w160.004", (401, false)), ("n60w160.005", (502, false)), ("n60w180.001", (411, false)),
        ("n60w180.002", (399, false)), ("n60w180.003", (445, false)), ("n60w180.004", (456, false)),
        ("n60w180.005", (395, false)), ("n60w200.001", (410, false)), ("n60w200.002", (414, false)),
        ("n60w200.003", (455, false)), ("n60w200.004", (431, false)), ("n60w200.005", (427, false)),
        ("n80w100.001", (565, false)), ("n80w100.002", (567, false)), ("n80w100.003", (580, false)),
        ("n80w100.004", (649, false)), ("n80w100.005", (532, false)), ("n80w120.001", (498, false)),
        ("n80w120.002", (577, false)), ("n80w120.003", (540, false)), ("n80w120.004", (501, false)),
        ("n80w120.005", (597, false)), ("n80w140.001", (512, false)), ("n80w140.002", (470, false)),
        ("n80w140.003", (580, false)), ("n80w140.004", (423, false)), ("n80w140.005", (545, false)),
        ("n80w160.001", (506, false)), ("n80w160.002", (549, false)), ("n80w160.003", (521, false)),
        ("n80w160.004", (509, false)), ("n80w160.005", (439, false)), ("n80w180.001", (551, false)),
        ("n80w180.002", (479, false)), ("n80w180.003", (524, false)), ("n80w180.004", (479, false)),
        ("n80w180.005", (470, false)), ("n80w200.001", (490, false)), ("n80w200.002", (488, false)),
        ("n80w200.003", (464, false)), ("n80w200.004", (526, false)), ("n80w200.005", (439, false)),
        ("n100w80.001", (670, false)), ("n100w80.002", (668, false)), ("n100w80.003", (691, false)),
        ("n100w80.004", (700, false)), ("n100w80.005", (603, false)), ("n100w100.001", (643, false)),
        ("n100w100.002", (619, false)), ("n100w100.003", (685, false)), ("n100w100.004", (684, false)),
        ("n100w100.005", (572, false)), ("n100w120.001", (629, false)), ("n100w120.002", (540, false)),
        ("n100w120.003", (617, false)), ("n100w120.004", (663, false)), ("n100w120.005", (537, false)),
        ("n100w140.001", (604, false)), ("n100w140.002", (615, false)), ("n100w140.003", (481, false)),
        ("n100w140.004", (533, false)), ("n100w140.005", (509, false)), ("n100w160.001", (582, false)),
        ("n100w160.002", (532, false)), ("n100w160.003", (495, false)), ("n100w160.004", (580, false)),
        ("n100w160.005", (587, false)), ("n100w180.001", (602, false)), ("n100w180.002", (536, false)),
        ("n100w180.003", (599, false)), ("n100w180.004", (557, false)), ("n100w180.005", (514, false)),
        ("n100w200.001", (570, false)), ("n100w200.002", (535, false)), ("n100w200.003", (594, false)),
        ("n100w200.004", (552, false)), ("n100w200.005", (500, false)))
        .map{case (instanceName, (value, isOptimal)) => ("Gendreau/" + instanceName, ObjectiveValue(value, isOptimal))}

    // keep models aligned
    private def verifyAgainstCpModel(task: ZincTestTask) = task.copy(verificationModelName = "tsptw_cp")

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(amendKnownBestResult).map(verifyAgainstCpModel).map(Array(_)).asJava

}
