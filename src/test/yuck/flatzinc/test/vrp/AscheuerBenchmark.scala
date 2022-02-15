package yuck.flatzinc.test.vrp

import java.io.File

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.flatzinc.test.util.*
import yuck.test.util.ParallelParameterizedTestRunner

/**
 * Runs the Ascheuer TSPTW benchmark
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[ParallelParameterizedTestRunner])
final class AscheuerBenchmark(task: MiniZincTestTask) extends MiniZincBasedTest {

    @Test
    def solve(): Unit = {
        super.solve(task.copy(maybeMaximumNumberOfThreads = Some(1), keepFlatZincFile = false))
    }

}

/**
 * Generates test tasks from the Ascheuer TSPTW benchmark instances
 *
 * @author Michael Marte
 */
object AscheuerBenchmark extends VrpTestTaskFactory {

    override protected def modelFilter(file: File) = List("tsptw_yuck.mzn").contains(file.getName)

    override protected def instanceFilter(file: File) = super.instanceFilter(file) && file.getPath.contains("Ascheuer")

    // retrieved on Oct 31th 2020 from http://lopez-ibanez.eu/tsptw-instances,
    // optimality results were obtained with OR-Tools
    override protected val Results = Map(
        ("rbg010a", (671, true)), ("rbg016a", (938, true)), ("rbg016b", (1304, true)),
        ("rbg017.2", (852, true)), ("rbg017", (893, true)), ("rbg017a", (4296, true)),
        ("rbg019a", (1262, true)), ("rbg019b", (1866, true)), ("rbg019c", (4536, true)),
        ("rbg019d", (1356, true)), ("rbg020a", (4689, true)), ("rbg021", (4536, true)),
        ("rbg021.2", (4528, true)), ("rbg021.3", (4528, true)), ("rbg021.4", (4525, true)),
        ("rbg021.5", (4515, true)), ("rbg021.6", (4480, true)), ("rbg021.7", (4479, true)),
        ("rbg021.8", (4478, true)), ("rbg021.9", (4478, true)), ("rbg027a", (5091, true)),
        ("rbg031a", (1863, true)), ("rbg033a", (2069, true)), ("rbg034a", (2222, true)),
        ("rbg035a.2", (2056, true)), ("rbg035a", (2144, true)), ("rbg038a", (2480, true)),
        ("rbg040a", (2378, false)), ("rbg041a", (2598, false)), ("rbg042a", (2772, false)),
        ("rbg048a", (9383, false)), ("rbg049a", (10018, false)), ("rbg050a", (2953, false)),
        ("rbg050b", (9863, false)), ("rbg050c", (10024, false)), ("rbg055a", (3761, false)),
        ("rbg067a", (4625, false)), ("rbg086a", (8400, false)), ("rbg092a", (7160, false)),
        ("rbg125a", (7936, false)), ("rbg132.2", (8200, false)), ("rbg132", (8470, false)),
        ("rbg152.3", (9797, false)), ("rbg152", (10032, false)), ("rbg172a", (10961, false)),
        ("rbg193.2", (12167, false)), ("rbg193", (12547, false)), ("rbg201a", (12967, false)),
        ("rbg233.2", (14549, false)), ("rbg233", (15031, false)))
        .map{case (instanceName, (value, isOptimal)) => ("Ascheuer/" + instanceName, ObjectiveValue(value, isOptimal))}

    // keep models aligned
    private def verifyAgainstCpModel(task: MiniZincTestTask) = task.copy(verificationModelName = "tsptw_cp")

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(amendKnownBestResult).map(verifyAgainstCpModel).map(Array(_)).asJava

}
