package yuck.flatzinc.test.vrp

import java.io.File

import org.junit.*

import scala.jdk.CollectionConverters.*

import yuck.flatzinc.test.util.*
import yuck.test.util.ParallelParameterizedTestRunner

/**
 * Runs the Dumas TSPTW benchmark
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[ParallelParameterizedTestRunner])
final class DumasBenchmark(task: ZincTestTask) extends ZincBasedTest {

    @Test
    def solve(): Unit = {
        super.solve(task)
    }

}

/**
 * Generates test tasks from the Dumas TSPTW benchmark instances
 *
 * @author Michael Marte
 */
object DumasBenchmark extends VrpTestTaskFactory {

    override protected def modelFilter(file: File) = List("tsptw_yuck.mzn").contains(file.getName)

    override protected def instanceFilter(file: File) = super.instanceFilter(file) && file.getPath.contains("Dumas")

    // retrieved on Oct 31th 2020 from http://lopez-ibanez.eu/tsptw-instances
    override protected val Results = Map(
        ("n20w20.001", 378), ("n20w20.002", 286), ("n20w20.003", 394), ("n20w20.004", 396), ("n20w20.005", 352),
        ("n20w40.001", 254), ("n20w40.002", 333), ("n20w40.003", 317), ("n20w40.004", 388), ("n20w40.005", 288),
        ("n20w60.001", 335), ("n20w60.002", 244), ("n20w60.003", 352), ("n20w60.004", 280), ("n20w60.005", 338),
        ("n20w80.001", 329), ("n20w80.002", 338), ("n20w80.003", 320), ("n20w80.004", 304), ("n20w80.005", 264),
        ("n20w100.001", 237), ("n20w100.002", 222), ("n20w100.003", 310), ("n20w100.004", 349), ("n20w100.005", 258),
        ("n40w20.001", 500), ("n40w20.002", 552), ("n40w20.003", 478), ("n40w20.004", 404), ("n40w20.005", 499),
        ("n40w40.001", 465), ("n40w40.002", 461), ("n40w40.003", 474), ("n40w40.004", 452), ("n40w40.005", 453),
        ("n40w60.001", 494), ("n40w60.002", 470), ("n40w60.003", 408), ("n40w60.004", 382), ("n40w60.005", 328),
        ("n40w80.001", 395), ("n40w80.002", 431), ("n40w80.003", 412), ("n40w80.004", 417), ("n40w80.005", 344),
        ("n40w100.001", 429), ("n40w100.002", 358), ("n40w100.003", 364), ("n40w100.004", 357), ("n40w100.005", 377),
        ("n60w20.001", 551), ("n60w20.002", 605), ("n60w20.003", 533), ("n60w20.004", 616), ("n60w20.005", 603),
        ("n60w40.001", 591), ("n60w40.002", 621), ("n60w40.003", 603), ("n60w40.004", 597), ("n60w40.005", 539),
        ("n60w60.001", 609), ("n60w60.002", 566), ("n60w60.003", 485), ("n60w60.004", 571), ("n60w60.005", 569),
        ("n60w80.001", 458), ("n60w80.002", 498), ("n60w80.003", 550), ("n60w80.004", 566), ("n60w80.005", 468),
        ("n60w100.001", 515), ("n60w100.002", 538), ("n60w100.003", 560), ("n60w100.004", 510), ("n60w100.005", 451),
        ("n80w20.001", 616), ("n80w20.002", 737), ("n80w20.003", 667), ("n80w20.004", 615), ("n80w20.005", 748),
        ("n80w40.001", 606), ("n80w40.002", 618), ("n80w40.003", 674), ("n80w40.004", 557), ("n80w40.005", 695),
        ("n80w60.001", 554), ("n80w60.002", 633), ("n80w60.003", 651), ("n80w60.004", 619), ("n80w60.005", 575),
        ("n80w80.001", 624), ("n80w80.002", 592), ("n80w80.003", 589), ("n80w80.004", 594), ("n80w80.005", 570),
        ("n100w20.001", 738), ("n100w20.002", 715), ("n100w20.003", 762), ("n100w20.004", 799), ("n100w20.005", 774),
        ("n100w40.001", 770), ("n100w40.002", 653), ("n100w40.003", 736), ("n100w40.004", 651), ("n100w40.005", 699),
        ("n100w60.001", 655), ("n100w60.002", 659), ("n100w60.003", 744), ("n100w60.004", 764), ("n100w60.005", 661),
        ("n150w20.001", 918), ("n150w20.002", 864), ("n150w20.003", 834), ("n150w20.004", 854), ("n150w20.005", 846),
        ("n150w40.001", 917), ("n150w40.002", 941), ("n150w40.003", 727), ("n150w40.004", 764), ("n150w40.005", 824),
        ("n150w60.001", 859), ("n150w60.002", 782), ("n150w60.003", 793), ("n150w60.004", 819), ("n150w60.005", 840),
        ("n200w20.001", 1016), ("n200w20.002", 958), ("n200w20.003", 1046), ("n200w20.004", 984), ("n200w20.005", 1014),
        ("n200w40.001", 1023), ("n200w40.002", 948), ("n200w40.003", 933), ("n200w40.004", 980), ("n200w40.005", 1033))
        // "All instances were proposed and solved to optimality by Dumas et al."
        .map{case (instanceName, value) => ("Dumas/" + instanceName, ObjectiveValue(value, true))}

    // keep models aligned
    private def verifyAgainstCpModel(task: ZincTestTask) = task.copy(verificationModelName = "tsptw_cp")

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(amendKnownBestResult).map(verifyAgainstCpModel).map(Array(_)).asJava

}
