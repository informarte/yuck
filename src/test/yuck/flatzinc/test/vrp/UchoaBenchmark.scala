package yuck.flatzinc.test.vrp

import java.io.File

import org.junit._

import scala.jdk.CollectionConverters._

import yuck.flatzinc.test.util._
import yuck.test.util.ParallelParameterizedTestRunner

/**
 * Runs the Uchoa CVRP benchmark
 *
 * @author Michael Marte
 */
@Test
@FixMethodOrder(runners.MethodSorters.NAME_ASCENDING)
@runner.RunWith(classOf[ParallelParameterizedTestRunner])
final class UchoaBenchmark(task: MiniZincTestTask) extends MiniZincBasedTest {

    @Test
    def solve: Unit = {
        super.solve(task.copy(dataAssignments = Map(("MaxKToMinKRatio", "2")), maybeMaximumNumberOfThreads = Some(1), keepFlatZincFile = false))
    }

}

/**
 * Generates test tasks from the Uchoa CVRP benchmark instances
 *
 * @author Michael Marte
 */
object UchoaBenchmark extends VrpTestTaskFactory {

    override protected def modelFilter(file: File) = List("cvrp_yuck.mzn").contains(file.getName)

    override protected def instanceFilter(file: File) = super.instanceFilter(file) && file.getPath.contains("Uchoa")

    // retrieved on Dec 2nd 2020 from http://vrp.galgos.inf.puc-rio.br/
    override protected val Results = Map(
        ("X-n101-k25", (27591, true)), ("X-n106-k14", (26362, true)), ("X-n110-k13", (14971, true)),
        ("X-n115-k10", (12747, true)), ("X-n120-k6", (13332, true)), ("X-n125-k30", (55539, true)),
        ("X-n129-k18", (28940, true)), ("X-n134-k13", (10916, true)), ("X-n139-k10", (13590, true)),
        ("X-n143-k7", (15700, true)), ("X-n148-k46", (43448, true)), ("X-n153-k22", (21220, true)),
        ("X-n157-k13", (16876, true)), ("X-n162-k11", (14138, true)), ("X-n167-k10", (20557, true)),
        ("X-n172-k51", (45607, true)), ("X-n176-k26", (47812, true)), ("X-n181-k23", (25569, true)),
        ("X-n186-k15", (24145, true)), ("X-n190-k8", (16980, true)), ("X-n195-k51", (44225, true)),
        ("X-n200-k36", (58578, true)), ("X-n204-k19", (19565, true)), ("X-n209-k16", (30656, true)),
        ("X-n214-k11", (10856, true)), ("X-n219-k73", (117595, true)), ("X-n223-k34", (40437, true)),
        ("X-n228-k23", (25742, true)), ("X-n233-k16", (19230, true)), ("X-n237-k14", (27042, true)),
        ("X-n242-k48", (82751, true)), ("X-n247-k50", (37274, true)), ("X-n251-k28", (38684, true)),
        ("X-n256-k16", (18839, false)), ("X-n261-k13", (26558, false)), ("X-n266-k58", (75478, true)),
        ("X-n270-k35", (35291, true)), ("X-n275-k28", (21245, true)), ("X-n280-k17", (33503, false)),
        ("X-n284-k15", (20215, true)), ("X-n289-k60", (95151, true)), ("X-n294-k50", (47161, false)),
        ("X-n298-k31", (34231, true)), ("X-n303-k21", (21736, false)), ("X-n308-k13", (25859, false)),
        ("X-n313-k71", (94043, false)), ("X-n317-k53", (78355, true)), ("X-n322-k28", (29834, true)),
        ("X-n327-k20", (27532, false)), ("X-n331-k15", (31102, true)), ("X-n336-k84", (139111, false)),
        ("X-n344-k43", (42050, false)), ("X-n351-k40", (25896, false)), ("X-n359-k29", (51505, false)),
        ("X-n367-k17", (22814, false)), ("X-n376-k94", (147713, true)), ("X-n384-k52", (65938, false)),
        ("X-n393-k38", (38260, true)), ("X-n401-k29", (66154, false)), ("X-n411-k19", (19712, false)),
        ("X-n420-k130", (107798, true)), ("X-n429-k61", (65449, false)), ("X-n439-k37", (36391, true)),
        ("X-n449-k29", (55233, false)), ("X-n459-k26", (24139, false)), ("X-n469-k138", (221824, true)),
        ("X-n480-k70", (89449, false)), ("X-n491-k59", (66483, false)), ("X-n502-k39", (69226, false)),
        ("X-n513-k21", (24201, false)), ("X-n524-k153", (154593, true)), ("X-n536-k96", (94846, false)),
        ("X-n548-k50", (86700, true)), ("X-n561-k42", (42717, false)), ("X-n573-k30", (50673, false)),
        ("X-n586-k159", (190316, false)), ("X-n599-k92", (108451, false)), ("X-n613-k62", (59535, false)),
        ("X-n627-k43", (62164, false)), ("X-n641-k35", (63692, false)), ("X-n655-k131", (106780, true)),
        ("X-n670-k130", (146332, false)), ("X-n685-k75", (68205, false)), ("X-n701-k44", (81923, false)),
        ("X-n716-k35", (43379, false)), ("X-n733-k159", (136187, false)), ("X-n749-k98", (77269, false)),
        ("X-n766-k71", (114418, false)), ("X-n783-k48", (72393, false)), ("X-n801-k40", (73305, false)),
        ("X-n819-k171", (158121, false)), ("X-n837-k142", (193737, false)), ("X-n856-k95", (88965, false)),
        ("X-n876-k59", (99299, false)), ("X-n895-k37", (53860, false)), ("X-n916-k207", (329179, false)),
        ("X-n936-k151", (132715, false)), ("X-n957-k87", (85465, false)), ("X-n979-k58", (118976, false)),
        ("X-n1001-k43", (72355, false)))
        .map{case (instanceName, (value, isOptimal)) => ("Uchoa/X/" + instanceName, ObjectiveValue(value, isOptimal))}

    // keep models aligned
    private def verifyAgainstCpModel(task: MiniZincTestTask) = task.copy(verificationModelName = "cvrp_cp")

    @runners.Parameterized.Parameters(name = "{index}: {0}")
    def parameters = tasks.map(amendKnownBestResult).map(verifyAgainstCpModel).map(Array(_)).asJava

}
