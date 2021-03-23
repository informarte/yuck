package yuck.test

package object util {

    val DefaultNumberOfThreads: Int =
        Option(System.getenv("YUCK_TEST_NUMBER_OF_THREADS")).map(_.toInt)
            .getOrElse(Runtime.getRuntime.availableProcessors)

    val DefaultRuntimeLimitInSeconds: Int =
        Option(System.getenv("YUCK_TEST_RUNTIME_LIMIT")).map(_.toInt).getOrElse(300)

}
