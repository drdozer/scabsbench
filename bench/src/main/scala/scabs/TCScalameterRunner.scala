package scabs

import org.scalameter.api.Bench

object TCScalameterRunner extends TCBenchmarkRunner[Bench[Double]] {

  private def runWithVariety[C[_[_]], M[_], R[_], I](bench: Bench[Double], benchmark: TCBenchmark[C, R, I], variety: TCBenchVariety[C, M]): Unit = {
    implicit val instance = variety.instance
    println(s"Running benchmark ${benchmark.name} for ${variety.name}")
    bench.measure method variety.name in {
      bench.using(benchmark.gen[M]) in benchmark.run[M]
    }
    println(s"Done")
  }

  override def runSuite[C[_[_]]](ctx: Bench[Double], suite: TCBenchSuite[C]): Unit = {
    println(s"Running suite")
    suite.benchmarks.foreach { benchmark =>
      ctx.performance of benchmark.name in {
        suite.varieties.foreach(runWithVariety[C, Nothing, Nothing, Nothing](ctx, benchmark, _))
      }
    }
    println(s"Completed suite")
  }

}
