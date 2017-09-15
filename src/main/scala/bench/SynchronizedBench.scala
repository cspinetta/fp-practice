package bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import sample.{AtomicCounter, SynchronizedCounter}

@State(Scope.Group)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(value = 1)
@Warmup(iterations = 1)
@Measurement(iterations = 1)
class SynchronizedBench {

  val syncCounter = new SynchronizedCounter()
  val atomicCounter = new AtomicCounter()

  @Benchmark
  @Group("locks")
  def sync(): Unit = {
    syncCounter.increment
  }

  @Benchmark
  @Group("locks")
  def atomic(): Unit = {
    atomicCounter.increment
  }
}
