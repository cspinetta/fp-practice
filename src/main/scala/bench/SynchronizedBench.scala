package bench

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import org.openjdk.jmh.annotations._
import sample.{AtomicCounter, SynchronizedCounter}

@State(Scope.Group)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(value = 1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
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
