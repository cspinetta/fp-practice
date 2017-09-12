package sample

import java.util.concurrent.atomic.AtomicInteger

class AtomicCounter extends Counter {
  private val counter = new AtomicInteger(0)

  override def increment: Unit = counter.incrementAndGet()

  override def count: Int = counter.get()
}
