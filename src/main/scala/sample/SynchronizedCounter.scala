package sample

class SynchronizedCounter extends Counter {
  private var counter = 0

  override def increment: Unit = synchronized(counter += 1)

  override def count: Int = counter
}
