package sample

object LockRunner extends App {

  val threads = 10
  val quantityPerThread = 1000
  val expected: Int = quantityPerThread * threads
  val generalLock = new Object()
  var count = 0

//  def syncMethod(name: String)(lock: Object): Unit = lock.synchronized {
//    count += 1
////    println(s"Hello $name!")
////    if (name != "Carl") syncMethod("Carl")
//  }

  def callback(lock: Object): Unit = lock.synchronized(count += 1)

  (1 to threads)
    .map(_ => {
//      val t = new Thread(Executor(1000, callback, Some(generalLock)))
      val t = new Thread(Executor(1000, callback, Some(generalLock)))
      t.start()
      t
    })
    .foreach(_.join())

  println(s"Count is: $count")
  println(s"Expected: $expected")
  assert(count == expected)

  println(s"Good bye bitch!")
//  syncMethod("Cristian")


}

case class Executor(quantity: Int, callback: Object => Unit, lock: Option[Object]) extends Runnable {
  private val _lock = lock getOrElse new Object()
  override def run(): Unit = (1 to quantity).foreach(_ => callback(_lock))
}
