package sample

import java.util.concurrent.{Executors, TimeUnit}

import parallel.{Interpreter, ParUtils}

object ParRunner extends App {

  private val nThreads = 10
  private val executor = Executors.newFixedThreadPool(nThreads)

  val par = ParUtils.maxPar((1 to 1000).toList)
  val maxFut = Interpreter.run(executor)(par)
  val max = maxFut.get(1, TimeUnit.SECONDS)

  println(s"The max value is: $max")

}
