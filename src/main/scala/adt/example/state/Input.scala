package adt.example.state

import adt.States
import adt.States.State

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

// The rules of the machine are as follows:
// * Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
// * Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
// * Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
// * A machine that’s out of candy ignores all inputs.

object MachineOpts {

  def processInput(input: Input): State[Machine, (Int, Int)] = {
    // disassemble function calls
    //    State[Machine, (Int, Int)](s => {
            // Get machine as a value to be able to manipulate it
    //      val (value, next) = State[Machine, Machine](s => (s, s)).run(s)
    //      def f = (machine: Machine) => {
    //        State[Machine, (Int, Int)](ss => {
    //          val (_, next) = State[Machine, Unit](_ => ((), operateMachine(input, machine))).run(ss)
    //          State[Machine, (Int, Int)](s => {
    //            val (value, next) = State[Machine, Machine](s => (s, s)).run(s)
    //            ((value.coins, value.candies), next)
    //          }).run(next)
    //        })}
    //      f(value).run(next)

    // desugar for-comprehension
//    States.get[Machine]
//      .flatMap(machine => States.set(operateMachine(input, machine))
//        .flatMap { case _ => States.get[Machine]
//          .map(finalMachine => (finalMachine.coins, finalMachine.candies))
//    })

    // with for-comprehension
//    for {
//      machine <- States.get[Machine]
//      _ <- States.set(operateMachine(input, machine))
//      finalMachine <- States.get[Machine]
//    } yield (finalMachine.coins, finalMachine.candies)

    // Using States.modify()
    for {
      _ <- States.modify[Machine](operateMachine(input, _))
      finalMachine <- States.get[Machine]
    } yield (finalMachine.coins, finalMachine.candies)
  }

  private def operateMachine(input: Input, machine: Machine): Machine = {
    (input, machine) match {
      case (_, m@Machine(_, candies, _)) if candies <= 0 => m
      case (Coin, m@Machine(true, candies, coins)) if candies > 0 => m.copy(locked = false, coins = coins + 1)
      case (Coin, m@Machine(false, _, coins)) => m.copy(coins = coins + 1)
      case (Turn, m@Machine(false, candies, _)) => m.copy(locked = true, candies = candies - 1)
      case (Turn, m@Machine(true, _, _)) => m
    }
  }
}

object MachinePlay extends App {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    // Using sequence
    for {
      _ <- States.sequence(inputs.map(input => MachineOpts.processInput(input)))
      machine <- States.get[Machine]
    } yield (machine.coins, machine.candies)

    // Using foldLeft
//    val stateMachine = State[Machine, (Int, Int)](m => ((m.coins, m.candies), m))
//    inputs.foldLeft(stateMachine)((m, input) => m.flatMap(_ => MachineOpts.processInput(input)))
  }

  def exampleExec: Unit = {
    val initialMachine = Machine(locked = true, candies = 5, coins = 10)
    val inputs = List(Coin, Turn, Coin, Turn, Turn, Turn, Turn, Coin, Turn, Coin, Turn)
    println(s"Simulate Machine with a Initial Machine: $initialMachine, and inputs: $inputs")
    val finalMachineState = simulateMachine(inputs).run(initialMachine)
    println(s"Final Machine State: $finalMachineState")
  }

  exampleExec

}

// book solution
//object Candy {
//  def update = (i: Input) => (s: Machine) =>
//    (i, s) match {
//      case (_, Machine(_, 0, _)) => s
//      case (Coin, Machine(false, _, _)) => s
//      case (Turn, Machine(true, _, _)) => s
//      case (Coin, Machine(true, candy, coin)) =>
//        Machine(false, candy, coin + 1)
//      case (Turn, Machine(false, candy, coin)) =>
//        Machine(true, candy - 1, coin)
//    }
//
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
//    _ <- States.sequence(inputs map (States.modify[Machine] _ compose update))
//    s <- States.get
//  } yield (s.coins, s.candies)
//}
