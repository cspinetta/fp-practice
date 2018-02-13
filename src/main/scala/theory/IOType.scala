package theory

import parallel.Par
import parallel.Par.Par
import theory.IOSamples.Factorials

import scala.io.StdIn
import scala.util.Try

object IOType {

  object IOv1 {

    sealed trait IO[A] { self =>

      def run: A

      def ++(io: IO[A]): IO[A] = new IO[A] {
        def run: A = {
          self.run
          io.run
        }
      }

      def map[B](f: A => B): IO[B] = new IO[B] {
        override def run: B = {
          f(self.run)
        }
      }

      def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
        override def run: B = {
          f(self.run).run
        }
      }
    }

    object IO extends Monads.Monad[IO] {
      override def unit[A](a: A): IO[A] = new IO[A] {
        override def run: A = a
      }

      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)

      def apply[A](value: => A): IO[A] = new IO[A] {
        override def run: A = value
      }
    }

    def printLn(value: String): IO[Unit] = new IO[Unit] {
      override def run: Unit = println(value)
    }

    def readLn: IO[String] = new IO[String] {
      override def run: String = StdIn.readLine()
    }
  }

  object IOWithTailRec {

    sealed trait TailRec[A] {

      def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)

      def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))

    }

    // It has finished
    case class Return[A](a: A) extends TailRec[A]

    // It's ready to execute
    case class Suspend[A](resume: () => A) extends TailRec[A]

    // It has other effect to continue the initial execution
    case class FlatMap[A, B](x: TailRec[A], f: A => TailRec[B]) extends TailRec[B]

    object TailRec {

      def apply[A](value: A): TailRec[A] = Return(value)

      @scala.annotation.tailrec
      def run[A](io: TailRec[A]): A = io match {
        case Return(value) => value
        case Suspend(thunk) => thunk()
        case FlatMap(thunk, f) => thunk match {
          case Return(value) => run(f(value))
          case Suspend(susp) => run(f(susp()))
          case FlatMap(innerThunk, g) => run(innerThunk.flatMap(x => g(x).flatMap(f)))
        }
      }
    }

    def forever[A, B](a: TailRec[A]): TailRec[B] = {
      lazy val t: TailRec[B] = forever(a)
      a flatMap (_ => t)
    }

    def printLn(value: String): TailRec[Unit] = Suspend(() => println(value))

    def readLn: TailRec[String] = Suspend(() => StdIn.readLine())
  }

  object IOWithAsync {

    sealed trait Async[A] {
      def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)
      def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))
    }
    case class Return[A](a: A) extends Async[A]
    case class Suspend[A](resume: Par[A]) extends Async[A]
    case class FlatMap[A,B](sub: Async[A],
                            k: A => Async[B]) extends Async[B]

    @annotation.tailrec
    def step[A](async: Async[A]): Async[A] = async match {
      case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
      case FlatMap(Return(x), f) => step(f(x))
      case _ => async
    }

//    def run[A](async: Async[A]): Par[A] = step(async) match {
//      case Return(a) => Par.unit(a)
//      case Suspend(r) => Par.flatMap(r)(a => run(a))
//      case FlatMap(x, f) => x match {
//        case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
//        case _ => sys.error("Impossible; `step` eliminates these cases")
//      }
//    }
  }

//  object IO {
//
//    def empty: IO[Unit] = new IO[Unit] { def run = () }
//  }

}

object IOSamples {

  import IOType.IOv1._

  object Players {

    case class Player(name: String, score: Int)

    def winner(p1: Player, p2: Player): Option[Player] =
      if (p1.score > p2.score) Some(p1)
      else if (p1.score < p2.score) Some(p2)
      else None

    def winnerMsg(p: Option[Player]): String = p map {
      case Player(name, _) => s"$name is the winner!"
    } getOrElse "It's a draw."

    // impure function
    def contest(p1: Player, p2: Player): Unit = println(winnerMsg(winner(p1, p2)))

    // pure function
    def contestPure(p1: Player, p2: Player): IO[Unit] = printLn(winnerMsg(winner(p1, p2)))
    // contestPure() returns an IO value, which simply describes an
    //  action that needs to take place, but doesn’t actually execute it. We say that contest
    //  has (or produces) an effect or is effectful, but it’s only the interpreter of IO (its run
    //  method) that actually has a side effect.
    //
    // In other word, contestPure() only indicates that the message
    //  should be printed to the console, but the responsibility of
    //  interpreting the effect and actually manipulating the console
    //  is held by the run method on IO.
  }

  object Convert {

    def fahrenheitToCelsius(f: Double): Double =
      (f - 32) * 5.0/9.0

    // Without side-effect. it’s a referentially transparent description of a computation with effects.
    // converter.run is the interpreter that will actually execute those effects.
    def converter: IO[Unit] = {
      for {
        _ <- printLn("Enter a temperature in degrees Fahrenheit: ")
        value <- readLn.map(_.toDouble)
        converted <- IO { fahrenheitToCelsius(value) }
        _ <- printLn(converted.toString)
      } yield ()
    }
  }

  object Factorials {

    // Imperative factorial using a mutable IO reference.
//    def factorial(n: Int): IO[Int] = for {
//        acc <- ref(1)
//      _ <- foreachM (1 to n toStream) (i => acc.modify(_ * i).skip)
//      result <- acc.get
//    } yield result

    // introduce a number
    // the factorial is: ----
    // again at the start

    def tryToParseAsInt(input: String): Option[Int] = Try(input.toInt).toOption

    def factorial(n: Int): Int = {
      @scala.annotation.tailrec
      def calculate(acc: Int, next: Int): Int = {
        if (next == 0) 1
        else if (next == 1) acc
        else calculate(acc * next, next - 1)
      }
      calculate(0, n)
    }

    def processInput(input: String): IO[Unit] = {
        tryToParseAsInt(input)
          .map(factorial)
          .map(result =>
            printLn(result.toString)
              .flatMap(_ => {
                requestNumber
              }))
          .getOrElse {
            input match {
              case "q" => printLn("Good bye!")
              case _ =>
                printLn(s"The value entered is not a number: $input")
                  .flatMap(_ => {
                    requestNumber
                  })
            }
          }
    }

    def requestNumber: IO[Unit] = {
      println("starting requestNumber")
      for {
        _ <- printLn("Introduce a number or 'q' tu finish:")
        value <- readLn
        _ <- processInput(value)
      } yield ()
    }

  }

  object LargestNestedFunctions {

    import theory.IOType.IOWithTailRec._

//    def exec: Unit = {
//
//      val f: Int => TailRec[Int] = TailRec.apply
//
//      val g = List.fill(10000)(f).foldLeft(f) { (a, b) =>
//        x => Suspend(() => a(x).flatMap(b))
//      }
//
//      TailRec.run(g(42))
//    }
  }

}

object IORunner extends App {
//  IOSamples.LargestNestedFunctions.exec
//  Factorials.requestNumber.run
}

//sealed trait FileIO[A] {
//
//  def flatMap[B](f: A => FileIO[B]): FileIO[B] = ???
//  def map[B](f: A => B): FileIO[B] = ???
//}
//
//case class OpenFile(path: String) extends FileIO[Handle]
//case class ReadLine(handle: Handle) extends FileIO[String]
//case class WriteLine(handle: Handle, line: String) extends FileIO[Unit]
//case class SeekByLine(handle: Handle, position: Int) extends FileIO[Unit]
//case class Flush(handle: Handle) extends FileIO[Unit]
//
//trait Handle
//
//object FileUtils {
//
//  def copyLine(position: Int): FileIO[Unit] = {
//    for {
//      handle <- OpenFile("/tmp/file.txt")
//      _ <- SeekByLine(handle, position)
//      line <- ReadLine(handle)
//      _ <- WriteLine(handle, line)
//      _ <- Flush(handle)
//    } yield ()
//  }
//}

sealed trait Console[A] { self =>

  def run: A

  def flatMap[B](f: A => Console[B]): Console[B] =
    new Console[B] { def run: B = f(self.run).run }

  def map[B](f: A => B): Console[B] =
    new Console[B] { def run: B = f(self.run) }

  // ...
}

abstract class ConsoleOps {
  def ReadLn: Console[Option[String]]
  def PrintLn(line: String): Console[Unit]
}

class Program(ops: ConsoleOps) {

  def binaryConverter: Console[Unit] = {
    for {
      _    <- ops.PrintLn("Enter a positive integer:")
      line <- ops.ReadLn
      _    <- ops.PrintLn(response(line))
    } yield ()
  }

//  def binaryConverter: Console[Unit] = {
//    ops.PrintLn("Enter a positive integer:")
//      .flatMap(_ => ops.ReadLn
//        .flatMap(line => ops.PrintLn(response(line))
//          .map(_ => () )))
//  }

  private def response(line: Option[String]): String =
    line
      .flatMap(rawInput => stringToInt(rawInput))
      .map(decimalToBinary)
      .map(value => s"Binary representation: $value")
      .getOrElse(s"Wrong value introduced: $line")

  private def stringToInt(value: String): Option[Int] =
    Try(value.toInt).toOption

  private def decimalToBinary(decimal: Int): String =
    Integer.toBinaryString(decimal)
}

object TerminalInterpreter extends ConsoleOps {
  def ReadLn: Console[Option[String]] =
    new Console[Option[String]] { def run = Option(StdIn.readLine()) }

  def PrintLn(line: String): Console[Unit] =
    new Console[Unit] { def run: Unit = println(line) }
}

object FakeInterpreter extends ConsoleOps {
  def ReadLn: Console[Option[String]] =
    new Console[Option[String]] { def run = Some("100") }

  def PrintLn(line: String): Console[Unit] =
    new Console[Unit] { def run: Unit = () }
}

object Runner extends App {

  def onTerminal(): Unit =
    new Program(TerminalInterpreter).binaryConverter.run

  onTerminal()
}
