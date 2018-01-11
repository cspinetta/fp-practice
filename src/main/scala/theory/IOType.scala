package theory

import scala.io.StdIn

object IOType {

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

//  object IO {
//
//    def empty: IO[Unit] = new IO[Unit] { def run = () }
//  }

}

object IOSamples {

  import IOType._

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

}
