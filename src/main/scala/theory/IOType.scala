package theory

object IOType {

  trait IO { self =>

    def run: Unit

    def ++(io: IO): IO = new IO {
      def run: Unit = {
        self.run
        io.run
      }
    }
  }

  def printLn(value: String): IO = new IO {
    override def run: Unit = println(value)
  }

  object IO {

    def empty: IO = new IO { def run = () }
  }

}

object IOSamples {

  import IOType._

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
  def contestPure(p1: Player, p2: Player): IO = printLn(winnerMsg(winner(p1, p2)))
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
