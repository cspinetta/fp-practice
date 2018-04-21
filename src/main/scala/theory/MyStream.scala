package theory


class MyStream {

}

object MyStreamExamples {

  object ImperativeVersion {
    import IOType.IOv1._

    def linesGt40k(filename: String): IO[Boolean] = IO {
      val src = io.Source.fromFile(filename)
      try {
        var count = 0
        val lines: Iterator[String] = src.getLines
        while (count <= 40000 && lines.hasNext) {
          lines.next
          count += 1
        }
        count > 40000
      }
      finally src.close
    }
  }

  /**
    * A Stream transducers or Stream processors specifies a '''transformation''' from
    * one stream to another.
    */
  object StreamTransducer {

    /**
      * It's not just a function like `Stream[I] => Stream[O]`. Instead, it's a state machine
      * that must be driven forward with a `driver`:
      *
      *
      *  - `Emit(head, tail)` indicated to the driver that the `head` value should be emitted to the
      * output stream, and the machine should then be transitioned to the `tail` state.
      *
      *  - `Await(recv)` requests a value from the input stream. The driver should pass the
      * next available value to the `recv` or `None` if the input has no more elements.
      *
      *  - `Halt()` indicates to the driver that no more elements should be read from the input
      * or emitted to the output.
      *
      */
    sealed trait Process[-I, +O] {

      def apply(stream: Stream[I]): Stream[O] = this match {
        case Halt() => Stream()
        case Await(recv) =>
          stream match {
            case h #:: t => recv(Some(h))(t)
            case xs => recv(None)(xs)
          }
        case Emit(h, t) => h #:: t(stream)
      }

      def repeat: Process[I, O] = {
        def go(p: Process[I, O]): Process[I, O] = p match {
          case Halt() => go(this)
          case Await(recv) => Await {
            case None => recv(None)
            case i => go(recv(i))
          }
          case Emit(h, t) => Emit(h, go(t))
        }
        go(this)
      }
    }
    case class Emit[-I, +O](head: O, tail: Process[I, O] = Halt()) extends Process[I, O]
    case class Await[-I, +O](recv: Option[I] => Process[I, O]) extends Process[I, O]
    case class Halt[-I, +O]() extends Process[I, O]

    object Process {

      def liftOne[I, O](f: I => O): Process[I, O] = Await {
        case Some(i) => Emit(f(i))
        case None => Halt()
      }

      def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

      def filter[I](f: I => Boolean): Process[I, I] = Await[I, I] {
        case Some(i) if f(i) => Emit(i)
        case _ => Halt()
      } repeat

      def addToEach(addValue: Double = 0): Process[Double, Double] = Await[Double, Double] {
        case Some(value) => Emit(addValue + value)
        case None => Halt()
      } repeat

      def sum: Process[Double, Double] = {
        def go(acc: Double): Process[Double, Double] = Await[Double, Double] {
          case Some(value) => Emit(acc + value, go(acc + value))
          case None => Halt()
        }
        go(acc = 0)
      }

      def take[I](n: Int): Process[I, I] = {
        def go(current: Int): Process[I, I] = Await[I, I] {
          case Some(i) if current < n => Emit(i, go(current + 1))
          case _                      => Halt()
        }
        go(current = 0)
      }

      def drop[I](n: Int): Process[I, I] = {
        def go(current: Int): Process[I, I] = Await[I, I] {
          case Some(_) if current < n => go(current = current + 1)
          case Some(i) => Emit(i, go(current + 1).repeat)
          case None => Halt()
        }
        go(current = 0)
      }

      def takeWhile[I](f: I => Boolean): Process[I, I] = {
        def go(taking: Boolean): Process[I, I] = Await[I, I] {
          case Some(i) if taking && f(i) => Emit(i, go(taking))
          case _ => Halt()
        }
        go(taking = true)
      }

      def dropWhile[I](f: I => Boolean): Process[I, I] = {
        def go(dropping: Boolean): Process[I, I] = Await[I, I] {
          case Some(i) if dropping && f(i) => go(dropping)
          case Some(i) => Emit(i, go(dropping = false).repeat)
          case None => Halt()
        }
        go(dropping = true)
      }
    }
  }
}
