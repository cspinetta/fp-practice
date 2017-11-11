package adt

import scala.annotation.tailrec

object States {

  trait RNG {

    def nextInt: (Int, RNG)
  }

  // State action (also called State transition).
  // It's NOT a randomly generated A, but a _program_ that describe
  // how generate a randomly A from a RNG state
  type Rand[+A] = RNG => (A, RNG)

  object RNG {

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (value, next) = rng.nextInt
      (if (value < 0) -value else value, next)
    }

    def double(rng: RNG): (Double, RNG) = {
      val (value, next) = nonNegativeInt(rng)
      (value / (Int.MaxValue.toDouble + 1), next)
    }

    def randomPair(rng: RNG): ((Int,Int), RNG) = {
      val (value, next) = rng.nextInt
      val (value2, next2) = next.nextInt
      ((value, value2), next2)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (value, next) = rng.nextInt
      val (value2, next2) = double(next)
      ((value, value2), next2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((v1, v2), next) = intDouble(rng)
      ((v2, v1), next)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (value1, next1) = double(rng)
      val (value2, next2) = double(next1)
      val (value3, next3) = double(next2)
      ((value1, value2, value3), next3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      val (values, next) = (1 to count).foldLeft((List.empty[Int], rng)) { case ((list, r), _) =>
        val (value, next) = rng.nextInt
        (value :: list, next)
      }
      (values.reverse, next)
    }

    // tail-recursive solution
    def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
      @tailrec
      def nextValue(result: List[Int], r: RNG, count: Int): (List[Int], RNG) = {
        if (count < 1) (result, r)
        else {
          val (value, next) = rng.nextInt
          nextValue(value :: result, next, count - 1)
        }
      }
      nextValue(List.empty, rng, count)
    }

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (value, next) = s(rng)
      (f(value), next)
    }

    def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

    val doubleMoreElegant: Rand[Double] = map(int)(i => i / (Int.MaxValue.toDouble + 1))

    def map2[A, B, C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (v1, next1) = a(rng)
      val (v2, next2) = b(next1)
      (f(v1, v2), next2)
    }

    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] = both(int, double)

    val randDoubleInt: Rand[(Double, Int)] = both(double, int)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List.empty[A]))((value, acc) =>
      map2(acc, value)((partialList, v) => v :: partialList))

    def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
      fs.foldRight((List.empty[A], rng)) { case (v, (acc, next)) => {
        map(v)(v => v :: acc)
        val (value, next2) = v(next)
        (value :: acc, next2)
      }}
    }

    def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

//    def nonNegativeLessThan(n: Int): Rand[Int] = map(nonNegativeInt) { i =>
//      val mod = i % n
//      if (i + (n-1) - mod >= 0) mod else nonNegativeLessThan(n)(???)
//    }

    def nonNegativeLessThanWithoutFlatMap(n: Int): Rand[Int] = rng => {
      val (i, next) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n-1) - mod >= 0) (mod, next) else nonNegativeLessThanWithoutFlatMap(n)(next)
    }

    def flatMap[A,B](s: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
      val (value, next) = s(rng)
      f(value)(next)
    }

    def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

    def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(f.andThen(unit))
    def map2ViaFlatMap[A, B, C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(a)(x => map(b)(f.curried(x)))
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

  }

  // Generalization of State Monad
  type StateT[S, +A] = S => (A, S)

  case class State[S, A](run: StateT[S, A]) {

    // Enrich via package object's implicits

    def get(): State[S, S] = State(s => (s, s))
    def set[T](s: T): State[T, Unit] = State(_ => ((), s))
  }

  def unit[S, A](a: A): State[S, A] = State(x => (a, x))

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] = State(ss => {
    val (value, next) = s.run(ss)
    (f(value), next)
  })

  def map2[S, A, B, C](s1: State[S, A], s2: State[S, B])(f: (A, B) => C): State[S, C] = State(ss => {
    val (value, next) = s1.run(ss)
    val (value2, next2) = s2.run(next)
    (f(value, value2), next2)
  })

  def flatMap[S, A, B](s: State[S, A])(f: A => State[S, B]): State[S, B] = State(ss => {
    val (value, next) = s.run(ss)
    f(value).run(next)
  })

  def sequence[S, A](s: List[State[S, A]]): State[S, List[A]] = {
    s.foldLeft(unit[S , List[A]](Nil))(map2(_, _)((partialList, elem) => elem :: partialList))
  }

  type RandFromGeneral[A] = StateT[RNG, A]

//  val ns: Rand[List[Int]] = for {
//    a <- int
//  }
}

object StateRunner extends App {

  import States._

  val newRNG = SimpleRNG(20)

  val (value1, next1) = newRNG.nextInt
  val (value2, next2) = next1.nextInt

  println(s"Values: $value1, $value2")
}

