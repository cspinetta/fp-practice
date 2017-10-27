package theory

import adt.States
import adt.States.State

object Monads {

  // A purely algebraic interface, that satisfies the identity and associative laws
  // and provide a lot of combinator functions from a set of primitives. These primitives could be:
  //
  // unit + flatMap
  // unit + compose
  // unit + join + map
  //
  // In other words:
  // A monad is an implementation of one of the minimal sets of monadic
  // combinators, satisfying the laws of associativity and identity
  //
  // But what is exactly??
  // We could say that monads provide a context for introducing and binding variables,
  // and performing variable substitution. For example:
  //
  // scala> for {
  //   a <- Id("Hello, ")
  //   b <- Id("monad!")
  // } yield a + b
  // res1: Id[java.lang.String] = Id(Hello, monad!)
  //
  // It's equivalent to:
  //
  // scala> "Hello, " + "monad!"
  // res2: java.lang.String = Hello, monad!
  //
  trait Monad[F[_]] extends Functors.Functor[F] {

    // possible primitives:

    // unit + flatMap
    // unit + compose
    // unit + join + map

    // primitives:
    def unit[A](a: A): F[A]

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] // = flatMapViaJoinAndMap(fa)(f)

    // combinations:
    override def map[A, B](fa: F[A])(f: (A) => B): F[B] = flatMap(fa)(f.andThen(unit))

    type T = String Map Int
    val t: T = Map[String, Int]("xxx" -> 2)

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
//      val relativeToFlatMapAndMap: F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
      flatMap(fa)(a => flatMap(fb)(b => unit(f(a, b))))
    }

    def traverse[A, B](fa: List[A])(f: A => F[B]): F[List[B]] = {
      fa.foldRight(unit(List.empty[B]))((a, b) =>  map2(f(a), b)(_ :: _)) // flatMap(b)((bb) => map(f(a))(aa => aa :: bb)))
    }

    def sequence[A](fa: List[F[A]]): F[List[A]] = {
      traverse(fa)(identity)
    }

    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
      sequence(List.fill(n)(ma)) // traverse((1 to n).toList)(_ => ma)
    }

    def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
      ms.foldRight(unit(List.empty[A]))((a, b) => map2(f(a), b)((aIsValid, acc) => if (aIsValid) a :: acc else acc))
    }

    // Kleisli composition function
    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

    // proving the associative law:
    // compose(compose(f, g), h) == compose(f, compose(g, h))

    // providing the identity laws:
    // compose(f, unit) == f
    // compose(unit, f) == f

    def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)(())

    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

    def flatMapViaJoinAndMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

    def composeViaJoinAndMap[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))
  }



  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    override def unit[A](a: A): State[S, A] = S => (a, S)
    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]) = States.flatMap(fa)(f)
  }

  def idM = new Monad[Id] {
    override def unit[A](a: A) = Id(a)
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
  }

}

// It's just a simple wrapper
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}
