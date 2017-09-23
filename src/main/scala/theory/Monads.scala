package theory

object Monads {

  trait Monad[F[_]] extends Functors.Functor[F] {

    // primitives:
    def unit[A](a: A): F[A]

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    // combinations:
    override def map[A, B](fa: F[A])(f: (A) => B): F[B] = flatMap(fa)(f.andThen(unit))

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
//      val relativeToFlatMapAndMap: F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
      flatMap(fa)(a => flatMap(fb)(b => unit(f(a, b))))
    }
  }

}
