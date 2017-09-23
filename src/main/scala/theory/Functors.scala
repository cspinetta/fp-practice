package theory

object Functors {

  trait Functor[F[_]] {

    def map[A, B](fa: F[A])(f: A => B): F[B]

    // also called `unzip`
    def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

    // also called `zip`
    def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
      case Right(fb) => map(fb)(Right.apply)
      case Left(fa) => map(fa)(Left.apply)
    }
  }

  val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa.map(f)
  }
}
