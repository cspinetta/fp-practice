package theory

object Functors {

trait Functor[F[_]] {
  /**
    * Identity means: Option(identity(1)) == identity(Option(1))
    * Composition means:
    *     `F.map(Some(3))(x => f.andThen(g)(x))`
    *   is equivalent to:
    *     `F.map( F.map(Some(3))(f) )(g)`
    */
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))

  // also called `unzip`
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  // also called `zip`
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Right(fb) => map(fb)(Right.apply)
    case Left(fa) => map(fa)(Left.apply)
  }
}

  val listFunctor: Functor[List] =
    new Functor[List] {
      def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
        case Nil => Nil
        case a :: as => f(a) :: map(as)(f)
      }
    }

//  def functionFunctor[A, R]: Functor[Function1[A, B]] =
//    new Functor[({type t[x] = Function1[x, R]})#t] {
//      def map[A, B](fa: Function1[A, R])(f: A => B): Function1[B, R] = fa.andThen()
//    }


  implicit val optionFunctor: Functor[Option] =
    new Functor[Option] {
      def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
        case None => None
        case Some(a) => Some(f(a))
      }
    }

  val validName: String => Boolean = _.length > 2
  assert(listFunctor.map(List("Paul", "Ng", "Sam"))(validName) == List(true, false, true))

  optionFunctor.map(Some(5))(_ + 3)


  trait Applicative[F[_]] extends Functor[F] {

    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

    def pure[A](a: A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
  }

  val optionApplicative = new Applicative[Option] {
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = (ff, fa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }

    override def pure[A](a: A): Option[A] = Option(a)
  }

  trait Monad[F[_]] extends Applicative[F] {

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
      flatMap(ff)(x => map(fa)(x))
  }

  val optionMonad = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa match {
        case None => None
        case Some(a) => f(a)
      }

    def pure[A](a: A): Option[A] = Some(a)
  }

  def map[A, B](as: Option[A])
               (f: A => B)
               (implicit functor: Functor[Option]): Option[B] =
    functor.map(as)(f)


  def onlyPositive(value: Int): Option[Int] = {
    if (value >= 0) Some(value)
    else None
  }

  optionMonad.flatMap(Some(5))(onlyPositive)

}

object OptionMonadRunner extends App {

  import Functors._

  val optionMonad = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa match {
        case None => None
        case Some(a) => f(a)
      }

    def pure[A](a: A): Option[A] = Some(a)
  }

  implicit class OptionMonad[A](opt: Option[A]) {
    def flatMap[B](f: A => Option[B]): Option[B] = optionMonad.flatMap(opt)(f)
  }

  def half(number: Int): Option[Int] = {
    if (number % 2 == 0) Some(number / 2)
    else None
  }

  val result = Some(6)
    .flatMap(half) // Some(3)
    .flatMap(half) // None
    .flatMap(half) // None

  println(s"RESULT: $result")
}

object OptionApplicativeRunner extends App {

  import Functors._

  implicit val optionApplicative = new Applicative[Option] {
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = (ff, fa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }

    override def pure[A](a: A): Option[A] = Option(a)
  }

//  implicit class OptionApplicative[A](opt: Option[A]) {
//    def ap[B](ff: Option[A => B]): Option[B] = optionApplicative.ap(ff)(opt)
//  }

  def apOption[A, B](opt: Option[A])
                     (f: Option[A => B])
                     (implicit functor: Applicative[Option]): Option[B] =
    optionApplicative.ap(f)(opt)


  val string: Option[String] = apOption(Some(2))(Some(i => s"The number is: ${i.toString}")) // Some(The number is: 2)

}

