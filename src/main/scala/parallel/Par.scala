package parallel

import java.util.concurrent.{ExecutorService, _}

import scala.collection.immutable
import scala.concurrent.duration._

//sealed trait Par[A]
//
//case class UnitPar[A](value: A) extends Par[A]
//case class ForkPar[A](thunk: () => A) extends Par[A]

object Par {

  import implicits._

  type Par[A] = ExecutorService => Future[A]

  // promotes a constant value to a parallel computation
  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  // marks a computation for concurrent evaluation. The evaluation won’t actually occur until forced by run
  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => es.submit(() => a(es).get)

  // wraps its unevaluated argument in a Par and marks it for concurrent evaluation
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // combines the results of two parallel computations with a binary function
  def map2[A, B, C](u1: Par[A], u2: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    UnitFuture(f(u1(es).get, u2(es).get))
  }

  def map[A, B](fromPar: Par[A])(f: A => B): Par[B] = {
    fromPar.map2(Par.unit(()))((a, _) => f(a))
  }

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def sequence[A](l: List[Par[A]]): Par[List[A]] = {
    l.foldRight(unit(List.empty[A]))((par, accResult) => par.map2(accResult)(_ :: _))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val list: List[Par[B]] = ps.map(asyncF(f))
    sequence(list)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
//    as.foldRight(unit(List.empty[A]))((a, b) => if (f(a)) unit(a).map2(b)(_ :: _) else b)
//    val pars: List[Par[List[A]]] = as.map
//    val pars: Par[List[List[A]]] = fork(parMap(as)((a) => if (f(a)) List(a) else List.empty[A]))
    val pars: List[Par[List[A]]] = as map ((elem: A) => lazyUnit { if (f(elem)) List(elem) else List.empty })
    map(sequence(pars))(_.flatten)
  }

  object implicits {
//    implicit class InfixMap2[A](p: Par[A]) {
//      def map2[B, C](other: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, other)(f)
//    }
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    class ParOps[A](p: Par[A]) {
      def map2[B, C](other: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, other)(f)
    }
  }

  // extracts a value from a Par by actually performing the computation
  def run[A](s: ExecutorService)(par: Par[A]): Future[A] = par(s) // par match { case ForkPar(thunk) => thunk(); case UnitPar(value) => value }


  case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get
  }
}

object ParUtils {

  import Par._
  import Par.implicits._

  def sum(l: Seq[Int]): Par[Int] = {
    if (l.size <= 1) Par.unit(l.headOption getOrElse 0)
    else {
      val (l1, l2) = l.splitAt(l.size / 2)
      val sumL: Par[Int] = sum(l1)
      val sumR: Par[Int] = sum(l2)
      sumL.map2(sumR)(_ + _)
    }
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
    parList.map2(Par.unit(()))((l1, _) => l1.sorted)
  }

  def sortParViaMap(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)
}

