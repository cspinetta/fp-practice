package adt

import scala.annotation.tailrec

sealed trait ListMonad[+A] {

  def head: A
  def tail: ListMonad[A]

}

object ListMonad {

  def apply[A](values: A*): ListMonad[A] = values match {
    case Seq() => NilMonad
    case _ => ConsMonad(values.head, apply(values.tail: _*))
  }

  def empty[A]: ListMonad[A] = NilMonad

  @tailrec
  def foldLeft[A, B](elems: ListMonad[A], z: B)(f: (A, B) => B): B = elems match {
    case ConsMonad(v, tail) => foldLeft(tail, f(v, z))(f)
    case NilMonad => z
  }

  def foldRight[A, B](elems: ListMonad[A], z: B)(f: (A, B) => B): B = elems match {
    case ConsMonad(v, tail) => f(v, foldRight(tail, z)(f))
    case NilMonad => z
  }

  def foldRightViaFoldLeft[A, B](elems: ListMonad[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(elems), z)(f)
  }

  def reverse[A](elems: ListMonad[A]): ListMonad[A] =
    foldLeft(elems, ListMonad.empty[A])((elem, partial) => ConsMonad(elem, partial))

  def append[A](list1: ListMonad[A], list2: ListMonad[A]): ListMonad[A] =
    list1 match {
      case ConsMonad(v, tail) => ConsMonad(v, append(tail, list2))
      case NilMonad => list2
    }

  def appendViaFoldRight[A](list1: ListMonad[A], list2: ListMonad[A]): ListMonad[A] =
    foldRight(list1, list2)((v, l2) => ConsMonad(v, l2))

  def concat[A](l: ListMonad[ListMonad[A]]): ListMonad[A] =
    foldRight(l, ListMonad.empty[A])((value,result) => append(result, value))
}

case class ConsMonad[A](value: A, tail: ListMonad[A]) extends ListMonad[A] {
  override def head = value
}

case object NilMonad extends ListMonad[Nothing] {
  override def head = throw new RuntimeException("NilMonad doesn't have a value")
  override def tail = throw new RuntimeException("NilMonad doesn't have tail")
}
