package adt

import scala.annotation.tailrec

sealed trait ListADT[+A] {

  def head: A
  def tail: ListADT[A]

}

object ListADT {

  def apply[A](values: A*): ListADT[A] = values match {
    case Seq() => NilADT
    case _ => ConsADT(values.head, apply(values.tail: _*))
  }

  def empty[A]: ListADT[A] = NilADT

  @tailrec
  def foldLeft[A, B](elems: ListADT[A], z: B)(f: (A, B) => B): B = elems match {
    case ConsADT(v, tail) => foldLeft(tail, f(v, z))(f)
    case NilADT => z
  }

  def foldRight[A, B](elems: ListADT[A], z: B)(f: (A, B) => B): B = elems match {
    case ConsADT(v, tail) => f(v, foldRight(tail, z)(f))
    case NilADT => z
  }

  def foldRightViaFoldLeft[A, B](elems: ListADT[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(elems), z)(f)
  }

  def reverse[A](elems: ListADT[A]): ListADT[A] =
    foldLeft(elems, ListADT.empty[A])((elem, partial) => ConsADT(elem, partial))

  def append[A](list1: ListADT[A], list2: ListADT[A]): ListADT[A] =
    list1 match {
      case ConsADT(v, tail) => ConsADT(v, append(tail, list2))
      case NilADT => list2
    }

  def appendViaFoldRight[A](list1: ListADT[A], list2: ListADT[A]): ListADT[A] =
    foldRight(list1, list2)((v, l2) => ConsADT(v, l2))

  def concat[A](l: ListADT[ListADT[A]]): ListADT[A] =
    foldRight(l, ListADT.empty[A])((value, result) => append(result, value))
}

case class ConsADT[A](value: A, tail: ListADT[A]) extends ListADT[A] {
  override def head = value
}

case object NilADT extends ListADT[Nothing] {
  override def head = throw new RuntimeException("NilMonad doesn't have a value")
  override def tail = throw new RuntimeException("NilMonad doesn't have tail")
}
