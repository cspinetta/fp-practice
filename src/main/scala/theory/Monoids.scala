package theory

object Monoids {

  // Monoid laws: associative and identity
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String) = a1 ++ a2
    override def zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]) = a1 ++ a2
    override def zero = List.empty[A]
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 + a2
    override def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 * a2
    override def zero = 1
  }

  val boolOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 || a2
    override def zero = false
  }

  val boolAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 && a2
    override def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
    override def zero = None
  }

  def endoFuncMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A) = a1 andThen a2
    override def zero = identity
  }

  def dualMonoid[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A) = m.op(a2, a1)
    override def zero = m.zero
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRightViaFoldMap[A, B](as: List[A])(zero: B)(f: (A, B) => B): B = {
    foldMap(as, endoFuncMonoid[B])(f.curried)(zero)
  }

  def foldLeftViaFoldMap[A, B](as: List[A], zero: B)(f: (B, A) => B): B = {
    foldMap(as, dualMonoid(endoFuncMonoid[B]))(a => b => f(b, a))(zero)
  }

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.size == 1) {
      f(v.head)
    } else {
      val (v1, v2) = v.splitAt(v.size / 2)
      val r1 = foldMapV(v1, m)(f)
      val r2 = foldMapV(v2, m)(f)
      m.op(r1, r2)
    }
  }

}
