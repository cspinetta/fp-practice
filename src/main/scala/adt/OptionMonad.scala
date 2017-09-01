package adt

sealed trait OptionMonad[+T] {

  def isDefined: Boolean
  def get: T
}

object OptionMonad {
  def apply[T](v: T): OptionMonad[T] = if (v != null) Some(v) else None
}

case class Some[T](v: T) extends OptionMonad[T] {

  override def isDefined: Boolean = true

  override def get: T = v
}

case object None extends OptionMonad[Nothing] {

  override def isDefined: Boolean = false

  override def get: Nothing = throw new RuntimeException("None doesn't have a value")

}
