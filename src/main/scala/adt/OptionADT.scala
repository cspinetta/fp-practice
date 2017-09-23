package adt

sealed trait OptionADT[+T] {

  def isDefined: Boolean
  def get: T
}

object OptionADT {
  def apply[T](v: T): OptionADT[T] = if (v != null) Some(v) else None
}

case class Some[T](v: T) extends OptionADT[T] {

  override def isDefined: Boolean = true

  override def get: T = v
}

case object None extends OptionADT[Nothing] {

  override def isDefined: Boolean = false

  override def get: Nothing = throw new RuntimeException("None doesn't have a value")

}
