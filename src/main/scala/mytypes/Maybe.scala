package mytypes

object Maybe {

  def fromOption[T](o: Option[T]): Maybe[T] = o match {
    case None => Nothingg
    case Some(value) => Just(value)
  }
}

trait Maybe[+A] {

  def value: A
  def get: A = value

  def getOrElse[B >: A](default: ⇒ B): B = if (isJust) get else default

  def isJust: Boolean
  def isNothingg: Boolean = !isJust
  def isDefined: Boolean = isJust
  def notDefined: Boolean = !isJust

  def length: Int = if (isNothingg) 0 else 1

  def foldRight[B](z: B)(op: (A, B) ⇒ B): B = if (isNothingg) z else op(value, z)
  def foldLeft[B](z: B)(op: (B, A) ⇒ B): B = if (isNothingg) z else op(z, value)
  def fold[B](ifEmpty: ⇒ B)(f: (A) ⇒ B): B = if (isNothingg) ifEmpty else f(value)

  def filter(p: (A) ⇒ Boolean): Maybe[A] = if (this.isDefined && p(value)) this else Nothingg
  def filterNot(p: (A) ⇒ Boolean): Maybe[A] = filter(!p(_))

  def map[B](f: (A) ⇒ B): Maybe[B] = if (isNothingg) Nothingg else Just(f(value))

  def flatMap[B](f: (A) ⇒ Maybe[B]): Maybe[B] = if (isNothingg) Nothingg else f(value)
  def flatten: Any =
    if (isNothingg) Nothingg
    else if (value.isInstanceOf[Maybe[Any]]) value
    else throw new IllegalStateException("Maybe cannot be flattened, it is not a Maybe of Maybes.")

  def contains[A1 >: A](elem: A1): Boolean = if (isNothingg) false else elem == get
  def exists(p: (A) ⇒ Boolean): Boolean = if (isNothingg) false else p(value)
  def forall(p: (A) ⇒ Boolean): Boolean = if (isNothingg) true else p(value)

  def foreach[U](f: (A) ⇒ U): Unit = if (isNothingg) () else {f(value); ()}

  def zipWith[B, C](that: Maybe[B])(f: (A, B) => C): Maybe[C] =
    if (this.isNothingg || that.isNothingg) Nothingg
    else Just(f(this.value, that.value))
  def zip[B](that: Maybe[B]): Maybe[(A, B)] = zipWith(that)((_, _))
  def zipWithIndex: Maybe[(A, Int)] = zip(Just(0))

  def toOption: Option[A] = if (isNothingg) None else Some(value)
}

case object Nothingg extends Maybe[Nothing] {

  override def value = throw new NoSuchElementException("get value of Nothingg")

  override def isJust: Boolean = false

  override def canEqual(that: Any): Boolean = this.getClass == that.getClass

  override def equals(that: Any): Boolean = canEqual(that)

  override def hashCode(): Int = super.hashCode()
}

final case class Just[+A](value: A) extends Maybe[A] {

  override def isJust: Boolean = true

  override def canEqual(that: Any): Boolean = this.getClass == that.getClass

  override def equals(that: Any): Boolean = canEqual(that) && this.value == that.asInstanceOf[Just[A]].value

  override def hashCode(): Int = super.hashCode()
}
