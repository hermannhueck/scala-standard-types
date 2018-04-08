package mytypes

/**
  * CList is a Cons List that implements Monoid.
  *
  * It implements two Monoid functions:
  * - mempty = Nil: gives us the neutral element of addition
  * def mempty[B >: A]: CList[Nothing]
  * - mappend: appends/merges two CLists into one by adding them
  * def mappend[B >: A](other: Monoid[B]): Monoid[B]
  *
  * With the above functions you get <> and mconcat for free (is implemented in the trait Monoid):
  * def <>   ... is just an alias for mappend
  * def mconcat[B >: A](ms: List[Monoid[B]]): Monoid[B]
  *
  * concat (in the companion object) is just the same as mconcat with the types changed to CList[B] instead of Monoid[B]
  * def concat[A, B >: A](ms: List[CList[B]]): CList[B]
  */
object CList {

  def apply[A](as: A*): CList[A] =
    if (as.isEmpty)
      Nil
    else
      Cons(as.head, apply(as.tail: _*))

  def fromList[A](list: List[A]) = apply(list: _*)

  def fromSeq[A](seq: Seq[A]) = apply(seq: _*)

  def fromSet[A](set: Set[A]) = apply(set.toSeq: _*)

  def empty[A]: CList[A] = CList[A]()
}

sealed trait CList[+A] {

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  def head: A

  def tail: CList[A]

  def headOption: Option[A] = if (isEmpty) None else Some(head)

  def append[B >: A](that: CList[B]): CList[B] = this match { // list concatenation
    case Nil => that
    case Cons(h, t) => Cons(h, t append that)
  }

  def ++[B >: A](other: CList[B]): CList[B] = append(other) // ++ is an alias for append

  def foldRight[B](acc: B)(op: (A, B) ⇒ B): B = this match {
    case Nil => acc
    case Cons(h, t) => op(h, t.foldRight(acc)(op))
  }

  def foldLeft[B](acc: B)(op: (B, A) ⇒ B): B = this match {
    case Nil => acc
    case Cons(h, t) => op(t.foldLeft(acc)(op), h)
  }

  def fold[B >: A](zero: B)(op: (B, B) ⇒ B): B = foldRight(zero)(op)

  def reduceRight[B >: A](op: (A, B) ⇒ B): B = {

    def go(xs: CList[A], op: (A, B) ⇒ B, acc: B): B =
      if (xs.isEmpty) acc
      else op(xs.head, go(xs.tail, op, acc))

    if (this.isEmpty)
      throw new UnsupportedOperationException("operation not supported on empty CList")
    else
      go(tail, op, head)
  }

  def reduceLeft[B >: A](op: (B, A) ⇒ B): B = {

    def go(xs: CList[A], op: (B, A) ⇒ B, acc: B): B =
      if (xs.isEmpty) acc
      else op(go(xs.tail, op, acc), xs.head)

    if (this.isEmpty)
      throw new UnsupportedOperationException("operation not supported on empty CList")
    else
      go(tail, op, head)
  }

  def reduce[B >: A](op: (B, B) ⇒ B): B = reduceRight(op)

  def length: Int = foldRight(0)((_, acc) => 1 + acc)

  def size: Int = length

  def isDefinedAt(index: Int): Boolean = index >= 0 && index < length

  def contains[B >: A](elem: B): Boolean = exists(_ == elem)

  def exists(p: (A) ⇒ Boolean): Boolean = foldRight(false)((elem, found) => if (found) true else p(elem))

  def forall(p: (A) ⇒ Boolean): Boolean = !exists(p(_) == false)

  def reverse: CList[A] = {
    def reverse(clist: CList[A], acc: CList[A]): CList[A] = clist match {
      case Nil => acc
      case Cons(head, tail) => reverse(tail, Cons(head, acc))
    }

    reverse(this, Nil)
  }

  def isPalindrom: Boolean = this.reverse == this

  // recursive impl: ... = if (p(head)) Cons(head, tail.filter(p)) else tail.filter(p)
  def filter(p: A => Boolean): CList[A] =
    foldRight(CList[A]())((elem, acc) => if (p(elem)) Cons(elem, acc) else acc)

  def filterNot(p: A => Boolean): CList[A] = filter(!p(_))

  // recursive impl:  ... =  Cons(f(head), tail.map(f)) // recursive
  def map[B](f: A => B): CList[B] =
    foldRight(CList[B]())((elem, acc) => Cons(f(elem), acc))

  // recursive impl:  ... = f(head) ++ tail.flatMap(f)
  def flatMap[B](f: A => CList[B]): CList[B] =
    foldRight(CList[B]())((elem, acc) => f(elem) ++ acc)

  // bind is an alias for flatMap
  def bind[B](f: A => CList[B]): CList[B] = flatMap(f)

  def flattenOld: CList[Any] = { // flattens a CList of CLists, but looses type information
    val id = (elem: Any) => elem.asInstanceOf[CList[A]] // id with a cast!!! AWFUL!!!
    this match {
      case Nil =>
        Nil
      case Cons(h, _) if h.isInstanceOf[CList[Any]] =>
        flatMap(id) // recursive: h ++ tail.flatten
      case _ => // compiles, but throws Exception at runtime!!! AWFUL!!!
        throw new IllegalStateException("CList cannot be flattened, it is not a CList of CLists.")
    }
  }

  def flatten[E](implicit ev: A <:< CList[E]): CList[E] = { // flattens a CList of CLists and retains type information
    this match { // no casts necessary in this implementation
      case Nil => CList.empty[E]
      case Cons(_, _) => flatMap(identity(_)) // identity ~= elem => elem
    }
  }

  def concatOld: CList[Any] = flattenOld
  def concat[E](implicit ev: A <:< CList[E]): CList[E] = flatten

  def takeWhile(p: A => Boolean): CList[A] = this match {
    case Nil => Nil
    case Cons(h, _) if !p(h) => Nil
    case Cons(h, t) => Cons(h, t.takeWhile(p))
  }

  def dropWhile(p: A => Boolean): CList[A] = this match {
    case Nil => Nil
    case Cons(h, t) if !p(h) => Cons(h, t)
    case Cons(_, t) => t.dropWhile(p)
  }

  def take(n: Int): CList[A] = this match {
    case Nil => Nil
    case Cons(h, t) => if (n <= 0) Nil else Cons(h, t.take(n - 1))
  }

  def drop(n: Int): CList[A] = this match {
    case Nil => Nil
    case Cons(_, t) => if (n <= 0) this else t.drop(n - 1)
  }

  def toList: List[A] = foldRight(List[A]())(_ :: _)

  def toSeq: Seq[A] = toList

  // you may loose the order of the elements when going to a Set
  def toSet[B >: A]: Set[B] = toList.toSet

  def foreach(f: A ⇒ Unit): Unit = reverse.map(f)

  def zipWith[B, C](that: CList[B])(f: (A, B) => C): CList[C] = (this, that) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case _ => Cons(f(this.head, that.head), this.tail.zipWith(that.tail)(f))
  }

  def zip[B](that: CList[B]): CList[(A, B)] = zipWith(that)((_, _))

  def zipWithIndex: CList[(A, Int)] = {
    def go(clist: CList[A], index: Int): CList[(A, Int)] = clist match {
      case Nil => Nil
      case Cons(head, tail) => Cons((head, index), go(tail, index + 1))
    }

    go(this, 0)
  }

  def sum[B >: A](implicit num: Numeric[B]): B = fold(num.zero)(num.plus)

  def avg[B >: A](implicit num: Numeric[B]): Double = num.toDouble(fold(num.zero)(num.plus)) / length

  def min[B >: A](implicit ord: Ordering[B]): B = reduce((x, y) => if (ord.lteq(x, y)) x else y)

  def max[B >: A](implicit ord: Ordering[B]): B = reduce((x, y) => if (ord.gteq(x, y)) x else y)
}

case object Nil extends CList[Nothing] {

  override def isEmpty: Boolean = true

  override def head = throw new NoSuchElementException("head of empty CList")

  override def tail = throw new NoSuchElementException("tail of empty CList")
}

final case class Cons[+A](head: A, tail: CList[A]) extends CList[A] {

  override def isEmpty: Boolean = false
}
