package scabs.seq

/**
  *
  *
  * @author Matthew Pocock
  */
sealed trait CellQ[T] {
  def isEmpty: Boolean
  def +: (a: T): CellQ[T]
  def :+ (a: T): CellQ[T]
  def ++ (rhs: CellQ[T]): CellQ[T]
  def map[U](f: T => U): CellQ[U]
  def foreach[U](f: T => U): Unit
  def uncons: Option[(T, CellQ[T])]
  def unsnoc: Option[(CellQ[T], T)]
}

object CellQ {

  implicit val cellQSequence = new Sequence[CellQ] {
    override def empty[A]: CellQ[A] = CellQ.empty

    override def isEmpty[A](q: CellQ[A]): Boolean = q.isEmpty

    override def head[A](queue: CellQ[A]): A = uncons(queue).get._1

    override def last[A](queue: CellQ[A]): A = unsnoc(queue).get._2

    override def init[A](queue: CellQ[A]): CellQ[A] = unsnoc(queue).get._1

    override def tail[A](queue: CellQ[A]): CellQ[A] = {
//      println(s"tail: $queue")
      uncons(queue).get._2
    }

    override def cons[A](x: A, q: CellQ[A]): CellQ[A] = x +: q

    override def snoc[A](q: CellQ[A], y: A): CellQ[A] = {
//      println(s"snoc: $q")
      q :+ y
    }

    override def lengthSeq[A](q: CellQ[A]): Int = {
      var s: Int = 0
      foreach(q)(_ => s = s + 1)
      s
    }

    override def fold[A, B](q: CellQ[A])(z: B)(f: (B, A) => B): B = {
      var b = z
      foreach(q)(a => b = f(b, a))
      b
    }

    override def toList[A](q: CellQ[A]): List[A] = {
      val builder = List.newBuilder[A]
      foreach(q)(builder += _)
      builder.result()
    }

    override def toSeq[A](xs: List[A]): CellQ[A] = {
      var s: CellQ[A] = empty
      xs.foreach(x => s = s :+ x)
      s
    }

    override def uncons[A](s: CellQ[A]): Option[(A, CellQ[A])] = s.uncons

    override def unsnoc[A](s: CellQ[A]): Option[(CellQ[A], A)] = s.unsnoc

    override def map[A, B](q: CellQ[A])(f: (A) => B): CellQ[B] = q.map(f)

    override def foreach[A, U](q: CellQ[A])(f: (A) => U): Unit = q.foreach(f)

    override def concat[A](fst: CellQ[A],
                           snd: CellQ[A]): CellQ[A] = fst ++ snd
  }

  private val emptySingleton = Empty[Nothing]()
  def empty[T]: Empty[T] = emptySingleton.asInstanceOf[Empty[T]]

  final case class Empty[T]() extends CellQ[T] {

    override def isEmpty: Boolean = true

    override def +:(a: T): Singleton[T] = Singleton(a)

    override def :+(a: T): Singleton[T] = Singleton(a)

    override def ++(rhs: CellQ[T]): CellQ[T] = rhs

    override def map[U](f: (T) => U): Empty[U] = empty

    override def foreach[U](f: (T) => U): Unit = {}

    override def uncons: None.type = None

    override def unsnoc: None.type = None
  }

  sealed trait NonEmpty[T] extends CellQ[T] {

    override def isEmpty: Boolean = false

    override def +:(a: T): NonEmpty[T]
    override def :+(a: T): NonEmpty[T]
    override def ++(rhs: CellQ[T]): NonEmpty[T]
    override def map[U](f: (T) => U): NonEmpty[U]
    override def uncons: Some[(T, CellQ[T])] = Some(unconsUnsafe)
    override def unsnoc: Some[(CellQ[T], T)] = Some(unsnocUnsafe)
    def unconsUnsafe: (T, CellQ[T])
    def unsnocUnsafe: (CellQ[T], T)
  }


  final case class Singleton[T](get: T) extends NonEmpty[T] {
    override def +:(a: T): ConsSnoc[T] = ConsSnoc(a, get)

    override def :+(a: T): ConsSnoc[T] = ConsSnoc(get, a)

    override def ++(rhs: CellQ[T]): NonEmpty[T] = rhs match {
      case Empty() => this
      case Singleton(g) => ConsSnoc(get, g)
      case ConsSnoc(c, s) => ConsCarrySnoc(get, Singleton(c), s)
      case ConsCarrySnoc(c, a, s) => ConsCarrySnoc(get, c +: a, s)
      case ConsAppendSnoc(c, l, r, s) => ConsAppendSnoc(get, c +: l, r, s)
    }

    override def map[U](f: (T) => U): Singleton[U] = Singleton(f(get))

    override def foreach[U](f: (T) => U): Unit = f(get)

    override def unconsUnsafe: (T, Empty[T]) = (get, empty)

    override def unsnocUnsafe: (CellQ[T], T) = (empty, get)
  }

  final case class ConsSnoc[T](cons: T, snoc: T) extends NonEmpty[T] {
    override def +:(a: T): ConsCarrySnoc[T] = ConsCarrySnoc(a, Singleton(cons), snoc)

    override def :+(a: T): ConsCarrySnoc[T] = ConsCarrySnoc(cons, Singleton(snoc), a)

    override def ++(rhs: CellQ[T]): NonEmpty[T] = rhs match {
      case Empty() => this
      case Singleton(g) => ConsCarrySnoc(cons, Singleton(snoc), g)
      case ConsSnoc(c, s) => ConsCarrySnoc(cons, ConsSnoc(snoc, c), s)
      case ConsCarrySnoc(c, a, s) => ConsAppendSnoc(cons, Singleton(snoc),  c +: a, s)
      case ConsAppendSnoc(c, l, r, s) => ConsAppendSnoc(cons, snoc +: c +: l, r, s)
    }

    override def map[U](f: (T) => U): ConsSnoc[U] = ConsSnoc(f(cons), f(snoc))

    override def foreach[U](f: (T) => U): Unit = {
      f(cons)
      f(snoc)
    }

    override def unconsUnsafe: (T, CellQ[T]) = (cons, Singleton(snoc))

    override def unsnocUnsafe: (CellQ[T], T) = (Singleton(cons), snoc)
  }

  final case class ConsCarrySnoc[T](cons: T, carry: NonEmpty[T], snoc: T) extends NonEmpty[T] {
    override def +:(a: T): ConsAppendSnoc[T] = ConsAppendSnoc(a, Singleton(cons), carry, snoc)

    override def :+(a: T): ConsAppendSnoc[T] = ConsAppendSnoc(cons, carry, Singleton(snoc), a)

    override def ++(rhs: CellQ[T]): NonEmpty[T] = rhs match {
      case Empty() => this
      case Singleton(g) => ConsAppendSnoc(cons, carry, Singleton(snoc), g)
      case ConsSnoc(c, s) => ConsAppendSnoc(cons, carry :+ snoc, Singleton(c), s)
      case ConsCarrySnoc(c, a, s) => ConsAppendSnoc(cons, carry :+ snoc, c +: a, s)
      case ConsAppendSnoc(c, l, r, s) => ConsAppendSnoc(cons, carry :+ snoc, (c +: l) ++ r, s)
    }

    override def map[U](f: (T) => U): ConsCarrySnoc[U] = ConsCarrySnoc(f(cons), carry map f, f(snoc))

    override def foreach[U](f: (T) => U): Unit = {
      f(cons)
      carry foreach f
      f(snoc)
    }

    override def unconsUnsafe: (T, CellQ[T]) = (cons, carry :+ snoc)

    override def unsnocUnsafe: (CellQ[T], T) = (cons +: carry, snoc)
  }

  final case class ConsAppendSnoc[T](cons: T, fst: NonEmpty[T], snd: NonEmpty[T], snoc: T) extends NonEmpty[T] {
    override def +:(a: T): ConsAppendSnoc[T] = ConsAppendSnoc(a, cons +: fst, snd, snoc)

    override def :+(a: T): ConsAppendSnoc[T] = snd match {
      case cas@ConsAppendSnoc(_, _, _, _) => ConsAppendSnoc(cons, fst ++ cas, Singleton(snoc), a)
      case _ => ConsAppendSnoc(cons, fst, snd :+ snoc, a)
    }

    override def ++(rhs: CellQ[T]): NonEmpty[T] = rhs match {
      case Empty() => this
      case Singleton(g) => ConsAppendSnoc(cons, fst, snd :+ snoc, g)
      case ConsSnoc(c, s) => ConsAppendSnoc(cons, fst ++ snd, ConsSnoc(snoc, c), s)
      case ConsCarrySnoc(c, a, s) => ConsAppendSnoc(cons, fst ++ snd :+ snoc, c +: a, s)
      case ConsAppendSnoc(c, l, r, s) => ConsAppendSnoc(cons, fst ++ snd :+ snoc, (c +: l) ++ r, s)
    }

    override def map[U](f: (T) => U): ConsAppendSnoc[U] = ConsAppendSnoc(f(cons), fst map f, snd map f, f(snoc))

    override def foreach[U](f: (T) => U): Unit = {
      f(cons)
      fst foreach f
      snd foreach f
      f(snoc)
    }

    override def unconsUnsafe: (T, CellQ[T]) = (cons, fst.unconsUnsafe match {
      case (c, Empty()) => ConsCarrySnoc(c, snd, snoc)
      case (c, f : NonEmpty[T]) => ConsAppendSnoc(c, f, snd, snoc)
    })

    override def unsnocUnsafe: (CellQ[T], T) = (snd.unsnocUnsafe match {
      case (Empty(), s) => ConsCarrySnoc(cons, fst, s)
      case (r : NonEmpty[T], s) => ConsAppendSnoc(cons, fst, r, s)
    }, snoc)
  }

}

