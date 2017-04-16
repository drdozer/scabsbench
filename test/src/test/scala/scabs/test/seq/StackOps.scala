package scabs.test.seq

import cats.laws.discipline.arbitrary
import cats.~>
import org.scalacheck.{Arbitrary, Gen, Shrink}
import scabs.seq.Sequence

/**
  *
  *
  * @author Matthew Pocock
  */
sealed trait StackOps[T]

object StackOps {
  case class Empty[T]() extends StackOps[T]
  final case class Cons[T](head: T, ops: StackOps[T]) extends StackOps[T]
  final case class Snoc[T](last: T, ops: StackOps[T]) extends StackOps[T]
  final case class Append[T](lhs: StackOps[T], rhs: StackOps[T]) extends StackOps[T]

  implicit def genEmpty[T]: Gen[Empty[T]] = Gen.const(Empty())

  implicit def genCons[T](implicit t: Arbitrary[T]): Gen[StackOps[T]] = Gen.sized { size =>
    if(size <= 0) Empty[T]()
    else for {
      head <- t.arbitrary
      g = Gen.resize(size - 1, genOps[T])
      ops <- g
    } yield Cons(head, ops)
  }

  implicit def genSnoc[T](implicit t: Arbitrary[T]): Gen[StackOps[T]] = Gen.sized { size =>
    if(size <= 0) Empty[T]()
    else for {
      last <- t.arbitrary
      g = Gen.resize(size - 1, genOps[T])
      ops <- g
    } yield Snoc(last, ops)
  }

  implicit def genAppend[T](implicit t: Arbitrary[T]): Gen[StackOps[T]] = Gen.sized { size =>
    if (size <= 0) Empty[T]()
    else for {
      ls <- Gen.chooseNum(0, size-1)
      rs = size - ls
      lhs <- Gen.resize(ls, genOps[T])
      rhs <- Gen.resize(rs, genOps[T])
    } yield Append(lhs, rhs)
  }

  implicit def genOps[T](implicit t: Arbitrary[T]): Gen[StackOps[T]] =
    Gen.oneOf(genCons[T], genSnoc[T], genAppend[T])

  implicit def shrinkOps[T]: Shrink[StackOps[T]] = Shrink[StackOps[T]] {
    case Empty() => Stream()
    case Cons(_, ops) => Stream(Empty(), ops)
    case Snoc(_, ops) => Stream(Empty(), ops)
    case Append(Empty(), Empty()) => Stream(Empty())
    case Append(Empty(), rhs) => Stream(Empty(), rhs)
    case Append(lhs, Empty()) => Stream(Empty(), lhs)
    case Append(lhs, rhs) =>
      val here = Stream(Empty[T](), lhs, rhs)
      val rec = for {
        l <- shrinkOps.shrink(lhs)
        r <- shrinkOps.shrink(rhs)
      } yield Append(l, r)

      here ++ rec
  }

  object taglessFinal {
    trait StackTF[Rep[_]] {
      def empty[T]: Rep[T]
      def cons[T](t: T, ops: Rep[T]): Rep[T]
      def snoc[T](t: T, ops: Rep[T]): Rep[T]
      def append[T](lhs: Rep[T], rhs: Rep[T]): Rep[T]
    }

    implicit def sequenceStackTF[Q[_]](implicit q: Sequence[Q]) = new StackTF[Q] {
      override def empty[T]: Q[T] = q.empty

      override def cons[T](t: T, ops: Q[T]): Q[T] = q.cons(t, ops)

      override def snoc[T](t: T, ops: Q[T]): Q[T] = q.snoc(ops, t)

      override def append[T](lhs: Q[T], rhs: Q[T]): Q[T] = q.concat(lhs, rhs)
    }

    def replay[Rep[_], T](ops: StackOps[T])(implicit s: StackTF[Rep]): Rep[T] = ops match {
      case StackOps.Empty() => s.empty
      case StackOps.Cons(h, o) => s.cons(h, replay(o))
      case StackOps.Snoc(h, o) => s.snoc(h, replay(o))
      case StackOps.Append(l, r) => s.append(replay(l), replay(r))
    }
  }
}
