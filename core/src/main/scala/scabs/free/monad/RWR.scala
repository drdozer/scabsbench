package scabs
package free
package monad

import cats._
import cats.implicits._
import scabs.free.Constraint.{FreeConstraint1, FreeMonad}
import scabs.seq.Sequence

import scala.annotation.tailrec

sealed trait RWR[S[_], F[_], A] {

  def flatMap[B](f: A => RWR[S, F, B])(implicit S: Sequence[S]): RWR[S, F, B]

}

object RWR {

  final def foldMap[S[_], F[_], G[_], A](fv: RWR[S, F, A])(trans: F ~> G)(implicit G: Monad[G], S: Sequence[S]): G[A] = {
    val result: G[Any] =
      G.tailRecM[RWR[S, F, Any], Any](fv.asInstanceOf[RWR[S, F, Any]]) {
        case RWR.Pure(a) =>
          G.pure(Right[RWR[S, F, Any], Any](a.asInstanceOf[Any]))
        case RWR.Seq(nv, s) =>
          G.map(trans(nv))(b => Left[RWR[S, F, Any], Any](RWR.run(b, s)))
      }
    result.asInstanceOf[G[A]]
  }

  final def retract[S[_], F[_], A](fv: RWR[S, F, A])(implicit F: Monad[F], S: Sequence[S]): F[A] = {
    val result: F[Any] =
      F.tailRecM[RWR[S, F, Any], Any](fv.asInstanceOf[RWR[S, F, Any]]) {
        case RWR.Pure(a) =>
          F.pure(Right[RWR[S, F, Any], Any](a.asInstanceOf[Any]))
        case RWR.Seq(nv, s) =>
          F.map(nv)(b => Left[RWR[S, F, Any], Any](RWR.run(b, s)))
      }
    result.asInstanceOf[F[A]]
  }

  @tailrec
  final def run[S[_], F[_]](v: Any, nexts: S[Any => RWR[S, F, Any]])(implicit S: Sequence[S]): RWR[S, F, Any] = S.uncons(nexts) match {
    case None => RWR.Pure(v)
    case Some((head, tail)) =>
      head(v) match {
        case RWR.Pure(a) => run(a, tail)
        case RWR.Seq(nv, s) => RWR.Seq[S, F, Any](nv, S.concat(s, tail))
      }
  }

  type Curried[S[_], F[_]] = {type l[A] = RWR[S, F, A]}

  implicit def rwrFreeMonad[S[_], F[_]](implicit S: Sequence[S]): FreeMonad[F, Curried[S, F]#l] =
    new FreeConstraint1[Monad, F, Curried[S, F]#l] {
      val generated: Monad[Curried[S, F]#l] = new Monad[Curried[S, F]#l] {
        def pure[A](a: A): RWR[S, F, A] = RWR.Pure(a)

        override def map[A, B](fa: RWR[S, F, A])(f: (A) => B): RWR[S, F, B] =
          flatMap(fa)(f.andThen(RWR.Pure[S, F, B]))

        def flatMap[A, B](fa: RWR[S, F, A])(f: (A) => RWR[S, F, B]): RWR[S, F, B] =
          fa.flatMap(f)

        override def flatten[A](ffa: RWR[S, F, RWR[S, F, A]]): RWR[S, F, A] =
          flatMap(ffa)(identity)

        def tailRecM[A, B](a: A)(f: (A) => RWR[S, F, Either[A, B]]): RWR[S, F, B] =
          flatMap(f(a)) {
            case Right(b)   => pure(b)
            case Left(next) => tailRecM(next)(f)
          }
      }

      def foldMap[A, G[_]](fv: RWR[S, F, A])(trans: ~>[F, G])(implicit ev: Monad[G]): G[A] =
        RWR.foldMap(fv)(trans)

      def retract[A](fv: RWR[S, F, A])(implicit ev: Monad[F]): F[A] =
        RWR.retract(fv)

      def lift[A](a: F[A]): RWR[S, F, A] =
        Seq[S, F, A](a.asInstanceOf[F[Any]], S.empty)
    }

  case class Pure[S[_], F[_], A](a: A) extends RWR[S, F, A] {
    def flatMap[B](f: A => RWR[S, F, B])(implicit S: Sequence[S]): RWR[S, F, B] =
      f(a)
  }

  case class Seq[S[_], F[_], A](value: F[Any], continuations: S[Any => RWR[S, F, Any]]) extends RWR[S, F, A] {
    def flatMap[B](f: A => RWR[S, F, B])(implicit S: Sequence[S]): RWR[S, F, B] =
      RWR.Seq[S, F, B](value, S.snoc(continuations, f.asInstanceOf[Any => RWR[S, F, Any]]))
  }

}

