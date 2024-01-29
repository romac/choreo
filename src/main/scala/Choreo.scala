package chord

import cats.Monad
import cats.free.Free
import cats.effect.IO
import cats.effect.kernel.Concurrent
import cats.syntax.all.*
import cats.arrow.FunctionK

import chord.utils.toFunctionK

type Choreo[M[_], A] = Free[[X] =>> ChoreoSig[M, X], A]

extension [M[_], A](c: Choreo[M, A])
  def run[B](backend: B, at: Loc)(using B: Backend[B, M]): M[A] =
    backend.runNetwork(at)(Endpoint.project(c, at))

object Choreo:
  def pure[M[_], A](a: A): Choreo[M, A] = Free.pure(a)

enum ChoreoSig[M[_], A]:
  case Local[M[_], A, L <: Loc](l: L, m: Unwrap[L] => M[A])
      extends ChoreoSig[M, A @@ L]

  case Comm[M[_], A, L0 <: Loc, L1 <: Loc](l0: L0, a: A @@ L0, l1: L1)
      extends ChoreoSig[M, A @@ L1]

  case Cond[M[_], A, B, L <: Loc](l: L, a: A @@ L, f: A => Choreo[M, B])
      extends ChoreoSig[M, B]

extension [L <: Loc](l: L)
  def locally[M[_], A](m: Unwrap[l.type] ?=> M[A]): Choreo[M, A @@ l.type] =
    Free.liftF(ChoreoSig.Local[M, A, l.type](l, un => m(using un)))

  def send[A](a: A @@ L): Sendable[A, L] = (l, a)

  def cond[M[_], A, B](a: A @@ L)(f: A => Choreo[M, B]): Choreo[M, B] =
    Free.liftF(ChoreoSig.Cond(l, a, f))

opaque type Sendable[A, L <: Loc] = (L, A @@ L)

extension [A, Src <: Loc](s: Sendable[A, Src])
  def to[M[_], Dst <: Loc](dst: Dst): Choreo[M, A @@ dst.type] =
    Free.liftF(ChoreoSig.Comm(s._1, s._2, dst))

extension [M[_], A](c: Choreo[M, A])
  def runLocal(using M: Monad[M]): M[A] =
    c.foldMap(localHandler.toFunctionK)

  private[chord] def localHandler(using
      M: Monad[M]
  ): [A] => ChoreoSig[M, A] => M[A] = [A] =>
    (c: ChoreoSig[M, A]) =>
      c match
        case ChoreoSig.Local(l, m) =>
          m(unwrap).map(wrap(_).asInstanceOf)

        case ChoreoSig.Comm(l0, a, l1) =>
          M.pure(wrap(unwrap(a)).asInstanceOf)

        case ChoreoSig.Cond(l, a, f) =>
          f(unwrap(a)).runLocal
