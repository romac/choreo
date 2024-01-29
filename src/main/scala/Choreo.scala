package chord

import chord.typeclasses.{given, *}

type Choreo[M[_], A] = Free[[X] =>> ChoreoSig[M, X], A]

object Choreo:
  def pure[M[_], A](a: A): Choreo[M, A] = Free.pure(a)

enum ChoreoSig[M[_], A]:
  case Local[M[_], A, L](l: L, m: Unwrap[L] => M[A])
      extends ChoreoSig[M, A @@ L]

  case Comm[M[_], A, L0, L1](l0: L0, a: A @@ L0, l1: L1)
      extends ChoreoSig[M, A @@ L1]

  case Cond[M[_], A, B, L](l: L, a: A @@ L, f: A => Choreo[M, B])
      extends ChoreoSig[M, B]

extension [L](l: L)
  def locally[M[_], A](m: Unwrap[l.type] ?=> M[A]): Choreo[M, A @@ l.type] =
    Free.lift(ChoreoSig.Local[M, A, l.type](l, un => m(using un)))

  def send[A](a: A @@ L): Sendable[A, L] = (l, a)

  def cond[M[_], A, B](a: A @@ L)(f: A => Choreo[M, B]): Choreo[M, B] =
    Free.lift(ChoreoSig.Cond(l, a, f))

opaque type Sendable[A, L] = (L, A @@ L)

extension [A, Src](s: Sendable[A, Src])
  def to[M[_], Dest](dest: Dest): Choreo[M, A @@ dest.type] =
    Free.lift(ChoreoSig.Comm(s._1, s._2, dest))

extension [M[_], A](c: Choreo[M, A])
  def runLocal(using M: Monad[M]) = Free.eval(c) {
    [X] => (x: ChoreoSig[M, X]) => localHandler(x)
  }

private[chord] def localHandler[M[_], A](c: ChoreoSig[M, A])(using
    M: Monad[M]
): M[A] =
  c match
    case ChoreoSig.Local(l, m) =>
      m(unwrap).map(wrap(_).asInstanceOf)

    case ChoreoSig.Comm(l0, a, l1) =>
      M.pure(wrap(unwrap(a)).asInstanceOf)

    case ChoreoSig.Cond(l, a, f) =>
      f(unwrap(a)).runLocal
