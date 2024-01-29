package chord

import chord.typeclasses.*

type Choreo[M[_], A] = Free[[X] =>> ChoreoSig[M, X], A]

object Choreo:
  def pure[M[_], A](a: A): Choreo[M, A] = Free.pure(a)

extension [M[_], A](c: Choreo[M, A])
  def map[B](f: A => B): Choreo[M, B] =
    c.flatMap(a => Free.pure(f(a)))

  def flatMap[B](f: A => Choreo[M, B]): Choreo[M, B] =
    c match
      case Free.Pure(a) => f(a)
      case Free.FlatMap(c, g) =>
        Free.FlatMap(c, a => g(a).flatMap(f))

enum ChoreoSig[M[_], A]:
  case Local[M[_], A, L <: Location](l: L, m: Unwrap[L] => M[A])
      extends ChoreoSig[M, A @@ L]

  case Comm[M[_], A, L0 <: Location, L1 <: Location](l0: L0, a: A @@ L0, l1: L1)
      extends ChoreoSig[M, A @@ L1]

  case Cond[M[_], A, B, L <: Location](l: L, a: A @@ L, f: A => Choreo[M, B])
      extends ChoreoSig[M, B]

extension [L <: Location](l: L)
  def locally[M[_], A](m: Unwrap[l.type] => M[A]): Choreo[M, A @@ l.type] =
    Free.lift(ChoreoSig.Local(l, m))

  // def locally[M[_], A](m: Unwrap[l.type] ?=> M[A]): Choreo[M, A @@ l.type] =
  //   Free.lift(ChoreoSig.Local(l, un => m(using un)))

  def send[M[_], A, L1 <: Location](
      a: A @@ L,
      l1: L1
  ): Choreo[M, A @@ l1.type] =
    Free.lift(ChoreoSig.Comm(l, a, l1))

  def cond[M[_], A, B](a: A @@ L)(f: A => Choreo[M, B]): Choreo[M, B] =
    Free.lift(ChoreoSig.Cond(l, a, f))

extension [M[_], A](c: Choreo[M, A])
  def runLocal(using M: Monad[M]) = Free.eval(c) {
    [X] => (x: ChoreoSig[M, X]) => localHandler(x)
  }

private[chord] def localHandler[M[_], A](c: ChoreoSig[M, A])(using
    M: Monad[M]
): M[A] =
  c match
    case ChoreoSig.Local(l, m) =>
      m(unwrap).map(wrap[l.type](_))

    case ChoreoSig.Comm(l0, a, l1) =>
      M.pure(wrap(unwrap(a)))

    case ChoreoSig.Cond(l, a, f) =>
      f(unwrap(a)).runLocal
