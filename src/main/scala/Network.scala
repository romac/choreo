package chord

import cats.Monad
import cats.free.Free
import cats.effect.IO
import cats.syntax.all.*
import cats.arrow.FunctionK

import chord.utils.toFunctionK

enum NetworkSig[M[_], A]:
  case Run(ma: M[A]) extends NetworkSig[M, A]
  case Send(a: A, to: Loc) extends NetworkSig[M, Unit]
  case Recv(from: Loc) extends NetworkSig[M, A]
  case Broadcast(a: A) extends NetworkSig[M, Unit]

type Network[M[_], A] = Free[[X] =>> NetworkSig[M, X], A]

object Network:
  def pure[M[_], A](a: A): Network[M, A] =
    Free.pure(a)

  def run[M[_], A](ma: M[A]): Network[M, A] =
    Free.liftF(NetworkSig.Run(ma))

  def send[M[_], A](a: A, to: Loc): Network[M, Unit] =
    Free.liftF(NetworkSig.Send(a, to))

  def recv[M[_], A](from: Loc): Network[M, A] =
    Free.liftF(NetworkSig.Recv(from))

  def broadcast[M[_], A](a: A): Network[M, Unit] =
    Free.liftF(NetworkSig.Broadcast(a))

  def empty[M[_], A, L <: Loc]: Network[M, A @@ L] =
    Network.pure(At.empty[A, L])

object Endpoint:
  def project[M[_], A](c: Choreo[M, A], at: Loc): Network[M, A] =
    c.foldMap(epp[M](at).toFunctionK)

  private[chord] def epp[M[_]](
      at: Loc
  ): [A] => ChoreoSig[M, A] => Network[M, A] = [A] =>
    (c: ChoreoSig[M, A]) =>
      c match
        case ChoreoSig.Local(loc, m) =>
          if at == loc then Network.run(m(unwrap)).map(wrap.asInstanceOf)
          else Network.empty.asInstanceOf

        case ChoreoSig.Comm(src, a, dst) =>
          if at == src then
            Network.send(unwrap(a), dst) *> Network.empty.asInstanceOf
          else if at == dst then Network.recv(src).map(wrap.asInstanceOf)
          else Network.empty[M, a.Value, a.Location]

        case ChoreoSig.Cond(loc, a, f) =>
          if at == loc then
            Network.broadcast(unwrap(a)) *> project(f(unwrap(a)), at)
          else Network.recv(loc).flatMap(a => project(f(a.asInstanceOf), at))
