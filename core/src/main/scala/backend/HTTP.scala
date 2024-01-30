package choreo
package backend

import cats.Monad
import cats.arrow.FunctionK
import cats.effect.kernel.Concurrent
import cats.syntax.all.*

import choreo.utils.toFunctionK

class HTTPBackend[M[_]](locs: List[Loc]):
  def runNetwork[A](at: Loc)(network: Network[M, A])(using M: Monad[M]): M[A] =
    network.foldMap(run(at, locs).toFunctionK)

  private[choreo] def run(at: Loc, locs: List[Loc])(using
      M: Monad[M]
  ): [A] => NetworkSig[M, A] => M[A] = [A] =>
    (na: NetworkSig[M, A]) =>
      na match
        case NetworkSig.Run(ma) =>
          ma

        case NetworkSig.Send(a, to, ser) =>
          val encoded = ser.encode(a)
          // TODO: send to network
          M.pure(())

        case NetworkSig.Recv(from, ser) =>
          val encoded: ser.Encoding = ??? // TODO: receive from network
          val value = ser.decode(encoded).get
          M.pure(value)

        case NetworkSig.Broadcast(a, ser) =>
          locs
            .filter(_ != at)
            .traverse_ { to =>
              run(at, locs)(NetworkSig.Send(a, to, ser))
          }
end HTTPBackend

object HTTPBackend:
  given backend[M[_]: Monad]: Backend[HTTPBackend[M], M] with
    extension (backend: HTTPBackend[M])
      def runNetwork[A](at: Loc)(network: Network[M, A]): M[A] =
        runNetwork(at)(network)
end HTTPBackend
