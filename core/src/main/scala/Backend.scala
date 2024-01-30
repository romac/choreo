package choreo

import cats.Monad
import cats.free.Free
import cats.arrow.FunctionK
import cats.effect.std.Queue
import cats.effect.kernel.Concurrent
import cats.syntax.all.*

import choreo.utils.toFunctionK

trait Backend[B, M[_]]:
  extension (backend: B)
    def runNetwork[A](at: Loc)(network: Network[M, A]): M[A]

object Backend:
  def local[M[_]: Concurrent](
      locs: List[Loc]
  ): M[LocalBackend[M]] =
    for inboxes <- LocalBackend.makeInboxes(locs)
    yield LocalBackend(inboxes)

  def http[M[_]: Concurrent](
      locs: List[Loc]
  ): HTTPBackend[M] =
    HTTPBackend(locs)

class LocalBackend[M[_]](inboxes: Map[Loc, Queue[M, Any]]):
  val locs = inboxes.keys.toSeq

  def runNetwork[A](at: Loc)(
      network: Network[M, A]
  )(using M: Monad[M]): M[A] =
    network.foldMap(run(at, inboxes).toFunctionK)

  private[choreo] def run(
      at: Loc,
      inboxes: Map[Loc, Queue[M, Any]]
  )(using M: Monad[M]): [A] => NetworkSig[M, A] => M[A] = [A] =>
    (na: NetworkSig[M, A]) =>
      na match
        case NetworkSig.Run(ma) =>
          ma

        case NetworkSig.Send(a, to, ser) =>
          val inbox = inboxes.get(to).get
          inbox.offer(a)

        case NetworkSig.Recv(from, ser) =>
          val inbox = inboxes.get(at).get
          inbox.take.map(_.asInstanceOf[A])

        case NetworkSig.Broadcast(a, ser) =>
          locs
            .filter(_ != at)
            .traverse_ { to =>
              run(at, inboxes)(NetworkSig.Send(a, to, ser))
          }

object LocalBackend:
  def makeInboxes[M[_]: Concurrent](
      locs: Seq[Loc]
  ): M[Map[Loc, Queue[M, Any]]] =
    for queues <- locs.traverse(_ => Queue.unbounded[M, Any])
    yield locs.zip(queues).toMap

  given backend[M[_]: Monad]: Backend[LocalBackend[M], M] with
    extension (backend: LocalBackend[M])
      def runNetwork[A](at: Loc)(network: Network[M, A]): M[A] =
        runNetwork(at)(network)

class HTTPBackend[M[_]](locs: List[Loc]):
  def runNetwork[A](at: Loc)(
      network: Network[M, A]
  )(using M: Monad[M]): M[A] =
    network.foldMap(run(at, locs).toFunctionK)

  private[choreo] def run(
      at: Loc,
      locs: List[Loc]
  )(using M: Monad[M]): [A] => NetworkSig[M, A] => M[A] = [A] =>
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

object HTTPBackend:
  given backend[M[_]: Monad]: Backend[HTTPBackend[M], M] with
    extension (backend: HTTPBackend[M])
      def runNetwork[A](at: Loc)(network: Network[M, A]): M[A] =
        runNetwork(at)(network)
