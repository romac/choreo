package choreo

import cats.Monad
import cats.free.Free
import cats.arrow.FunctionK
import cats.effect.std.Queue
import cats.effect.kernel.Concurrent
import cats.syntax.all.*

import choreo.utils.toFunctionK

trait Backend[B, M[_]]:
  extension (backend: B) def runNetwork[A](at: Loc)(network: Network[M, A]): M[A]

object Backend:
  def local[M[_]: Concurrent](
      locs: List[Loc]
  ): M[LocalBackend[M]] =
    for inboxes <- LocalBackend.makeInboxes(locs)
    yield LocalBackend(inboxes, locs)

type Channel = (from: Loc, to: Loc)

class LocalBackend[M[_]](inboxes: Map[Channel, Queue[M, Any]], val locs: Seq[Loc]):

  def runNetwork[A](at: Loc)(
      network: Network[M, A]
  )(using M: Monad[M]): M[A] =
    network.foldMap(run(at, inboxes).toFunctionK)

  private[choreo] def run(
      at: Loc,
      inboxes: Map[Channel, Queue[M, Any]]
  )(using M: Monad[M]): [A] => NetworkSig[M, A] => M[A] = [A] =>
    (na: NetworkSig[M, A]) =>
      na match
        case NetworkSig.Run(ma) =>
          ma

        case NetworkSig.Send(a, to) =>
          val inbox = inboxes((from = at, to = to))
          inbox.offer(a)

        case NetworkSig.Recv(from) =>
          val inbox = inboxes((from = from, to = at))
          inbox.take.map(_.asInstanceOf[A])

        case NetworkSig.Broadcast(a) =>
          locs
            .filter(_ != at)
            .traverse_ { to =>
              run(at, inboxes)(NetworkSig.Send(a, to))
          }

object LocalBackend:
  def makeInboxes[M[_]: Concurrent](
      locs: Seq[Loc]
  ): M[Map[Channel, Queue[M, Any]]] =
    val channels = for { s <- locs; r <- locs; if s != r } yield (from = s, to = r)
    for queues <- channels.traverse(_ => Queue.unbounded[M, Any])
    yield channels.zip(queues).toMap

  given localBackend[M[_]: Monad]: Backend[LocalBackend[M], M] with
    extension (b: LocalBackend[M])
      def runNetwork[A](at: Loc)(network: Network[M, A]): M[A] =
        b.runNetwork(at)(network)
