package choreo
package backend

import cats.Monad
import cats.arrow.FunctionK
import cats.effect.std.Queue
import cats.effect.kernel.Concurrent
import cats.syntax.all.*

import choreo.utils.toFunctionK

class LocalBackend[M[_]](inboxes: Map[Loc, Queue[M, Any]]):
  val locs = inboxes.keys.toSeq

  def runNetwork[A](at: Loc)(network: Network[M, A])(using M: Monad[M]): M[A] =
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
end LocalBackend

object LocalBackend:
  def make[M[_]: Concurrent](locs: List[Loc]): M[LocalBackend[M]] =
    makeInboxes(locs).map(LocalBackend(_))

  given backend[M[_]: Monad]: Backend[LocalBackend[M], M] with
    extension (backend: LocalBackend[M])
      def runNetwork[A](at: Loc)(network: Network[M, A]): M[A] =
        runNetwork(at)(network)
end LocalBackend

private[this] def makeInboxes[M[_]: Concurrent](
    locs: Seq[Loc]
): M[Map[Loc, Queue[M, Any]]] =
  for queues <- locs.traverse(_ => Queue.unbounded[M, Any])
  yield locs.zip(queues).toMap
