package chord

import cats.Monad
import cats.free.Free
import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.all.*
import cats.arrow.FunctionK

import chord.utils.toFunctionK

trait Backend:
  def runNetwork[A](at: Loc)(network: Network[IO, A]): IO[A]

object Backend:
  def local(locs: List[Loc]): IO[LocalBackend] =
    for inboxes <- LocalBackend.makeInboxes(locs)
    yield LocalBackend(inboxes)

class LocalBackend(inboxes: Map[Loc, Queue[IO, Any]]) extends Backend:
  val locs = inboxes.keys.toSeq

  def runNetwork[A](at: Loc)(
      network: Network[IO, A]
  ): IO[A] =
    network.foldMap(runLocal(at, inboxes).toFunctionK)

  def runLocal(
      at: Loc,
      inboxes: Map[Loc, Queue[IO, Any]]
  ): [A] => NetworkSig[IO, A] => IO[A] = [A] =>
    (na: NetworkSig[IO, A]) =>
      na match
        case NetworkSig.Run(ma) =>
          ma

        case NetworkSig.Send(a, to) =>
          val inbox = inboxes.get(to).get
          inbox.offer(a)

        case NetworkSig.Recv(from) =>
          val inbox = inboxes.get(at).get
          inbox.take.map(_.asInstanceOf[A])

        case NetworkSig.Broadcast(a) =>
          locs
            .filter(_ != at)
            .traverse_ { to =>
              runLocal(at, inboxes)(NetworkSig.Send(a, to))
          }

object LocalBackend:
  def makeInboxes(locs: Seq[Loc]): IO[Map[Loc, Queue[IO, Any]]] =
    for queues <- locs.traverse(_ => Queue.unbounded[IO, Any])
    yield locs.zip(queues).toMap
