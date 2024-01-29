package chord

import chord.typeclasses.{given, *}

trait Backend:
  def runNetwork[A](at: Loc)(network: Network[IO, A]): IO[A]

object Backend:
  def local(locs: List[Loc]): LocalBackend =
    LocalBackend(locs)

class LocalBackend(locs: List[Loc]) extends Backend:
  var inboxes: Map[Loc, MQueue[Any]] =
    locs.map(loc => (loc, MQueue.empty)).toMap

  def runNetwork[A](at: Loc)(
      network: Network[IO, A]
  ): IO[A] = Free.eval(network) {
    [X] => (x: NetworkSig[IO, X]) => runLocal(x, at)
  }

  def runLocal[A](na: NetworkSig[IO, A], at: Loc): IO[A] =
    na match
      case NetworkSig.Run(ma) =>
        ma

      case NetworkSig.Send(a, to) =>
        val inbox = inboxes.get(to).get
        inbox.write(a)

      case NetworkSig.Recv(from) =>
        val inbox = inboxes.get(at).get
        inbox.read.map(_.asInstanceOf[A])

      case NetworkSig.Broadcast(a) =>
        locs
          .filter(_ != at)
          .map { to =>
            runLocal(NetworkSig.Send(a, to), at)
          }
          .sequence_

import java.util.concurrent.LinkedTransferQueue

class MQueue[A](queue: LinkedTransferQueue[A]):
  def read: IO[A] = IO.suspend(queue.take())
  def write(a: A): IO[Unit] = IO.suspend(queue.put(a))

object MQueue:
  def empty[A]: MQueue[A] = new MQueue(new LinkedTransferQueue())
