package choreo
package backend

import cats.Monad
import cats.arrow.FunctionK
import cats.effect.std.Queue
import cats.effect.kernel.Concurrent
import cats.syntax.all.*

import fs2.io.net.{Network => FS2Network}
import fs2.io.net.Socket
import com.comcast.ip4s._

import choreo.utils.toFunctionK
import fs2.Chunk
import java.nio.ByteOrder.LITTLE_ENDIAN
import java.nio.ByteBuffer

type Peer = SocketAddress[IpAddress]
case class Client[M[_]](loc: Loc, peer: Peer, queue: Queue[M, Frame])

case class Frame(payload: Chunk[Byte])

object Frame:
  def read[M[_]](s: Socket[M])(using M: Monad[M]): M[Frame] =
    for
      sizeBytes <- s.readN(4)
      buffer = ByteBuffer.wrap(sizeBytes.toArray).order(LITTLE_ENDIAN)
      size = buffer.getInt
      payload <- s.readN(size)
    yield Frame(payload)

  def write[M[_]: Monad](s: Socket[M], frame: Frame): M[Unit] =
    val size = frame.payload.size
    val buffer = ByteBuffer.allocate(4).order(LITTLE_ENDIAN)
    val sizeBytes = buffer.putInt(size).array
    s.write(Chunk.array(sizeBytes)) >> s.write(frame.payload)

class TCPBackend[M[_]](peers: Map[Loc, Peer]):
  val locs = peers.keys.toSeq

  def makeClients(using Concurrent[M]): M[Seq[Client[M]]] =
    peers.toSeq.traverse: (loc, peer) =>
      Queue.unbounded[M, Frame].map(Client(loc, peer, _))

  def runNetwork[A](at: Loc)(network: Network[M, A])(using
      C: Concurrent[M],
      N: FS2Network[M]
  ): M[A] =
    for
      me <- C.pure(peers.get(at).get)
      clients <- makeClients
      server = FS2Network[M].server(port = Some(me.port))

      _ <- server
        .map { socket =>
          for
            frame <- Frame.read(socket)
            remoteAddr <- socket.remoteAddress
            client = clients.find(_.peer == remoteAddr).get
            _ <- client.queue.offer(frame)
          yield Stream.empty[Unit]
        }
        .parJoin(100)
        .compile
        .drain

      result <- network.foldMap(runAt(at).toFunctionK)
    yield result

  private[choreo] def runAt(loc: Loc)(using
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
            .filter(_ != loc)
            .traverse_ { to =>
              runAt(loc)(NetworkSig.Send(a, to, ser))
          }
end TCPBackend

object TCPBackend:
  given backend[M[_]: Concurrent: FS2Network]: Backend[TCPBackend[M], M] with
    extension (backend: TCPBackend[M])
      def runNetwork[A](at: Loc)(network: Network[M, A]): M[A] =
        runNetwork(at)(network)
end TCPBackend
