package choreo
package backend

import cats.Monad
import cats.arrow.FunctionK
import cats.syntax.all.*
import cats.effect.std.Queue
import cats.effect.kernel.Concurrent
import cats.effect.implicits.*

import fs2.io.net.{Network => FS2Network}
import fs2.io.net.Socket
import com.comcast.ip4s._

import choreo.utils.toFunctionK
import fs2.Chunk
import java.nio.ByteOrder.LITTLE_ENDIAN
import java.nio.ByteBuffer

type Peer = SocketAddress[IpAddress]
case class Client[M[_]](loc: Loc, peer: Peer, queue: Queue[M, Frame])

case class Frame(from: Loc, payload: Chunk[Byte])

object Frame:
  def readLoc[M[_]](s: Socket[M])(using M: Monad[M]): M[Loc] =
    for loc <- readPayload(s)
    yield new String(loc.toArray)

  def readPayload[M[_]](s: Socket[M])(using M: Monad[M]): M[Chunk[Byte]] =
    for
      sizeBytes <- s.readN(4)
      buffer = ByteBuffer.wrap(sizeBytes.toArray).order(LITTLE_ENDIAN)
      size = buffer.getInt
      payload <- s.readN(size)
    yield payload

  def read[M[_]](s: Socket[M])(using M: Monad[M]): M[Frame] =
    for
      from <- readLoc(s)
      payload <- readPayload(s)
    yield Frame(from, payload)

  def writePayload[M[_]: Monad](s: Socket[M], payload: Chunk[Byte]): M[Unit] =
    val size = payload.size
    val buffer = ByteBuffer.allocate(4).order(LITTLE_ENDIAN)
    val sizeBytes = buffer.putInt(size).array
    s.write(Chunk.array(sizeBytes)) >> s.write(payload)

  def writeLoc[M[_]: Monad](s: Socket[M], from: Loc): M[Unit] =
    writePayload(s, Chunk.array(from.getBytes))

  def write[M[_]: Monad](s: Socket[M], frame: Frame): M[Unit] =
    writeLoc(s, frame.from) >> writePayload(s, frame.payload)

class TCPBackend[M[_]](peers: Map[Loc, Peer]):
  val locs = peers.keys.toSeq

  def makeClients(using Concurrent[M]): M[Seq[Client[M]]] =
    peers.toSeq.traverse: (loc, peer) =>
      Queue.unbounded[M, Frame].map(Client(loc, peer, _))

  def runNetwork[A](at: Loc)(network: Network[M, A])(using
      C: Concurrent[M],
      N: FS2Network[M]
  ): M[A] =
    val me = peers(at)
    for
      clients <- makeClients
      fiber <- FS2Network[M]
        .server(port = Some(me.port))
        .parEvalMapUnordered(clients.size + 1) { socket =>
          for
            frame <- Frame.read(socket)
            remoteAddr <- socket.remoteAddress
            // _ = println(s"[$at] Received frame from $remoteAddr: $frame")
            client = clients.find(_.loc == frame.from).get
            _ <- client.queue.offer(frame)
          // _ = println(s"[$at] Offered frame to ${client.loc}: $frame")
          yield ()
        }
        .compile
        .drain
        .start
      result <- network.foldMap(run(at, clients).toFunctionK)
    yield result

  private[choreo] def run(at: Loc, clients: Seq[Client[M]])(using
      C: Concurrent[M],
      N: FS2Network[M]
  ): [A] => NetworkSig[M, A] => M[A] = [A] =>
    (na: NetworkSig[M, A]) =>
      na match
        case NetworkSig.Run(ma) =>
          ma

        case NetworkSig.Send(a, to, ser) =>
          val encoded = ser.encode(a)
          val chunk = Chunk.array[Byte](encoded)
          val client = clients.find(_.loc == to).get
          val socket = FS2Network[M].client(client.peer)
          val frame = Frame(at, chunk)
          // println(s"[$at] Sending frame to $to: $frame")
          socket.use: socket =>
            Frame.write(socket, frame)

        case NetworkSig.Recv(from, ser) =>
          val client = clients.find(_.loc == from).get
          // println(s"[$at] Waiting for message from $from")
          for
            frame <- client.queue.take
            // _ = println(s"[$at] Received frame from $from: $frame")
            value = ser.decode(frame.payload.toArray).get
          // _ = println(s"[$at] Decoded $value")
          yield value

        case NetworkSig.Broadcast(a, ser) =>
          // println(s"[$at] Broadcasting $a")
          locs
            .filter(_ != at)
            .traverse_ { to =>
              run(at, clients)(NetworkSig.Send(a, to, ser))
          }
end TCPBackend

object TCPBackend:
  given backend[M[_]: Concurrent: FS2Network]: Backend[TCPBackend[M], M] with
    extension (backend: TCPBackend[M])
      def runNetwork[A](at: Loc)(network: Network[M, A]): M[A] =
        runNetwork(at)(network)
end TCPBackend
