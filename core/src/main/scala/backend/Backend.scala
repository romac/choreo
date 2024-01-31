package choreo
package backend

import cats.Monad
import cats.effect.std.Queue
import cats.effect.kernel.Concurrent
import cats.syntax.all.*

import fs2.io.net.{Network => FS2Network}

trait Backend[B, M[_]]:
  extension (backend: B)
    def runNetwork[A](at: Loc)(network: Network[M, A]): M[A]

object Backend:
  def local[M[_]: Concurrent](locs: List[Loc]): M[LocalBackend[M]] =
    LocalBackend.make(locs)

  def tcp[M[_]: Concurrent: FS2Network](peers: Map[Loc, Peer]): TCPBackend[M] =
    TCPBackend(peers)
