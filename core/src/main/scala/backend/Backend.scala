package choreo
package backend

import cats.Monad
import cats.effect.std.Queue
import cats.effect.kernel.Concurrent
import cats.syntax.all.*

trait Backend[B, M[_]]:
  extension (backend: B)
    def runNetwork[A](at: Loc)(network: Network[M, A]): M[A]

object Backend:
  def local[M[_]: Concurrent](locs: List[Loc]): M[LocalBackend[M]] =
    LocalBackend.make(locs)

  def http[M[_]: Concurrent](locs: List[Loc]): HTTPBackend[M] =
    HTTPBackend(locs)
