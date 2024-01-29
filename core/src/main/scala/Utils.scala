package choreo
package utils

import cats.arrow.FunctionK

extension [F[_], G[_]](f: [A] => F[A] => G[A])
  def toFunctionK: FunctionK[F, G] = new FunctionK[F, G] {
    def apply[A](fa: F[A]): G[A] = f(fa)
  }
