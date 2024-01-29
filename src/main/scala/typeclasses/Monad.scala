package chord.typeclasses

trait Monad[F[_]] extends Applicative[F]:
  extension [A](fa: F[A]) def flatMap[B](f: A => F[B]): F[B]

object Monad:
  def apply[F[_]](using F: Monad[F]): Monad[F] = F
