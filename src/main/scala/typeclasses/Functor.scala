package chord.typeclasses

trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]
