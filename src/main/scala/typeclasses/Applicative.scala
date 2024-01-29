package chord.typeclasses

import scala.annotation.targetName

trait Applicative[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]

  extension [A](fa: F[A])
    def zip[B](ff: F[B]): F[(A, B)]

    @targetName("voidLeft")
    def *>[B](fb: F[B]): F[B] =
      fa.zip(fb).map(_._2)
