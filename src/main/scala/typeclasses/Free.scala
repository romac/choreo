package chord.typeclasses

enum Free[F[_], A]:
  case Pure[F[_], A](a: A) extends Free[F, A]
  case FlatMap[F[_], B, A](fb: F[B], f: B => Free[F, A]) extends Free[F, A]

given [F[_]]: Monad[[A] =>> Free[F, A]] with
  import Free.{Pure, FlatMap}

  def pure[A](a: A): Free[F, A] = Pure(a)

  extension [A](fa: Free[F, A])
    def map[B](f: A => B): Free[F, B] =
      fa.flatMap(a => pure(f(a)))

    def zip[B](ff: Free[F, B]): Free[F, (A, B)] =
      fa.flatMap(a => ff.map(b => (a, b)))

    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      fa match
        case Pure(a)        => f(a)
        case FlatMap(fa, g) => FlatMap(fa, a => g(a).flatMap(f))

type ~>[-F[_], +G[_]] = [A] => F[A] => G[A]

object Free:
  def pure[F[_], A](a: A): Free[F, A] = Pure(a)

  def lift[F[_], A](fa: F[A]): Free[F, A] =
    Free.FlatMap(fa, Free.Pure(_))

  def eval[F[_], G[_], A](ffa: Free[F, A])(f: F ~> G)(using
      G: Monad[G]
  ): G[A] =
    ffa match
      case Free.Pure(a) =>
        G.pure(a)

      case Free.FlatMap(fb, g) =>
        G.flatMap(f(fb))(a => eval(g(a))(f))
