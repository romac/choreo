package chord.typeclasses

trait Monad[F[_]] extends Applicative[F]:
  extension [A](fa: F[A]) def flatMap[B](f: A => F[B]): F[B]

object Monad:
  def apply[F[_]](using F: Monad[F]): Monad[F] = F

extension [M[_], A](xs: List[M[A]])
  def sequence(using M: Monad[M]): M[List[A]] =
    xs.foldRight(M.pure(List.empty[A])) { (ma, mla) =>
      M.flatMap(ma) { a =>
        M.map(mla) { la =>
          a :: la
        }
      }
    }

  def sequence_[B](using M: Monad[M]): M[Unit] =
    xs.foldRight(M.pure(())) { (ma, mla) =>
      M.flatMap(ma) { _ =>
        mla
      }
    }
