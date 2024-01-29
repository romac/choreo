package chord

import chord.typeclasses.*

enum IO[+A]:
  case Suspend(thunk: () => A)
  case FlatMap[A, B](io: IO[B], f: B => IO[A]) extends IO[A]

extension [A](io: IO[A])
  def unsafePerform(): A =
    io match
      case IO.Suspend(thunk) => thunk()
      case IO.FlatMap(io, f) =>
        val a = io.unsafePerform()
        f(a).unsafePerform()

given Monad[IO] with
  def pure[A](a: A): IO[A] =
    IO.Suspend(() => a)

  def suspend[A](thunk: => A): IO[A] =
    IO.Suspend(() => thunk)

  extension [A](fa: IO[A])
    def map[B](f: A => B): IO[B] =
      fa.flatMap(a => pure(f(a)))

    def zip[B](ff: IO[B]): IO[(A, B)] =
      fa.flatMap(a => ff.map(b => (a, b)))

    def flatMap[B](f: A => IO[B]): IO[B] =
      IO.FlatMap(fa, f)

object Console:
  def readLine(): IO[String] =
    IO.Suspend(() => scala.io.StdIn.readLine())

  def print(s: String): IO[Unit] =
    IO.Suspend(() => scala.Console.print(s))

  def println(s: String): IO[Unit] =
    IO.Suspend(() => scala.Console.println(s))

case class Date(year: Int, month: Int, day: Int):
  override def toString(): String = s"$year-$month-$day"

val buyer: "buyer" = "buyer"
val seller: "sender" = "sender"

val bookseller: Choreo[IO, Option[Date @@ "buyer"]] = for {
  titleB <- buyer.locally[IO, String] { _ =>
    Console.print("Enter book title: ") *> Console.readLine()
  }
  titleS <- buyer.send(titleB).to(seller)

  priceS <- seller.locally[IO, Double] { _ =>
    Monad[IO].pure(42.0)
  }
  priceB <- seller.send(priceS).to(buyer)

  decision <- buyer.locally { un =>
    Console.println(s"Price of ${un(titleB)} is ${un(priceB)}") *>
      Console.print("Do you want to buy it? [y/n] ") *>
      Console.readLine().map(_.trim == "y")
  }

  _ <- buyer.cond(decision) { decision =>
    if (decision) {
      for {
        deliveryDateS <- seller.locally { _ =>
          Monad[IO].pure(Date(2021, 1, 1))
        }
        deliveryDateB <- seller.send(deliveryDateS).to(buyer)

        _ <- buyer.locally { un =>
          Console.println(
            s"Book will be delivered on ${un(deliveryDateB)}"
          )
        }
      } yield Some(deliveryDateB)
    } else {
      for {
        _ <- buyer.locally { _ =>
          Console.println("Ok, bye!")
        }
      } yield None
    }
  }

} yield None

@main
def main: Unit =
  bookseller.runLocal.unsafePerform()
