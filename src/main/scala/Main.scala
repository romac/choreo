package chord

import chord.typeclasses.{given, *}

enum IO[+A]:
  case Suspend(thunk: () => A)
  case FlatMap[A, B](io: IO[B], f: B => IO[A]) extends IO[A]

object IO:
  def pure[A](a: A): IO[A] =
    IO.Suspend(() => a)

  def suspend[A](thunk: => A): IO[A] =
    IO.Suspend(() => thunk)

extension [A](io: IO[A])
  def unsafePerform(): A =
    io match
      case IO.Suspend(thunk) => thunk()
      case IO.FlatMap(io, f) =>
        val a = io.unsafePerform()
        f(a).unsafePerform()

given Monad[IO] with
  def pure[A](a: A): IO[A] =
    IO.pure(a)

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

val bookseller = for {
  titleB <- buyer.locally {
    Console.print("Enter book title: ") *> Console.readLine()
  }
  titleS <- buyer.send(titleB).to(seller)

  priceS <- seller.locally(IO.pure(42.0))
  priceB <- seller.send(priceS).to(buyer)

  decision <- buyer.locally {
    Console.println(s"Price of ${titleB.!} is ${priceB.!}") *>
      Console.print("Do you want to buy it? [y/n] ") *>
      Console.readLine().map(_ == "y")
  }

  deliveryDate <- buyer.cond(decision) {
    case true =>
      for {
        deliveryDateS <- seller.locally(IO.pure(Date(2021, 1, 1)))
        deliveryDateB <- seller.send(deliveryDateS).to(buyer)

        _ <- buyer.locally {
          Console.println(s"Book will be delivered on ${deliveryDateB.!}")
        }
      } yield Some(deliveryDateB)

    case false =>
      buyer.locally(Console.println("Ok, bye!")) *> Choreo.pure(None)
  }

} yield deliveryDate

import scala.concurrent.{Future, ExecutionContext}

@main
def main: Unit =
  val ec = ExecutionContext.global

  val backend = Backend.local(List(buyer, seller))

  ec.execute { () =>
    println(
      bookseller.runIO(backend, seller).unsafePerform()
    )
  }

  println(
    bookseller.runIO(backend, buyer).unsafePerform()
  )
