import chord.*
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
    IO.Suspend(() => print(s))

case class Book(
    title: String,
    price: Double
)

case class Date(year: Int, month: Int, day: Int)

class LocalSuite extends munit.FunSuite {
  val buyer: "buyer" = "buyer"
  val seller: "sender" = "sender"

  import chord.typeclasses.*

  val bookseller: Choreo[IO, Option[Date @@ "buyer"]] = for {
    title <- buyer.locally[IO, String] { _ =>
      Console.print("Enter book title: ") *> Console.readLine()
    }
  } yield None

  test("local bookseller") {
    bookseller.runLocal.unsafePerform()
  }
}
