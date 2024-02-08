package choreo
package examples
package bookseller

import cats.effect.IO
import cats.effect.IO.asyncForIO
import cats.syntax.all.*

import choreo.backend.Backend
import com.comcast.ip4s.SocketAddress
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.Port

case class Book(title: String, price: Double)

case class Date(year: Int, month: Int, day: Int):
  override def toString(): String = s"$year-$month-$day"

given Serialize[Date] with
  def encode(a: Date): Array[Byte] =
    a.toString.getBytes

  def decode(encoded: Array[Byte]): Option[Date] =
    String(encoded).split("-") match
      case Array(year, month, day) =>
        Some(Date(year.toInt, month.toInt, day.toInt))

      case _ =>
        None

val books = List(
  Book("Functional Programming in Scala", 121.0),
  Book("Scala with Cats", 42.0)
)

val buyer: "buyer" = "buyer"
val seller: "sender" = "sender"

def main: IO[Unit] =
  val localhost = IpAddress.fromString("127.0.0.1").get
  val backend = Backend.tcp(
    Map(
      buyer -> SocketAddress(localhost, Port.fromInt(8088).get),
      seller -> SocketAddress(localhost, Port.fromInt(8089).get)
    )
  )

  val sellerIO = protocol.run(backend, seller)
  val buyerIO = protocol.run(backend, buyer)

  (sellerIO, buyerIO).parTupled.void

def protocol(using
    Serialize[Boolean],
    Serialize[String],
    Serialize[Double],
    Serialize[Date]
): Choreo[IO, Option[Date @@ "buyer"]] =
  for
    titleB <- buyer.locally:
      IO.print("Enter book title: ") *> IO.readLine

    titleS <- buyer.send(titleB).to(seller)

    priceS <- seller.locally:
      for
        book <- IO.pure(books.find(_.title == titleS.!))
        price <- book match
          case Some(b) => IO.pure(b.price)
          case None    => IO.raiseError(new Exception("Book not found"))
      yield price

    priceB <- seller.send(priceS).to(buyer)

    decision <- buyer.locally:
      IO.println(s"Price of ${titleB.!} is ${priceB.!}") *>
        IO.print("Do you want to buy it? [y/n] ") *>
        IO.readLine.map(_ == "y")

    deliveryDate <- buyer.cond(decision):
      case true =>
        for
          deliveryDateS <- seller.locally(IO.pure(Date(2024, 12, 24)))
          deliveryDateB <- seller.send(deliveryDateS).to(buyer)

          _ <- buyer.locally:
            IO.println(s"Book will be delivered on ${deliveryDateB.!}")
        yield Some(deliveryDateB)

      case false =>
        buyer.locally(IO.println("Ok, bye!")) *> Choreo.pure(None)
  yield deliveryDate
