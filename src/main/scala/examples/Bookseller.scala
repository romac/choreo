package chord
package examples
package bookseller

import cats.effect.IO
import cats.syntax.all.*

case class Date(year: Int, month: Int, day: Int):
  override def toString(): String = s"$year-$month-$day"

val buyer: "buyer" = "buyer"
val seller: "sender" = "sender"

val protocol = for {
  titleB <- buyer.locally {
    IO.print("Enter book title: ") *> IO.readLine
  }
  titleS <- buyer.send(titleB).to(seller)

  priceS <- seller.locally(IO.pure(42.0))
  priceB <- seller.send(priceS).to(buyer)

  decision <- buyer.locally {
    IO.println(s"Price of ${titleB.!} is ${priceB.!}") *>
      IO.print("Do you want to buy it? [y/n] ") *>
      IO.readLine.map(_ == "y")
  }

  deliveryDate <- buyer.cond(decision) {
    case true =>
      for {
        deliveryDateS <- seller.locally(IO.pure(Date(2021, 1, 1)))
        deliveryDateB <- seller.send(deliveryDateS).to(buyer)

        _ <- buyer.locally {
          IO.println(s"Book will be delivered on ${deliveryDateB.!}")
        }
      } yield Some(deliveryDateB)

    case false =>
      buyer.locally(IO.println("Ok, bye!")) *> Choreo.pure(None)
  }

} yield deliveryDate

import cats.effect.IO.asyncForIO

val app: IO[Unit] = for {
  backend <- Backend.local(List(buyer, seller))

  fiberSeller <- protocol.run(backend, seller).start
  fiberBuyer <- protocol.run(backend, buyer).start

  _ <- fiberSeller.join
  _ <- fiberBuyer.join
} yield ()
