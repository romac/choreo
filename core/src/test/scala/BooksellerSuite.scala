package choreo

import cats.syntax.all.*
import cats.effect.IO
import cats.effect.kernel.Ref
import munit.CatsEffectSuite

class BooksellerSuite extends CatsEffectSuite {

  val buyer: "buyer"   = "buyer"
  val seller: "seller" = "seller"

  case class Book(title: String, price: Double)
  case class Date(year: Int, month: Int, day: Int)

  val catalog = Map(
    "Types and Programming Languages" -> Book("Types and Programming Languages", 80.0),
    "Erta Ale"                        -> Book("Erta Ale", 9999.0)
  )

  def bookseller(
      title: String,
      budget: Double
  ): Choreo[IO, Option[Date @@ "buyer"]] =
    for
      titleB <- buyer.locally(IO.pure(title))
      titleS <- buyer.send(titleB).to(seller)

      priceS <- seller.locally:
                  IO.pure(catalog.get(titleS.!).map(_.price).getOrElse(0.0))

      priceB <- seller.send(priceS).to(buyer)

      decision <- buyer.locally:
                    IO.pure(priceB.! > 0 && priceB.! <= budget)

      result <- buyer.cond(decision):
                  case true =>
                    for
                      dateS <- seller.locally(IO.pure(Date(2026, 6, 15)))
                      dateB <- seller.send(dateS).to(buyer)
                    yield Some(dateB)

                  case false =>
                    Choreo.pure(None)
    yield result

  test("runLocal: buyer purchases affordable book") {
    bookseller("Types and Programming Languages", 100.0).runLocal.map { result =>
      val date = unwrap[buyer.type](result.get)
      assertEquals(date, Date(2026, 6, 15))
    }
  }

  test("runLocal: buyer rejects expensive book") {
    bookseller("Erta Ale", 50.0).runLocal.map { result =>
      assertEquals(result, None)
    }
  }

  test("runLocal: buyer rejects unknown book") {
    bookseller("Nonexistent", 1000.0).runLocal.map { result =>
      assertEquals(result, None)
    }
  }

  test("distributed: buyer purchases affordable book") {
    for
      backend  <- Backend.local[IO](List(buyer, seller))
      choreo    = bookseller("Types and Programming Languages", 100.0)
      sellerF  <- choreo.project(backend, seller).start
      buyerRes <- choreo.project(backend, buyer)
      _        <- sellerF.joinWithNever
    yield {
      val date = unwrap[buyer.type](buyerRes.get)
      assertEquals(date, Date(2026, 6, 15))
    }
  }

  test("distributed: buyer rejects expensive book") {
    for
      backend  <- Backend.local[IO](List(buyer, seller))
      choreo    = bookseller("Erta Ale", 50.0)
      sellerF  <- choreo.project(backend, seller).start
      buyerRes <- choreo.project(backend, buyer)
      _        <- sellerF.joinWithNever
    yield assertEquals(buyerRes, None)
  }

  test("distributed: buyer rejects unknown book") {
    for
      backend  <- Backend.local[IO](List(buyer, seller))
      choreo    = bookseller("Nonexistent", 1000.0)
      sellerF  <- choreo.project(backend, seller).start
      buyerRes <- choreo.project(backend, buyer)
      _        <- sellerF.joinWithNever
    yield assertEquals(buyerRes, None)
  }

  test("distributed: effects only run at the owning location") {
    for
      buyerLog  <- Ref.of[IO, List[String]](Nil)
      sellerLog <- Ref.of[IO, List[String]](Nil)

      choreo = for
        titleB <- buyer.locally:
                    buyerLog.update(_ :+ "buyer:title") *> IO.pure("Types and Programming Languages")
        titleS <- buyer.send(titleB).to(seller)
        priceS <- seller.locally:
                    sellerLog.update(_ :+ "seller:price") *> IO.pure(80.0)
        priceB <- seller.send(priceS).to(buyer)
        decision <- buyer.locally:
                      buyerLog.update(_ :+ "buyer:decision") *> IO.pure(true)
        result <- buyer.cond(decision):
                    case true =>
                      for
                        dateS <- seller.locally:
                                   sellerLog.update(_ :+ "seller:date") *> IO.pure(Date(2026, 1, 1))
                        dateB <- seller.send(dateS).to(buyer)
                      yield Some(dateB)
                    case false =>
                      Choreo.pure(None)
      yield result

      backend  <- Backend.local[IO](List(buyer, seller))
      sellerF  <- choreo.project(backend, seller).start
      _        <- choreo.project(backend, buyer)
      _        <- sellerF.joinWithNever

      bLog <- buyerLog.get
      sLog <- sellerLog.get
    yield {
      assertEquals(bLog, List("buyer:title", "buyer:decision"))
      assertEquals(sLog, List("seller:price", "seller:date"))
    }
  }

  test("distributed: multiple rounds of communication") {
    val pingPong: Choreo[IO, Int @@ "buyer"] =
      for
        a  <- buyer.locally(IO.pure(1))
        b  <- buyer.send(a).to(seller)
        c  <- seller.locally(IO.pure(b.! + 10))
        d  <- seller.send(c).to(buyer)
        e  <- buyer.locally(IO.pure(d.! + 100))
        f  <- buyer.send(e).to(seller)
        g  <- seller.locally(IO.pure(f.! + 1000))
        h  <- seller.send(g).to(buyer)
      yield h

    for
      backend  <- Backend.local[IO](List(buyer, seller))
      sellerF  <- pingPong.project(backend, seller).start
      buyerRes <- pingPong.project(backend, buyer)
      _        <- sellerF.joinWithNever
    yield assertEquals(unwrap[buyer.type](buyerRes), 1111)
  }
}
