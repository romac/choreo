package choreo

import cats.syntax.all.*
import cats.effect.IO
import munit.CatsEffectSuite

class LocalSuite extends CatsEffectSuite {

  val alice: "alice" = "alice"
  val bob: "bob"     = "bob"
  val carol: "carol" = "carol"

  // -- runLocal semantics (single-location execution) --

  test("runLocal: locally returns wrapped value") {
    val c: Choreo[IO, Int @@ "alice"] =
      alice.locally(IO.pure(42))

    c.runLocal.map { result =>
      assertEquals(unwrap[alice.type](result), 42)
    }
  }

  test("runLocal: comm forwards value") {
    val c: Choreo[IO, Int @@ "bob"] =
      for
        a <- alice.locally(IO.pure(10))
        b <- alice.send(a).to(bob)
      yield b

    c.runLocal.map { result =>
      assertEquals(unwrap[bob.type](result), 10)
    }
  }

  test("runLocal: chained locally and comm") {
    val c: Choreo[IO, String @@ "bob"] =
      for
        a <- alice.locally(IO.pure("hello"))
        b <- alice.send(a).to(bob)
        c <- bob.locally(IO.pure(b.! + " world"))
        d <- bob.send(c).to(alice)
      yield c

    c.runLocal.map { result =>
      assertEquals(unwrap[bob.type](result), "hello world")
    }
  }

  // -- EPP correctness (projected endpoints produce correct results) --

  test("EPP: local operation projected at owning location runs the effect") {
    val c: Choreo[IO, Int @@ "alice"] =
      alice.locally(IO.pure(42))

    val network = Endpoint.project(c, alice)

    for
      backend <- Backend.local[IO](List(alice))
      result  <- backend.runNetwork(alice)(network)
    yield assertEquals(unwrap[alice.type](result), 42)
  }

  test("EPP: local operation projected at non-owning location returns Empty") {
    val c: Choreo[IO, Int @@ "alice"] =
      alice.locally(IO.pure(42))

    val network = Endpoint.project(c, bob)

    for
      backend <- Backend.local[IO](List(alice, bob))
      result  <- backend.runNetwork(bob)(network)
    yield assert(result.isInstanceOf[At.Empty[?, ?]])
  }

  test("EPP: comm projected at both endpoints transfers value") {
    val c: Choreo[IO, Int @@ "bob"] =
      for
        a <- alice.locally(IO.pure(99))
        b <- alice.send(a).to(bob)
      yield b

    for
      backend  <- Backend.local[IO](List(alice, bob))
      aliceNet  = Endpoint.project(c, alice)
      bobNet    = Endpoint.project(c, bob)
      fiber    <- backend.runNetwork(alice)(aliceNet).start
      result   <- backend.runNetwork(bob)(bobNet)
      _        <- fiber.joinWithNever
    yield assertEquals(unwrap[bob.type](result), 99)
  }

  // -- LocalBackend round-trip communication --

  test("LocalBackend: round-trip send and receive") {
    val c: Choreo[IO, String @@ "alice"] =
      for
        a <- alice.locally(IO.pure("ping"))
        b <- alice.send(a).to(bob)
        c <- bob.locally(IO.pure(b.! + "-pong"))
        d <- bob.send(c).to(alice)
      yield d

    for
      backend  <- Backend.local[IO](List(alice, bob))
      aliceFib <- c.project(backend, alice).start
      bobFib   <- c.project(backend, bob).start
      resultA  <- aliceFib.joinWithNever
      _        <- bobFib.joinWithNever
    yield assertEquals(unwrap[alice.type](resultA), "ping-pong")
  }

  test("LocalBackend: messages from different senders are routed correctly") {
    val c: Choreo[IO, (fromAlice: Int @@ "carol", fromBob: Int @@ "carol")] =
      for
        a <- alice.locally(IO.pure(1))
        b <- bob.locally(IO.pure(2))
        ca <- alice.send(a).to(carol)
        cb <- bob.send(b).to(carol)
      yield (fromAlice = ca, fromBob = cb)

    for
      backend  <- Backend.local[IO](List(alice, bob, carol))
      aliceFib <- c.project(backend, alice).start
      bobFib   <- c.project(backend, bob).start
      resultC  <- c.project(backend, carol)
      _        <- aliceFib.joinWithNever
      _        <- bobFib.joinWithNever
    yield {
      assertEquals(unwrap[carol.type](resultC.fromAlice), 1)
      assertEquals(unwrap[carol.type](resultC.fromBob), 2)
    }
  }

  // -- Cond branching --

  test("Cond: true branch is taken") {
    val c: Choreo[IO, String @@ "bob"] =
      for
        flag <- alice.locally(IO.pure(true))
        result <- alice.cond(flag) {
          case true  => bob.locally(IO.pure("yes"))
          case false => bob.locally(IO.pure("no"))
        }
      yield result

    for
      backend  <- Backend.local[IO](List(alice, bob))
      aliceFib <- c.project(backend, alice).start
      resultB  <- c.project(backend, bob)
      _        <- aliceFib.joinWithNever
    yield assertEquals(unwrap[bob.type](resultB), "yes")
  }

  test("Cond: false branch is taken") {
    val c: Choreo[IO, String @@ "bob"] =
      for
        flag <- alice.locally(IO.pure(false))
        result <- alice.cond(flag) {
          case true  => bob.locally(IO.pure("yes"))
          case false => bob.locally(IO.pure("no"))
        }
      yield result

    for
      backend  <- Backend.local[IO](List(alice, bob))
      aliceFib <- c.project(backend, alice).start
      resultB  <- c.project(backend, bob)
      _        <- aliceFib.joinWithNever
    yield assertEquals(unwrap[bob.type](resultB), "no")
  }

  test("Cond: runLocal takes the correct branch") {
    val c: Choreo[IO, String @@ "bob"] =
      for
        flag <- alice.locally(IO.pure(true))
        result <- alice.cond(flag) {
          case true  => bob.locally(IO.pure("yes"))
          case false => bob.locally(IO.pure("no"))
        }
      yield result

    c.runLocal.map { result =>
      assertEquals(unwrap[bob.type](result), "yes")
    }
  }

  // -- Error cases --

  test("unwrap Empty value throws an error") {
    val empty = At.empty[Int, "alice"]
    interceptMessage[RuntimeException]("Attempted to unwrap an Empty value") {
      unwrap[alice.type](empty)
    }
  }

  test("LocalBackend: missing channel throws an error") {
    val c: Choreo[IO, Int @@ "bob"] =
      for
        a <- alice.locally(IO.pure(42))
        b <- alice.send(a).to(bob)
      yield b

    for
      backend <- Backend.local[IO](List(alice))
      result  <- c.project(backend, alice).attempt
    yield assert(result.isLeft)
  }
}
