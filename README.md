[![Continuous Integration][ci-badge]][ci-link]

# Choreo

A library for choregraphic programming in Scala.

The implementation is based on the paper [_HasChor: Functional Choreographic Programming for All_][haschor-paper] by Gan Shen, Shun Kashiwa and Lindsey Kuper, 
and its [Haskell implementation][haschor-github].

## Example

```scala
package choreo
package examples
package kv

import cats.effect.IO
import cats.effect.IO.asyncForIO
import cats.effect.kernel.Ref
import cats.syntax.all.*

type State = Map[String, String]

enum Request:
  case Get(key: String)
  case Put(key: String, value: String)

type Response = Option[String]

val client: "client" = "client"
val server: "server" = "server"

def main: IO[Unit] =
  for
    backend <- Backend.local(List(client, server))
    clientTask = choreo.run(backend, client)
    serverTask = choreo.run(backend, server)
    _ <- (clientTask, serverTask).parTupled
  yield ()

def choreo: Choreo[IO, Unit] =
  for
    stateS <- server.locally(Ref.of[IO, State](Map.empty))
    _ <- step(stateS).foreverM
  yield ()

def step(stateS: Ref[IO, State] @@ "server"): Choreo[IO, Unit] =
  for
    reqC <- client.locally(readRequest)
    resC <- kvs(reqC, stateS)
    _ <- client.locally:
      resC.!.fold(IO.println("Key not found")):
        IO.print("> ") *> IO.println(_)
  yield ()

def kvs(
    reqC: Request @@ "client",
    stateS: Ref[IO, State] @@ "server"
): Choreo[IO, Response @@ "client"] =
  for
    reqS <- client.send(reqC).to(server)
    resS <- server.locally(handleRequest(reqS.!, stateS.!))
    resC <- server.send(resS).to(client)
  yield resC

def handleRequest(
    req: Request,
    state: Ref[IO, State]
): IO[Response] =
  req match
    case Request.Get(key) =>
      state.get.map(_.get(key))

    case Request.Put(key, value) =>
      state.update(_.updated(key, value)).as(Some(value))

def readRequest: IO[Request] =
  for
    _ <- IO.print("> ")
    line <- IO.readLine
    req <- line.split(" ") match
      case Array("GET", key) =>
        IO.pure(Request.Get(key))

      case Array("PUT", key, value) =>
        IO.pure(Request.Put(key, value))

      case _ =>
        IO.raiseError(new Exception("Invalid request"))
  yield req
```

## License

This library is released under the same license as HasChor, namely the BSD-3 license.

Please see the [`LICENSE`](./LICENSE) within this repository for the full text of the license.

[ci-badge]: https://github.com/romac/choreo/actions/workflows/ci.yml/badge.svg
[ci-link]: https://github.com/romac/choreo/actions/workflows/ci.yml
[haschor-paper]: https://dl.acm.org/doi/10.1145/3607849
[haschor-github]: https://github.com/gshen42/HasChor
