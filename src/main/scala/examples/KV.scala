package chord
package examples
package kv

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all.*

type State = Map[String, String]

enum Request:
  case Get(key: String)
  case Put(key: String, value: String)

type Response = Option[String]

val client: "client" = "client"
val server: "server" = "server"

def app: IO[Unit] =
  for
    backend <- Backend.local[IO](List(client, server))
    clientTask = choreo.run(backend, client)
    serverTask = choreo.run(backend, server)
    _ <- (clientTask, serverTask).parTupled
  yield ()

def choreo: Choreo[IO, Unit] =
  for
    stateS <- server.locally(Ref.of[IO, State](Map.empty))
    _ <- step(stateS).forever
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

// Request are either:
//    GET key
//    PUT key value
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
