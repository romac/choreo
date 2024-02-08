package choreo
package examples
package kv

import cats.effect.IO
import cats.effect.IO.asyncForIO
import cats.effect.kernel.Ref
import cats.syntax.all.*

import choreo.backend.Backend
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.SocketAddress
import com.comcast.ip4s.Port

import io.circe.*
import io.circe.generic.semiauto.*

type State = Map[String, String]

enum Request:
  case Get(key: String)
  case Put(key: String, value: String)

object Request:
  given Encoder[Request] = deriveEncoder
  given Decoder[Request] = deriveDecoder

type Response = Option[String]

val client: "client" = "client"
val server: "server" = "server"

def main: IO[Unit] =
  val localhost = IpAddress.fromString("127.0.0.1").get
  val backend = Backend.tcp(
    Map(
      client -> SocketAddress(localhost, Port.fromInt(8088).get),
      server -> SocketAddress(localhost, Port.fromInt(8089).get)
    )
  )

  val clientTask = app.run(backend, client)
  val serverTask = app.run(backend, server)
  (clientTask, serverTask).parTupled.void

def app(using Serialize[Request], Serialize[Response]): Choreo[IO, Unit] =
  for
    stateS <- server.locally(Ref.of[IO, State](Map.empty))
    _ <- step(stateS).foreverM
  yield ()

def step(
    stateS: Ref[IO, State] @@ "server"
)(using Serialize[Request], Serialize[Response]): Choreo[IO, Unit] =
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
)(using
    Serialize[Request],
    Serialize[Response]
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
