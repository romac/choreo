package choreo
package examples

import cats.effect.IO
import cats.syntax.all.*
import cats.effect.unsafe.implicits.global

val allExamples = Map(
  "book" -> bookseller.main,
  "kv" -> kv.main
)

def choose: IO[Unit] =
  for
    _ <- IO.println("Available examples:")
    _ <- allExamples.keys.toSeq.traverse(e => IO.println(s" - $e"))

    _ <- IO.print("Run example: ")
    example <- IO.readLine.map(_.trim)

    _ <- allExamples.get(example) match
      case Some(app) => app
      case None => IO.println(s"Example $example not found") *> IO.println("")
  yield ()

@main
def main: Unit =
  choose.foreverM.unsafeRunSync()
