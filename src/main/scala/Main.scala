package chord

import cats.effect.IO
import cats.syntax.all.*
import cats.effect.unsafe.implicits.global

@main
def main: Unit =
  chord.examples.bookseller.app.unsafeRunSync()
