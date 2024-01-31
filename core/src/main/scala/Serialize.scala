package choreo

trait Serialize[A]:
  // type Encoding
  def encode(a: A): Array[Byte]
  def decode(encoded: Array[Byte]): Option[A]

object Serialize:
  def identity[A] = new Serialize[A]:
    // type Encoding = A
    def encode(a: A) = ???
    def decode(encoded: Array[Byte]) = ???

  object identities:
    given [A]: Serialize[A] = identity[A]
