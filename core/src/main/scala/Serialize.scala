package choreo

trait Serialize[A]:
  type Encoding
  def encode(a: A): Encoding
  def decode(encoded: Encoding): Option[A]

object Serialize:
  def identity[A] = new Serialize[A]:
    type Encoding = A
    def encode(a: A) = a
    def decode(encoded: Encoding) = Some(encoded)

  object identities:
    given [A]: Serialize[A] = identity[A]
