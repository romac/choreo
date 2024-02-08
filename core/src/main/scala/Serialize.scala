package choreo

trait Serialize[A]:
  def encode(a: A): Array[Byte]
  def decode(encoded: Array[Byte]): Option[A]
end Serialize

object Serialize:
  import io.circe.{Encoder, Decoder}

  given [A](using encoder: Encoder[A], decoder: Decoder[A]): Serialize[A] with
    def encode(a: A): Array[Byte] =
      encoder(a).noSpaces.getBytes

    def decode(encoded: Array[Byte]): Option[A] =
      io.circe.parser
        .parse(new String(encoded))
        .toOption
        .flatMap(decoder.decodeJson(_).toOption)

end Serialize
