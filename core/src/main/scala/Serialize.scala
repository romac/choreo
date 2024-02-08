package choreo

trait Serialize[A]:
  def encode(a: A): Array[Byte]
  def decode(encoded: Array[Byte]): Option[A]

object Serialize:
  given Serialize[Boolean] with
    def encode(a: Boolean): Array[Byte] =
      a.toString.getBytes

    def decode(encoded: Array[Byte]): Option[Boolean] =
      String(encoded).toBooleanOption

  given Serialize[Int] with
    def encode(a: Int): Array[Byte] =
      a.toString.getBytes

    def decode(encoded: Array[Byte]): Option[Int] =
      String(encoded).toIntOption

  given Serialize[Double] with
    def encode(a: Double): Array[Byte] =
      a.toString.getBytes

    def decode(encoded: Array[Byte]): Option[Double] =
      String(encoded).toDoubleOption

  given Serialize[String] with
    def encode(a: String): Array[Byte] =
      a.getBytes

    def decode(encoded: Array[Byte]): Option[String] =
      Some(String(encoded))

  given [A](using s: Serialize[A]): Serialize[Option[A]] with
    def encode(a: Option[A]): Array[Byte] =
      a match
        case Some(a) => s.encode(a)
        case None    => Array.emptyByteArray

    def decode(encoded: Array[Byte]): Option[Option[A]] =
      if encoded.isEmpty then Some(None)
      else s.decode(encoded).map(Some(_))
