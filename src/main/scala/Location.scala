package choreo

import scala.annotation.targetName

type Loc = String

enum At[A, L <: Loc]:
  case Wrap(a: A) extends At[A, L]
  case Empty() extends At[A, L]

  type Value = A
  type Location = L

object At:
  def empty[A, L <: Loc]: A @@ L = Empty()
  def apply[L <: Loc]: [A] => A => A @@ L = [A] => (a: A) => Wrap(a)

type @@[A, L <: Loc] = At[A, L]

type Unwrap[L <: Loc] = [A] => A @@ L => A

extension [A, L <: Loc](a: A @@ L)
  @targetName("unwrap")
  def !(using U: Unwrap[L]): A = U(a)

private[choreo] def wrap[L <: Loc]: [A] => A => A @@ L =
  [A] => (a: A) => At.Wrap(a)

private[choreo] def unwrap[L <: Loc]: Unwrap[L] = [A] =>
  (a: A @@ L) =>
    a match
      case At.Wrap(a) => a
      case At.Empty() => scala.sys.error("Attempted to unwrap an Empty value")

extension [A](value: A)
  def at[L <: Loc](loc: L): A @@ loc.type =
    wrap[loc.type](value)
