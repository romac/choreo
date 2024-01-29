package chord

import scala.annotation.targetName

enum At[A, Loc]:
  case Wrap(a: A) extends At[A, Loc]
  case Empty() extends At[A, Loc]

type @@[A, Loc] = At[A, Loc]

type Unwrap[Loc] = [A] => A @@ Loc => A

extension [A, Loc](a: A @@ Loc)
  @targetName("unwrap")
  def !(using U: Unwrap[Loc]): A = U(a)

def wrap[Loc]: [A] => A => A @@ Loc =
  [A] => (a: A) => At.Wrap(a)

def unwrap[Loc]: Unwrap[Loc] = [A] =>
  (a: A @@ Loc) =>
    a match
      case At.Wrap(a) => a
      case At.Empty() => scala.sys.error("Attempted to unwrap an Empty value")

extension [A](value: A)
  def at[Loc](loc: Loc): A @@ loc.type =
    wrap[loc.type](value)
