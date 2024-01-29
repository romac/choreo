package chord

type Location = String

enum At[A, +Loc <: Location]:
  case Wrap(a: A) extends At[A, Loc]
  case Empty() extends At[A, Loc]

type @@[A, +Loc <: Location] = At[A, Loc]

extension [A, Loc <: Location](a: A @@ Loc)
  def un(using unwrap: Unwrap[Loc]): A = unwrap(a)

def wrap[Loc <: Location]: [A] => A => A @@ Loc =
  [A] => (a: A) => At.Wrap(a)

def unwrap[L <: Location]: Unwrap[L] = [A] =>
  (a: A @@ L) =>
    a match
      case At.Wrap(a) => a
      case At.Empty() => ???

extension [A](value: A)
  def at[L <: Location](loc: L): A @@ loc.type =
    wrap[loc.type](value)

type Unwrap[L <: Location] = [A] => A @@ L => A
