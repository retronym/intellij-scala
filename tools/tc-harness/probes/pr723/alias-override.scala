trait HasT { type T = Int }
class Same extends HasT { override type T = Int }
class Diff extends HasT { override type T = String }
class DiffNoOverride extends HasT { type T = String }
trait HasL { type L = List[Int] }
class DiffArg extends HasL { override type L = List[String] }
class EquivAlias extends HasL { type I = Int; override type L = List[I] }
trait Abs { type A }
trait Concrete extends Abs { type A = Int }
class ReAbs extends Concrete { override type A = String }
trait HasP[X] { type P = X }
class Pd extends HasP[Int] { override type P = String }
class Ps extends HasP[Int] { override type P = Int }
