// 903e53ca1f / b3d1e57b10: var over def pair, alias overrides
trait HasX { def x: Int; def x_=(i: Int): Unit }
class VarX extends HasX { var x: Int = 0 }
trait HasT { type T = Int }
class OverT extends HasT { override type T = Int }
trait AbsT { type T }
class ImplT extends AbsT { type T = String }
class BadOver extends HasT { override type T = String }
trait Gt { def g: Int }
class Va extends Gt { val g = 1 }
class Vr extends Gt { var g = 2 }
