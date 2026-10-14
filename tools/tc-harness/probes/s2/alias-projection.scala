// a80f7ae6c0's mixin case, and an alias implementing an abstract member of a supertrait
trait T0 { type M }
trait T1 extends T0
trait T2 { type M = Any }
trait K extends T1 with T2
trait Graph { type Edge }
object G1 extends Graph { type Edge = (Int, Int) }
class G2 extends Graph { type Edge = String }
object AP {
  val k: K = ???
  val mixin: T1#M = (??? : k.M)
  val e1: G1.Edge = (1, 2)
  val viaObject: Graph#Edge = e1
  val g2 = new G2
  val e2: g2.Edge = "s"
  val viaVal: Graph#Edge = e2
  val viaClass: Graph#Edge = (??? : G2#Edge)
}
