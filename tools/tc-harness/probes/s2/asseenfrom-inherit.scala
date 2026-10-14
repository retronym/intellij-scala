trait Container[E] { type Elem = E; def get: Elem; def all: List[Elem] = List(get) }
class IntC extends Container[Int] { def get = 1 }
trait Nested[E] { trait In { def e: E }; def in: In }
class StrN extends Nested[String] { def in = new In { def e = "s" } }
object AS {
  val c = new IntC
  val g: Int = c.get
  val l: List[Int] = c.all
  val e: c.Elem = 2
  val n = new StrN
  val s: String = n.in.e
  val ni: n.In = n.in
}
