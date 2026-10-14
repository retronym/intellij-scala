// ca1722b5f7 / fdd8ed14ad: a val typed by an alias to a singleton
object A { object Inner { val v = 1 }; type T = Inner.type }
class C { val o = new Object; type OT = o.type }
object SAV {
  val a: A.T = A.Inner
  val v: Int = a.v
  val same: A.Inner.type = a
  val c = new C
  val x: c.OT = c.o
  val y: c.o.type = x
  val d = new C
  val bad: d.OT = c.o
  type Alias = A.Inner.type
  val z: Alias = a
  def f(p: A.T): p.v.type = p.v
}
