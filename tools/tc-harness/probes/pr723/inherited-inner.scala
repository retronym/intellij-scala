class A { class In { def a: A.this.type = A.this }; def mk = new In; type Me = this.type }
class B extends A { def useIn(i: In): In = i; def again: In = mk; def me: Me = this }
object II {
  val b = new B
  val i: b.In = b.mk
  val j: b.In = b.again
  val k: A#In = i
  val m: b.type = b.me
  val n: b.type = i.a
  val c = new B
  val bad: c.In = b.mk
}
