class Outer { o =>
  class Inner { def outer: Outer.this.type = Outer.this; def self: o.type = o }
  def mk: Inner = new Inner
  val i: Inner = mk
  val back: this.type = i.outer
}
object TI {
  val a = new Outer
  val b = new Outer
  val ai: a.Inner = a.mk
  val bad: b.Inner = a.mk
  val proj: Outer#Inner = ai
  val o: a.type = ai.outer
}
