trait Box { type A; val a: A }
object RM {
  val ib: Box { type A = Int } = new Box { type A = Int; val a = 1 }
  val n: Int = ib.a
  def mk[X](x: X): Box { type A = X } = new Box { type A = X; val a = x }
  val s: String = mk("s").a
  val anyBox: Box = ib
  val bad: Int = anyBox.a
  def poly(b: Box)(x: b.A): b.A = x
  val p: Int = poly(ib)(2)
}
