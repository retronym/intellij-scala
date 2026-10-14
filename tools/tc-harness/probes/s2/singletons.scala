object Sg {
  val a = 1
  final val b = 2
  val c: b.type = b
  val s: "lit" = "lit"
  def id[A](x: A): x.type = x
  val o = new Object
  val p: o.type = id(o)
  object M
  val m: M.type = M
  def same(x: AnyRef)(y: x.type): y.type = y
  val q = same(o)(o)
}
// ? T c
// ? T q
// ? C c ; Int
