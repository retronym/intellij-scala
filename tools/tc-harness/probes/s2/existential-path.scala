trait Node { type V; def v: V; def same(x: V): Boolean = x == v }
object EP {
  val ns: List[Node] = Nil
  ns.foreach(n => n.same(n.v))
  def pick(n: Node): n.V = n.v
  val all = ns.map(n => pick(n))
  val bad = ns.map(n => n.same(1))
}
