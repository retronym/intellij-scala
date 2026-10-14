// b725bc2a42: a refinement member is not re-substituted from the path
trait Box { type A; def get: A; def self: Box { type A = Box.this.A } = this }
object RS {
  val ib: Box { type A = Int } = new Box { type A = Int; def get = 1 }
  val g: Int = ib.get
  val s = ib.self
  val sg: Int = s.get
  def wrap[X](x: X): Box { type A = List[X] } = new Box { type A = List[X]; def get = List(x) }
  val w: List[String] = wrap("s").get
  val ww: List[String] = wrap("s").self.get
}
