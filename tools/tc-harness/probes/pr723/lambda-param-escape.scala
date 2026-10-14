trait Key { type V; def default: V }
object IntKey extends Key { type V = Int; def default = 0 }
object LK {
  def get(k: Key): k.V = k.default
  val ks: List[Key] = List(IntKey)
  val vs = ks.map(k => get(k))
  val a: List[Key#V] = vs
  val b: List[Int] = vs
  val c: List[Int] = ks.map(k => get(k))
  val d = ks.map { k => val v = get(k); v }
  val e: Option[Key#V] = ks.headOption.map(k => k.default)
  def f(xs: List[Key]) = xs.map(k => (k, get(k)))
  val g = f(ks)
  val h: List[(Key, Key#V)] = g
  val i = (k: Key) => get(k)
  val j: Key => Key#V = i
  val first = ks.map(k => get(k)).head
}
// ? T vs
// ? T d
// ? T g
// ? T i
// ? T first
