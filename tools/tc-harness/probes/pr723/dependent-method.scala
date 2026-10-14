trait Key { type V; def default: V }
object IntKey extends Key { type V = Int; def default = 0 }
object StrKey extends Key { type V = String; def default = "" }
object DM {
  def get(k: Key): k.V = k.default
  def getAll(k1: Key, k2: Key): (k1.V, k2.V) = (get(k1), get(k2))
  val p: (Int, String) = getAll(IntKey, StrKey)
  def curried(k: Key)(f: k.V => k.V): k.V = f(k.default)
  val c: Int = curried(IntKey)(_ + 1)
  val ks: List[Key] = List(IntKey)
  val vs = ks.map(k => get(k))
  def nested(k: Key) = { val v = get(k); v }
  val n: Int = nested(IntKey)
}
// ? T vs
