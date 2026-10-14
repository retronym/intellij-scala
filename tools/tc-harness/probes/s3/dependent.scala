//> tc --scala 3.3
trait Key { type Value }
object Name extends Key { type Value = String }
object Age extends Key { type Value = Int }
object Dp:
  def get(k: Key): k.Value = ???
  val n: String = get(Name)
  val a: Int = get(Age)
  def dep(k: Key)(v: k.Value): k.Value = v
  val d = dep(Age)(3)
  val f: (k: Key) => k.Value = k => get(k)
// ? T d
