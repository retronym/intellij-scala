// 6f9595ad8d: members of an import's wildcard among selectors are seen from the qualifier
trait Api { type T; def mk: T; class Box(val t: T) }
object Impl extends Api { type T = String; def mk = "a" }
object IW {
  import Impl.{mk => make, _}
  val t: String = make
  val b = new Box("s")
  val bt: String = b.t
  val x: T = "q"
  def use(a: Api) = { import a._; val m: T = mk; new Box(m) }
  val u = use(Impl)
}
