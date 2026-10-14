final class Meter(val v: Double) extends AnyVal { def +(o: Meter): Meter = new Meter(v + o.v) }
case class Pair[A, B](a: A, b: B)
object VC {
  val m = new Meter(1) + new Meter(2)
  val p = Pair(1, "s")
  val q = p.copy(b = 2.0)
  val r = p.copy[String, String](a = "x")
  val Pair(x, y) = q
  val t = (x, y)
  val tup = Pair.unapply(p)
  val f = (Pair.apply[Int, Int] _).tupled
}
// ? T q
// ? T r
// ? T t
// ? T tup
