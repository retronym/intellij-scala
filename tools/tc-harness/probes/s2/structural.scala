import scala.language.reflectiveCalls
object ST {
  type Closeable = { def close(): Unit }
  def using[A <: Closeable, B](a: A)(f: A => B): B = try f(a) finally a.close()
  class Res { def close(): Unit = (); def read: String = "r" }
  val r: String = using(new Res)(_.read)
  val dyn: { val x: Int; def y(i: Int): String } = new { val x = 1; def y(i: Int) = i.toString }
  val dx: Int = dyn.x
  val dy: String = dyn.y(2)
  val bad: { def zz: Int } = new Res
}
