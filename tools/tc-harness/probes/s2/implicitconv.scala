import scala.language.implicitConversions
object IC {
  final class Rich(val s: String) { def twice: String = s + s }
  implicit def rich(s: String): Rich = new Rich(s)
  val t = "a".twice
  implicit class IntOps(private val i: Int) extends AnyVal { def squared: Int = i * i }
  val sq = 3.squared
  def f(x: Long) = x
  val l = f(1)
  val big: BigInt = 3
}
