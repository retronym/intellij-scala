trait A { type T <: AnyRef; def t: T }
trait B extends A { type T <: CharSequence }
class C extends B { type T = String; def t = "s" }
object AO {
  val c = new C
  val s: String = c.t
  val cs: CharSequence = (c: B).t
  val bad: String = (c: B).t
  def f(b: B): b.T = b.t
  val ft: String = f(c)
}
