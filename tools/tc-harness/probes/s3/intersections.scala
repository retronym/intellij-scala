//> tc --scala 3.3
trait A { def a: Int = 1 }
trait B { def b: String = "" }
object In:
  def both(x: A & B): (Int, String) = (x.a, x.b)
  val ab = new A with B {}
  val r = both(ab)
  def f[T <: A & B](t: T) = t.a + t.b.length
  type __q_ba = B & A
  type __q_ab = A & B
// ? E ab ; ba
// ? C ab ; A
