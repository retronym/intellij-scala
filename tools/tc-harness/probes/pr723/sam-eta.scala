// 6e9e5d6f1e: SAM eta-expansion
trait Fn { def apply(i: Int): String }
object SE {
  def m(i: Int): String = i.toString
  val f: Fn = m
  val g: Fn = m _
  val r: Runnable = () => println()
  def take(f: Fn) = f(1)
  val t = take(m)
  def poly[A](a: A): String = a.toString
  val p: Fn = poly
  def two(a: Int, b: Int) = a + b
  val bad: Fn = two
}
