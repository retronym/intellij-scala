object OV {
  def f(x: Int): String = "int"
  def f(x: Long): Int = 1
  def f(x: Any*): Boolean = true
  val a = f(1)
  val b = f(1L)
  val c = f("s")
  def g(x: Int, y: String = "d"): Int = x
  def g(x: String): String = x
  val d = g(1)
  def h[A](x: A)(implicit ev: A <:< AnyVal): A = x
  val e = h(1)
}
// ? T a
// ? T b
// ? T c
// ? T d
