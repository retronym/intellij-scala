object Sm {
  trait F { def apply(x: Int): String }
  val f: F = x => x.toString
  val r: Runnable = () => ()
  val cmp: java.util.Comparator[String] = (a, b) => a.length - b.length
  abstract class Abs[A] { def run(a: A): A }
  val ab: Abs[Int] = _ + 1
  def take(f: F) = f(1)
  val t = take(_.toString)
}
