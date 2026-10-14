//> tc --scala 3.3
trait Ord[T]:
  def compare(a: T, b: T): Int
given Ord[Int] with
  def compare(a: Int, b: Int) = a - b
given [T](using o: Ord[T]): Ord[List[T]] with
  def compare(a: List[T], b: List[T]) = 0
object Gv:
  def max[T](a: T, b: T)(using o: Ord[T]): T = if o.compare(a, b) >= 0 then a else b
  val m = max(List(1), List(2))
  def sorted[T: Ord](xs: List[T]) = xs
  val s = sorted(List(List(1)))
  val o = summon[Ord[List[List[Int]]]]
// ? T m
