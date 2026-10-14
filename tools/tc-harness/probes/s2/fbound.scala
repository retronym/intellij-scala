trait Ord[A <: Ord[A]] { def cmp(o: A): Int }
final class N(val n: Int) extends Ord[N] { def cmp(o: N) = n - o.n }
object FB {
  def max[A <: Ord[A]](xs: List[A]): A = xs.reduce((a, b) => if (a.cmp(b) >= 0) a else b)
  val m = max(List(new N(1), new N(2)))
  type __q_m = N
}
// ? T m
