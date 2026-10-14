// c8fe7da346 / e0b1c60e76: lub keeps type member refinements only as scalac does
trait M { type T }
object LR {
  val a = new M { type T = Int }
  val b = new M { type T = Int }
  val c = new M { type T = String }
  val ab = if (true) a else b
  val ac = if (true) a else c
  val x: ab.T = 1
  def pick[X](p: X, q: X): X = p
  val pab = pick(a, b)
  val y: pab.T = 2
}
// ? T ab
// ? T ac
// ? T pab
