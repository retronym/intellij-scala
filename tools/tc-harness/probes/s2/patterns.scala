object Pt {
  sealed trait Expr[T]
  case class IntE(i: Int) extends Expr[Int]
  case class BoolE(b: Boolean) extends Expr[Boolean]
  def eval[T](e: Expr[T]): T = e match {
    case IntE(i)  => i
    case BoolE(b) => b
  }
  val xs: Any = List(1)
  val n = xs match { case l: List[Int @unchecked] => l.sum; case _ => 0 }
  val Some((a, b)) = Some((1, "s"))
  val t = (a, b)
}
// ? T t
