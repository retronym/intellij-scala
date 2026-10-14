trait Show[A] { def show(a: A): String }
object Show {
  implicit val int: Show[Int] = _.toString
  implicit def list[A](implicit s: Show[A]): Show[List[A]] = _.map(s.show).mkString(",")
  implicit def pair[A: Show, B: Show]: Show[(A, B)] = p => implicitly[Show[A]].show(p._1) + implicitly[Show[B]].show(p._2)
}
object Tc {
  def show[A](a: A)(implicit s: Show[A]) = s.show(a)
  val s1 = show(List((1, List(2))))
  val s2 = implicitly[Show[List[List[Int]]]]
}
