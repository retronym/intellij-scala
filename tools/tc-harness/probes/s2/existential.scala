object Ex {
  val xs: List[_ <: CharSequence] = List("a")
  val h = xs.head
  def f(x: Map[_, _ <: Seq[Int]]) = x.values.head.sum
  val c: Class[_ <: String] = "".getClass
  type E = List[T] forSome { type T <: AnyVal }
  val e: E = List(1)
}
// ? T h
// ? C List[Int] ; Ex.E
