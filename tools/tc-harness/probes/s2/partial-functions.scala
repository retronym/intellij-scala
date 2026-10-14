object PFn {
  val pf: PartialFunction[Any, Int] = { case i: Int => i; case s: String => s.length }
  val collected = List(1, "ab", 2.0).collect(pf)
  val c2 = List(1, "ab").collect { case s: String => s }
  val lifted = pf.lift
  val orElse = pf.orElse[Any, Int] { case _ => 0 }
  val m = Map(1 -> "a").map { case (k, v) => v -> k }
}
// ? T c2
// ? T lifted
// ? T m
