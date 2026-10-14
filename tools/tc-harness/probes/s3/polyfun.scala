//> tc --scala 3.3
object PF:
  val id: [A] => A => A = [A] => (a: A) => a
  val i = id(1)
  val s = id("s")
  val swap: [A, B] => ((A, B)) => (B, A) = [A, B] => (p: (A, B)) => (p._2, p._1)
  val w = swap((1, "a"))
// ? T i
// ? T w
