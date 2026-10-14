//> tc --scala 3.3
object Un:
  def f(x: Int | String): String = x match
    case i: Int    => i.toString
    case s: String => s
  val u = if true then 1 else "s"
  val l = List(1, "a")
  val m: Matchable = u
  type __q_is = Int | String
  type __q_ia = Int & Any
// ? T u
// ? T l
// ? C is ; Matchable
// ? C is ; Any
// ? C Int ; Matchable
// ? C String ; Matchable
// ? E ia ; Int
