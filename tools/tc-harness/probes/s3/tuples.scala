//> tc --scala 3.3
object Tp:
  val t = (1, "a", true)
  val h = t.head
  val tl = t.tail
  val c = t ++ (2.0, 'c')
  val sz = t.size
  val z = (1, 2).zip(("a", "b"))
  val m = t.map[[X] =>> Option[X]]([X] => (x: X) => Some(x))
  val ap = t(1)
// ? T h
// ? T tl
// ? T c
// ? T ap
