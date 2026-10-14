//> tc --scala 3.3
object Ext:
  extension (s: String)
    def shout: String = s.toUpperCase
    def times(n: Int): String = s * n
  extension [T](xs: List[T])
    def second: T = xs.tail.head
  val a = "x".shout
  val b = "x".times(3)
  val c = List(1, 2).second
  extension (i: Int) def +++(j: Int): Int = i + j
  val d = 1 +++ 2
// ? T c
