//> tc --scala 3.3
type Elem[X] = X match
  case String => Char
  case Array[t] => t
  case Iterable[t] => t
object MT:
  val c: Elem[String] = 'c'
  val i: Elem[List[Int]] = 1
  val a: Elem[Array[Boolean]] = true
  def first[X](x: X): Elem[X] = ???
  val f = first("s")
  type __q_e = Elem[Vector[String]]
// ? T f
// ? E e ; String
