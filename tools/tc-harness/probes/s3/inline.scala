//> tc --scala 3.3
import scala.compiletime.*
object In:
  inline def sz[T]: Int = inline erasedValue[T] match
    case _: Int    => 4
    case _: Long   => 8
    case _         => error("unsupported")
  val a = sz[Int]
  transparent inline def pick(inline b: Boolean) = inline if b then 1 else "s"
  val p = pick(true)
  val q = pick(false)
  val bad = sz[String]
// ? T p
// ? T q
