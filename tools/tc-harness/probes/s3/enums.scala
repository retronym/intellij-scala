//> tc --scala 3.3
enum Color:
  case Red, Green
enum Opt[+T]:
  case Som(x: T)
  case Non
object En:
  val c: Color = Color.Red
  val o = Opt.Som(1)
  val n: Opt[Int] = Opt.Non
  def get[T](o: Opt[T], d: T): T = o match
    case Opt.Som(x) => x
    case Opt.Non    => d
  val g = get(o, 2)
  val all = Color.values
// ? T o
// ? T g
// ? T all
