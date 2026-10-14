//> tc --scala 3.3
class Printer:
  def print(s: String): String = s
  type Out = String
class Scanner:
  def scan(): Int = 1
class Copier:
  private val p = Printer()
  private val s = Scanner()
  export p.{print, Out}
  export s.*
object Ex:
  val c = Copier()
  val x: c.Out = c.print("a")
  val y = c.scan()
// ? T x
