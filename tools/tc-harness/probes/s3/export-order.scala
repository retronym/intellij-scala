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
class CopierNoStar:
  private val p = Printer()
  export p.{print, Out}
class CopierStarFirst:
  private val s = Scanner()
  export s.*
  private val p = Printer()
  export p.{print, Out}
object Ex3:
  val c = Copier()
  val x: c.Out = c.print("a")
  val n = CopierNoStar()
  val y: n.Out = n.print("a")
  val f = CopierStarFirst()
  val z: f.Out = f.print("a")
