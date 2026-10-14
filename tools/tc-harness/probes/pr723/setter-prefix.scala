// 6f9595ad8d: a setter is seen from its getter's prefix
trait Cell { type V; var value: V }
class IntCell extends Cell { type V = Int; var value: Int = 0 }
object SP {
  val c = new IntCell
  c.value = 3
  c.value += 1
  val g: Cell = c
  g.value = g.value
  def set(x: Cell)(v: x.V): Unit = x.value = v
  set(c)(5)
  c.value = "s"
}
