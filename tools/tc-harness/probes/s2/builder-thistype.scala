class Builder { var parts = List.empty[String]; def add(s: String): this.type = { parts ::= s; this } }
class HtmlBuilder extends Builder { def tag(t: String): this.type = add(s"<$t>") }
object BT {
  val h: HtmlBuilder = new HtmlBuilder().add("a").tag("p").add("b")
  val hb = new HtmlBuilder
  val same: hb.type = hb.add("x").tag("y")
  def chain[B <: Builder](b: B): B = b.add("z")
}
