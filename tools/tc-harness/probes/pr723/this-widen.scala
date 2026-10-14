class Node { def self: this.type = this; def other: Node = this; def chain = self.self.other.self }
class Sub extends Node { def subOnly = 1 }
object TW {
  val s = new Sub
  val x: Sub = s.self
  val y: s.type = s.self.self
  val z: Node = s.chain
  val w: Sub = s.chain
  def id[T <: Node](t: T): t.type = t.self
  val q: s.type = id(s)
  val n: Int = s.self.subOnly
}
