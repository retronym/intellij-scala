// 961bf9c13e: self type members anchored at the self type's class; self alias in a path is C.this
trait Syms { type Sym; def mk: Sym }
trait Trees { self: Syms =>
  class Tree { var sym: Sym = mk }
  def symOf(t: Tree): self.Sym = t.sym
  def symOf2(t: Tree): Trees.this.Sym = t.sym
  def symOf3(t: Tree): Sym = symOf(t)
}
abstract class U extends Syms with Trees { def all(t: Tree): List[Sym] = List(symOf(t), symOf2(t), symOf3(t)) }
object UImpl extends U { type Sym = String; def mk = "s" }
object SA2 {
  val t = new UImpl.Tree
  val s: String = UImpl.symOf(t)
  val l: List[String] = UImpl.all(t)
  val other = new U { type Sym = Int; def mk = 1 }
  val bad: Int = UImpl.symOf(t)
}
