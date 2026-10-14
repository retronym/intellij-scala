trait Universe { u =>
  type Tree <: TreeApi
  trait TreeApi { def children: List[Tree] }
  type Symbol <: SymbolApi
  trait SymbolApi { def owner: Symbol; def tree: Tree }
}
abstract class Global extends Universe {
  class Tree extends TreeApi { def children: List[Tree] = Nil }
  class Symbol extends SymbolApi { def owner: Symbol = this; def tree: Tree = new Tree }
}
trait Plugin { val global: Global; import global._
  def walk(t: Tree): List[Tree] = t :: t.children.flatMap(walk)
  def own(s: Symbol): global.Symbol = s.owner
  def tr(s: Symbol): Tree = s.tree
}
object CC {
  val g: Global = new Global {}
  val p = new Plugin { val global: g.type = g }
  val t: g.Tree = new g.Tree
  val w: List[g.Tree] = p.walk(t)
  val sym: g.Symbol = p.own(new g.Symbol)
}
