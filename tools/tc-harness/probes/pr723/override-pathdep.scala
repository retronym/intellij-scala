trait U { type Tree; type Sym; trait Traverser { def traverse(t: Tree): Unit; def sym(s: Sym): Sym = s } }
trait G extends U { class Tree; class Sym
  class MyTraverser extends Traverser { override def traverse(t: Tree): Unit = (); override def sym(s: Sym): Sym = s }
  abstract class Base { def f(t: G.this.Tree): Tree }
  class Impl extends Base { def f(t: Tree): G.this.Tree = t }
}
trait H extends U { self: G =>
  class T2 extends Traverser { def traverse(t: Tree): Unit = () }
}
