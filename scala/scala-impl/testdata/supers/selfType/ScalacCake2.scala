
trait Trees1 extends InternalTrees {
  self: Global1 =>

  abstract class Parens extends Tree {
    override def traverse<caret>(traverser: Traverser): Unit = {
    }
  }
}
class ApiUniverse extends ApiTrees {

}
trait ApiTrees { self: ApiUniverse =>
  type Tree <: TreeApi
  trait TreeApi {
    def traverse(traverser: Traverser) = ()
  }

  class Traverser
}
trait InternalTrees extends ApiTrees {
  self: InternalSymbolTable =>
  class Tree extends TreeApi
}
abstract class InternalSymbolTable extends ApiUniverse with InternalTrees {
}
abstract class Global1 extends InternalSymbolTable with Trees1 {
}
