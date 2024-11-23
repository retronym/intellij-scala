trait Base {
  class Tree
  abstract class HasFoo {
    def foo(t: Tree): Any
  }
}
abstract class SubClass extends SubTrait
trait SubTrait extends Base {
  self: SubClass // with SubTrait // WORKAROUND
  =>
  abstract class SubHasFoo extends HasFoo {
    // "Method 'foo' overrides nothing
    override def foo<caret>(t: Tree): Any
  }
}