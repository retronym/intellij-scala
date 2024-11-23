package api {

  abstract class Universe extends Trees with Symbols {
  }

  trait Symbols {
    self: Universe =>
    type Symbol

  }

  trait Trees {
    self: Universe =>
    abstract class Transformer {
      type ApiTreesSymbol = Symbol
    }
    class InternalTransformer extends Transformer
  }
}

package nsc {

  trait Trees extends api.Trees {
    self: Global =>
    type AstTransformer = Transformer
    class Symbol

    // Shadowing of a class here is deprecated but still should work.
    // It seems to be required to trigger the bug in the IDE.
    class Transformer extends InternalTransformer
  }

  class Global extends api.Universe with Trees {
    class S {
      def s(s: Symbol) = ()
    }
  }

  trait Transform {
    val global: Global
  }

  abstract class RefChecks extends Transform {

    class RefCheckTransformer extends global.AstTransformer {
      new global.S {
        override def s<caret>(s: ApiTreesSymbol): Unit = ()
      }
    }
  }
}
