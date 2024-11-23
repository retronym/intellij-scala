object api {

  abstract class Universe extends Trees

  trait Trees {
    self: Universe =>
    type Tree
    type Block <: Tree
  }
}

object nsc {
  trait Analyzer {
    val global: Global

    class Typer {
      def typed(x: global.Tree) = ()
    }
  }

  class Global extends api.Universe {
    object analyzer extends Analyzer {
      val global: Global.this.type = Global.this
    }
  }

  abstract class RefChecks {
    val global: Global
    new global.analyzer.Typer {
      override def typed<caret>(x: global.Tree): Unit = super.typed(x)
    }
  }
}