// SCL-21947 (OverrideHighlightingTest.testSCL21947Cake): fixed on #723, red on base
package api {
  trait Types { self: Universe =>
    type Type
    type RefinedType
    abstract class RefinedTypeExtractor {
      def unapply(tpe: RefinedType): Type
    }
  }
  abstract class Universe extends Types
}
package internal {
  trait Types extends api.Types { self: SymbolTable =>
    abstract class Type
    abstract class RefinedType extends RefinedTypeExtractor {
      override def unapply(tpe: RefinedType): Type
    }
  }
  abstract class SymbolTable extends api.Universe with Types
}
