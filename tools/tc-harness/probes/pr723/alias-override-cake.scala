// the shape behind b3d1e57b10: scala/scala StdNames `override type NameType = TypeName` over TypeNamesApi's
package api {
  trait Names { type Name >: Null <: AnyRef; type TypeName >: Null <: Name; type TermName >: Null <: Name }
  trait StandardNames { self: Universe =>
    trait NamesApi { type NameType >: Null <: Name }
    trait TypeNamesApi extends NamesApi { type NameType = TypeName }
    trait TermNamesApi extends NamesApi { type NameType = TermName }
  }
  abstract class Universe extends Names with StandardNames
}
package internal {
  trait Names extends api.Names { abstract class Name; class TypeName extends Name; class TermName extends Name }
  trait StdNames { self: SymbolTable =>
    abstract class Keywords { type NameType <: Name }
    trait TypeNames extends Keywords with TypeNamesApi { override type NameType = TypeName }
    trait TermNames extends Keywords with TermNamesApi { override type NameType = TermName }
    trait WrongNames extends Keywords with TermNamesApi { override type NameType = TypeName }
  }
  abstract class SymbolTable extends api.Universe with Names with StdNames
}
