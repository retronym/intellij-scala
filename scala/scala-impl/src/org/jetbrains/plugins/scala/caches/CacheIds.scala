package org.jetbrains.plugins.scala.caches

import scala.quoted.*

/**
 * Compile-time cache ids and display names, derived from the call site of the `inline` helpers in the `caches` package
 * object.
 *
 * The id is the path of named definitions enclosing the call site (package, classes, methods, vals), followed by the
 * cache name, e.g. `org$jetbrains$plugins$scala$Foo$bar$bar$cacheKey`. Including the enclosing method makes ids unique
 * even when two methods in a class (or local classes in different methods) use the same cache name.
 *
 * The display name, used by the cache statistics and the Tracer, is the enclosing class's simple name followed by the
 * cache name, e.g. `Foo.bar`.
 *
 * The helpers must call `id` and `display` directly in their (inlined) body, not inside a local definition: that
 * definition would become part of the id. They forward to non-inline implementations for that reason.
 */
private[caches] object CacheIds {
  inline def id(inline name: String): String = ${ idImpl('name) }

  inline def display(inline name: String): String = ${ displayImpl('name) }

  private def idImpl(name: Expr[String])(using Quotes): Expr[String] = {
    import quotes.reflect.*
    def path(s: Symbol): List[String] =
      if (s.isNoSymbol || s == defn.RootClass) Nil
      else if (s.flags.is(Flags.Synthetic) || s.isLocalDummy) path(s.owner)
      else s.name.stripSuffix("$") :: path(s.owner)
    val prefix = path(Symbol.spliceOwner).reverse.filterNot(_ == "<empty>")
    Expr((prefix :+ name.valueOrAbort).map(_.replace('.', '$')).mkString("", "$", "$cacheKey"))
  }

  private def displayImpl(name: Expr[String])(using Quotes): Expr[String] = {
    import quotes.reflect.*
    def enclosingClass(s: Symbol): Symbol =
      if (s.isClassDef && !s.flags.is(Flags.Synthetic)) s else enclosingClass(s.owner)
    Expr(enclosingClass(Symbol.spliceOwner).name.stripSuffix("$") + "." + name.valueOrAbort)
  }
}
