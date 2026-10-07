package org.jetbrains.plugins.scala.caches

import scala.quoted.*

/**
 * Identifies a cache. Create it with `cacheId[this.type, "getType"]`, which computes both strings at compile time.
 *
 * @param id          unique key, e.g. `org$jetbrains$plugins$scala$Foo$getType$cacheKey`
 * @param displayName shown in the cache statistics and the Tracer, e.g. `Foo.getType`
 */
final class CacheId(val id: String, val displayName: String) {
  override def toString: String = id
}

object CacheId {
  inline def of[T, N <: String & Singleton]: CacheId = ${ impl[T, N] }

  private def impl[T: Type, N <: String : Type](using Quotes): Expr[CacheId] = {
    import quotes.reflect.*

    val cls = TypeRepr.of[T].dealias match {
      case t: ThisType => t.tref.typeSymbol // `this.type`: the enclosing class, ignoring any self-type
      case t => t.widen.classSymbol.getOrElse(report.errorAndAbort(s"cacheId[...] needs a class type, got ${Type.show[T]}"))
    }
    val name = Type.valueOfConstant[N].getOrElse(report.errorAndAbort(s"cacheId[...] needs a literal string type, got ${Type.show[N]}"))

    // The path of named owners of the class, so that local classes in different methods get different ids.
    def path(s: Symbol): List[String] =
      if (s.isNoSymbol || s == defn.RootClass || s.name == "<root>" || s.name == "<empty>") Nil
      else if (s.flags.is(Flags.Synthetic) || s.isLocalDummy) path(s.owner)
      else s.name.stripSuffix("$") :: path(s.owner)

    val id = (path(cls).reverse :+ name).map(_.replace('.', '$')).mkString("", "$", "$cacheKey")
    val displayName = cls.name.stripSuffix("$") + "." + name
    '{ new CacheId(${ Expr(id) }, ${ Expr(displayName) }) }
  }
}
