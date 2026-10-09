package org.jetbrains.plugins.scala.lang.typePbt

import org.jetbrains.plugins.scala.lang.typePbt.Ast.*

/**
 * One-step simplifications of a program, smallest first where cheap to tell.
 * Query aliases named in `keep` are never deleted (their types still shrink).
 */
object Shrink {

  def program(p: Program, keep: Set[String]): LazyList[Program] =
    members(p.members, keep).map(Program(_))

  private def members(ms: List[Member], keep: Set[String]): LazyList[List[Member]] = {
    val deletions = LazyList.from(ms.indices).filter(i => deletable(ms(i), keep)).map(i => ms.patch(i, Nil, 1))
    val inner = LazyList.from(ms.indices).flatMap(i => member(ms(i), keep).map(m => ms.updated(i, m)))
    deletions #::: inner
  }

  private def deletable(m: Member, keep: Set[String]): Boolean = m match {
    case Query(id, _) => !keep(id)
    case ClassMem(c)  => !mentionsKept(c, keep)
    case _            => true
  }

  private def mentionsKept(c: ClassDef, keep: Set[String]): Boolean = c.members.exists {
    case Query(id, _) => keep(id)
    case ClassMem(i)  => mentionsKept(i, keep)
    case _            => false
  }

  private def member(m: Member, keep: Set[String]): LazyList[Member] = m match {
    case TypeMem(n, Some(a), hi) => TypeMem(n, None, hi) #:: tp(a).map(a2 => TypeMem(n, Some(a2), hi))
    case TypeMem(n, None, Some(h)) => TypeMem(n, None, None) #:: tp(h).map(h2 => TypeMem(n, None, Some(h2)))
    case TypeMem(_, None, None) => LazyList.empty
    case ValMem(n, t) => tp(t).map(ValMem(n, _))
    // a class alias names the class a `baseType` question is about; keep it
    case Query(id, _) if id.startsWith("c") => LazyList.empty
    case Query(id, t) => tp(t).map(Query(id, _))
    case ClassMem(c) => classDef(c, keep).map(ClassMem(_))
  }

  private def classDef(c: ClassDef, keep: Set[String]): LazyList[ClassDef] = {
    val dropSelf = c.self.map(_ => c.copy(self = None)).to(LazyList)
    val dropParent = LazyList.from(c.parents.indices).map(i => c.copy(parents = c.parents.patch(i, Nil, 1)))
    val inMembers = members(c.members, keep).map(ms => c.copy(members = ms))
    val inSelf = c.self.to(LazyList).flatMap(s => tp(s).map(s2 => c.copy(self = Some(s2))))
    val inParents = LazyList.from(c.parents.indices).flatMap(i => tp(c.parents(i)).map(p => c.copy(parents = c.parents.updated(i, p))))
    val toTrait = if (c.kind == AbstractClass) LazyList(c.copy(kind = Trait)) else LazyList.empty
    dropSelf #::: dropParent #::: inMembers #::: inSelf #::: inParents #::: toTrait
  }

  def tp(t: Tp): LazyList[Tp] = {
    val trivial = t match {
      case TBuiltin("Any") => LazyList.empty
      case TBuiltin(_)     => LazyList(TBuiltin("Any"))
      case _               => LazyList(TBuiltin("Any"), TBuiltin("Nothing"))
    }
    val structural: LazyList[Tp] = t match {
      case TRef(pre, n, args) =>
        args.to(LazyList) #:::
          pre.to(LazyList).flatMap(p => path(p).map(p2 => TRef(Some(p2), n, args))) #:::
          LazyList.from(args.indices).flatMap(i => tp(args(i)).map(a => TRef(pre, n, args.updated(i, a))))
      case TProj(q, n) =>
        q #:: tp(q).map(TProj(_, n))
      case TSingle(p) =>
        path(p).map(TSingle(_))
      case TWith(parts, refs) =>
        val keepOne = if (parts.size < 2) LazyList.empty else parts.to(LazyList).map(p => compound(List(p), refs))
        val dropRef = LazyList.from(refs.indices).map(i => compound(parts, refs.patch(i, Nil, 1)))
        val inParts = LazyList.from(parts.indices).flatMap(i => tp(parts(i)).map(p => compound(parts.updated(i, p), refs)))
        val inRefs = LazyList.from(refs.indices).flatMap(i => tp(refs(i)._2).map(r => compound(parts, refs.updated(i, refs(i)._1 -> r))))
        keepOne #::: dropRef #::: inParts #::: inRefs
      case TBuiltin(_) => LazyList.empty
    }
    trivial #::: structural
  }

  /** A single unrefined part prints as itself, so it must be itself. */
  private def compound(parts: List[Tp], refs: List[(String, Tp)]): Tp =
    if (parts.size == 1 && refs.isEmpty) parts.head else TWith(parts, refs)

  private def path(p: Path): LazyList[Path] = p match {
    case PSel(q, n) => q #:: path(q).map(PSel(_, n))
    case PThis(Some(_)) => LazyList(PThis(None))
    case _ => LazyList.empty
  }
}
