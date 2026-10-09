package org.jetbrains.plugins.scala.lang.typePbt

/** The generated programs: a single `object P` holding cake-shaped declarations and `type __q_<id> = ...` query aliases. */
object Ast {

  sealed trait Path
  /** `this` or `C.this` */
  final case class PThis(cls: Option[String]) extends Path
  final case class PId(name: String) extends Path
  final case class PSel(qual: Path, name: String) extends Path

  sealed trait Tp
  /** `name[args]`, or `prefix.name[args]` */
  final case class TRef(prefix: Option[Path], name: String, args: List[Tp]) extends Tp
  /** `qual#name` */
  final case class TProj(qual: Tp, name: String) extends Tp
  /** `path.type` */
  final case class TSingle(path: Path) extends Tp
  /** `A with B { type M = T }` */
  final case class TWith(parts: List[Tp], refinements: List[(String, Tp)]) extends Tp
  /** `Any`, `AnyRef`, `Nothing`, `Null`, `Int`, `String` */
  final case class TBuiltin(name: String) extends Tp

  final case class TParam(name: String, variance: String)

  sealed trait Member
  /** `type name = alias`, or `type name <: hi` */
  final case class TypeMem(name: String, alias: Option[Tp], hi: Option[Tp]) extends Member
  final case class ValMem(name: String, tpe: Tp) extends Member
  final case class ClassMem(cls: ClassDef) extends Member
  final case class Query(id: String, tpe: Tp) extends Member

  sealed trait Kind
  case object Trait extends Kind
  case object AbstractClass extends Kind
  case object ConcreteClass extends Kind

  final case class ClassDef(
    kind: Kind,
    name: String,
    tparams: List[TParam],
    parents: List[Tp],
    self: Option[Tp],
    members: List[Member]
  )

  final case class Program(members: List[Member]) {
    def queries: List[Query] = {
      def go(ms: List[Member]): List[Query] = ms.flatMap {
        case q: Query => List(q)
        case ClassMem(c) => go(c.members)
        case _ => Nil
      }
      go(members)
    }
  }

  // --- printing ---

  def show(p: Path): String = p match {
    case PThis(None)    => "this"
    case PThis(Some(c)) => s"$c.this"
    case PId(n)         => n
    case PSel(q, n)     => s"${show(q)}.$n"
  }

  def show(t: Tp): String = t match {
    case TRef(pre, n, args) =>
      pre.fold("")(p => show(p) + ".") + n + (if (args.isEmpty) "" else args.map(show).mkString("[", ", ", "]"))
    case TProj(q, n) => s"${simple(q)}#$n"
    case TSingle(p)  => s"${show(p)}.type"
    case TWith(parts, refs) =>
      val ps = parts.map(simple).mkString(" with ")
      if (refs.isEmpty) ps else refs.map { case (n, a) => s"type $n = ${show(a)}" }.mkString(s"$ps { ", "; ", " }")
    case TBuiltin(n) => n
  }

  /** `t`, parenthesized where it is a component of a compound or projection. */
  private def simple(t: Tp): String = t match {
    case _: TWith => s"(${show(t)})"
    case _        => show(t)
  }

  def show(m: Member, indent: String): String = m match {
    case TypeMem(n, Some(a), _) => s"${indent}type $n = ${show(a)}"
    case TypeMem(n, None, hi)   => s"${indent}type $n" + hi.fold("")(h => s" <: ${show(h)}")
    case ValMem(n, t)           => s"${indent}val $n: ${show(t)} = ???"
    case Query(id, t)           => s"${indent}type __q_$id = ${show(t)}"
    case ClassMem(c)            => show(c, indent)
  }

  def show(c: ClassDef, indent: String): String = {
    val kw = c.kind match {
      case Trait         => "trait"
      case AbstractClass => "abstract class"
      case ConcreteClass => "class"
    }
    val tps = if (c.tparams.isEmpty) "" else c.tparams.map(p => p.variance + p.name).mkString("[", ", ", "]")
    val ext = if (c.parents.isEmpty) "" else c.parents.map(show).mkString(" extends ", " with ", "")
    val self = c.self.fold("")(s => s" self: ${show(s)} =>")
    if (c.members.isEmpty && c.self.isEmpty) s"$indent$kw ${c.name}$tps$ext"
    else
      (s"$indent$kw ${c.name}$tps$ext {$self" +: c.members.map(show(_, indent + "  ")) :+ s"$indent}").mkString("\n")
  }

  def show(p: Program, pkg: String): String =
    (s"package $pkg\n\nobject P {" +: p.members.map(show(_, "  ")) :+ "}\n").mkString("\n")
}
