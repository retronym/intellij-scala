package org.jetbrains.plugins.scala.lang.typePbt

import org.jetbrains.plugins.scala.lang.typePbt.Ast.*

import scala.collection.mutable.ListBuffer
import scala.util.Random

/** A question about the types of two query aliases (or an alias and a class alias). */
sealed trait Question {
  def encode: String = this match {
    case Question.Conforms(a, b) => s"C\t$a\t$b"
    case Question.Equiv(a, b)    => s"E\t$a\t$b"
    case Question.BaseType(a, c) => s"B\t$a\t$c"
  }
  def ids: List[String] = this match {
    case Question.Conforms(a, b) => List(a, b)
    case Question.Equiv(a, b)    => List(a, b)
    case Question.BaseType(a, c) => List(a, c)
  }
  def show: String = this match {
    case Question.Conforms(a, b) => s"$a <:< $b"
    case Question.Equiv(a, b)    => s"$a =:= $b"
    case Question.BaseType(a, c) => s"$a baseType $c"
  }
}
object Question {
  final case class Conforms(a: String, b: String) extends Question
  final case class Equiv(a: String, b: String) extends Question
  final case class BaseType(a: String, cls: String) extends Question
}

final case class Case(program: Program, questions: List[Question])

/**
 * Generates cake-shaped programs and questions over them.
 *
 * The generator keeps a rough model of the classes it declares (parents, self
 * types, inner classes, type members, vals) so that most type expressions it
 * writes resolve. It doesn't check everything scalac does (bounds of overriding
 * aliases, self-type conformance of parents, ...); scalac is the filter.
 */
final class Gen(rnd: Random, config: Gen.Config = Gen.Config()) {

  /** The generator's view of a declared class. */
  private final class Cls(val name: String, val outer: Option[Cls], val kind: Kind, val arity: Int = 0) {
    var parents: List[Cls] = Nil
    var selfs: List[Cls] = Nil
    /** Type members: name, the class of their upper bound/alias if known, whether abstract. */
    val typeMems: ListBuffer[(String, Option[Cls], Boolean)] = ListBuffer.empty
    val inner: ListBuffer[Cls] = ListBuffer.empty
    val vals: ListBuffer[(String, Option[Cls])] = ListBuffer.empty
    val body: ListBuffer[Member] = ListBuffer.empty
    var parentTps: List[Tp] = Nil

    def ancestors: List[Cls] = (this :: parents.flatMap(_.ancestors)).distinct
    /** What `this` sees: ancestors plus self types and their ancestors (not their self types). */
    def view: List[Cls] = (ancestors ++ selfs.flatMap(_.ancestors)).distinct

    def isTop: Boolean = outer.isEmpty
    /** Spelling of this class's type from the top level. */
    def topRef: Tp = outer match {
      case None    => TRef(None, name, List.fill(arity)(TBuiltin("Any")))
      case Some(o) => TProj(o.topRef, name)
    }
  }

  /** Members visible on an instance of `c`: through a path (`viaThis = false`) or through `this`. */
  private def typeMembers(c: Cls, viaThis: Boolean): List[(String, Option[Cls])] =
    (if (viaThis) c.view else c.ancestors).flatMap(a => a.typeMems.map(m => m._1 -> m._2) ++ a.inner.map(i => i.name -> Some(i))).distinct
  private def valMembers(c: Cls, viaThis: Boolean): List[(String, Option[Cls])] =
    (if (viaThis) c.view else c.ancestors).flatMap(_.vals).distinct

  /** A scope types are written in: the top level of `P`, or the body of a class. */
  private final case class Scope(cls: Option[Cls])

  private val library = List(
    new Cls("Box", None, ConcreteClass, 1),
    new Cls("Inv", None, ConcreteClass, 1),
    new Cls("Con", None, ConcreteClass, 1),
  )
  private val variance = Map("Box" -> "+", "Inv" -> "", "Con" -> "-")
  private val traits = ListBuffer.empty[Cls]
  private val composites = ListBuffer.empty[Cls]
  private val topVals = ListBuffer.empty[(String, Option[Cls], Tp)]
  private var fresh = 0
  private def freshName(prefix: String): String = { fresh += 1; s"$prefix$fresh" }

  private def chance(p: Double): Boolean = rnd.nextDouble() < p
  private def pick[A](xs: Seq[A]): A = xs(rnd.nextInt(xs.size))
  private def pickOpt[A](xs: Seq[A]): Option[A] = if (xs.isEmpty) None else Some(pick(xs))
  private def subset[A](xs: Seq[A], max: Int): List[A] = rnd.shuffle(xs.toList).take(rnd.nextInt(max + 1))

  def generate(): Case = {
    // 1. top-level traits, their parents and self types
    val nTraits = 2 + rnd.nextInt(config.maxTraits - 1)
    for (i <- 0 until nTraits) traits += new Cls(s"T$i", None, Trait)
    for ((t, i) <- traits.zipWithIndex) {
      t.parents = subset(traits.take(i).toSeq, 2)
      val inherited = t.parents.flatMap(_.selfs).filterNot(t.ancestors.contains)
      val own = if (chance(config.selfTypeChance)) pickOpt(traits.filterNot(t.ancestors.contains).toSeq).toList else Nil
      t.selfs = (inherited ++ own).distinct
    }

    // 2. inner classes and abstract type members, in declaration order
    for (t <- traits) {
      for (_ <- 0 until rnd.nextInt(3)) t.inner += new Cls(freshName("I"), Some(t), if (chance(0.5)) Trait else AbstractClass)
      for (_ <- 0 until rnd.nextInt(3)) {
        val bound = if (chance(0.5)) pickOpt(typeMembers(t, viaThis = true).collect { case (_, Some(c)) if !c.isTop => c }) else None
        val n = freshName("M")
        t.typeMems += ((n, bound, true))
        t.body += TypeMem(n, None, bound.map(b => TRef(None, b.name, Nil)))
      }
    }
    // inner classes extend inner classes visible through `this` (inherited, or from the self type)
    val created = traits.flatMap(_.inner).toList
    for (t <- traits; i <- t.inner) {
      val candidates = t.view.flatMap(_.inner).filter(c => created.indexOf(c) < created.indexOf(i) && c.kind == Trait)
      i.parents = subset(candidates, 2)
      i.parentTps = i.parents.map(p => TRef(None, p.name, Nil))
    }

    // 3. composites: abstract classes that mix traits in, closed under self types
    for (k <- 0 until 1 + rnd.nextInt(2)) {
      val c = new Cls(s"K$k", None, AbstractClass)
      var ps = (pick(traits.toSeq) :: subset(traits.toSeq, 2)).distinct
      var changed = true
      while (changed) {
        val need = ps.flatMap(_.view).filter(traits.contains).distinct
        changed = need.exists(!ps.contains(_))
        ps = (ps ++ need).distinct
      }
      // a trait ahead of its own parents in the extends clause is fine, but keep parents first
      c.parents = ps.sortBy(traits.indexOf)
      c.parentTps = c.parents.map(p => TRef(None, p.name, Nil))
      composites += c
    }

    // 4. vals (stable paths), overriding aliases, inner bodies
    for (t <- traits) {
      for (_ <- 0 until rnd.nextInt(3)) addVal(t)
      for (i <- t.inner if chance(0.4)) addVal(i)
      overrideTypeMembers(t)
    }
    for (c <- composites) overrideTypeMembers(c)
    for (c <- composites) topVals += ((s"k${c.name.drop(1)}", Some(c), TRef(None, c.name, Nil)))
    for (_ <- 0 until rnd.nextInt(3)) {
      val (tp, cls) = genType(Scope(None), 1, classOnly = true)
      topVals += ((freshName("v"), pathClass(tp, cls), tp))
    }
    if (chance(0.4)) {
      val p = pick(paths(Scope(None)))
      topVals += ((freshName("a"), Some(p._2), TSingle(p._1)))
    }

    // 5. queries
    val queryScopes = Scope(None) :: (traits ++ composites).map(c => Scope(Some(c))).toList
    val queryTypes = ListBuffer.empty[(String, Option[Cls])]
    for (q <- 0 until config.queriesPerProgram) {
      val scope = if (chance(0.5)) Scope(None) else pick(queryScopes)
      val (tp, cls) = genType(scope, 2)
      val id = s"q$q"
      addQuery(scope, id, tp)
      queryTypes += ((id, cls))
      // a variant spelling of the same type, so that some answers are `true`
      variants(scope, tp, cls).foreach { case (vtp, vscope) =>
        val vid = s"q${q}v"
        addQuery(vscope, vid, vtp)
        queryTypes += ((vid, cls))
      }
    }
    val classAliases = scala.collection.mutable.LinkedHashMap.empty[Cls, String]
    def classAlias(c: Cls): String = classAliases.getOrElseUpdate(c, {
      val id = "c" + (c.outer.map(_.name + "_").getOrElse("") + c.name)
      topQueries += Query(id, c.topRef)
      id
    })

    val questions = ListBuffer.empty[Question]
    val ids = queryTypes.map(_._1).toVector
    for ((id, _) <- queryTypes if id.endsWith("v")) {
      val orig = id.dropRight(1)
      questions += Question.Conforms(orig, id) += Question.Conforms(id, orig) += Question.Equiv(orig, id)
    }
    for (_ <- 0 until config.pairsPerProgram) {
      val a = pick(ids); val b = pick(ids)
      questions += (if (chance(0.7)) Question.Conforms(a, b) else Question.Equiv(a, b))
    }
    for (case (id, Some(c)) <- queryTypes; anc <- (c.ancestors ++ (if (chance(0.3)) pickOpt(traits.toSeq).toList else Nil)).distinct if chance(0.5))
      questions += Question.BaseType(id, classAlias(anc))

    Case(Program(render()), questions.distinct.toList)
  }

  private val topQueries = ListBuffer.empty[Member]

  private def addVal(c: Cls): Unit = {
    val scope = Scope(Some(if (c.isTop) c else c.outer.get))
    // in an inner class, a bare `this` would mean the inner class, not the scope's
    def ok(tp: Tp): Boolean = c.isTop || !mentionsBareThis(tp)
    val (tp, cls) = Iterator.continually(genType(scope, 1, classOnly = true)).find(r => ok(r._1)).get
    val n = freshName("v")
    c.vals += ((n, pathClass(tp, cls)))
    c.body += ValMem(n, tp)
  }

  private def overrideTypeMembers(c: Cls): Unit = {
    val inherited = c.parents.flatMap(_.ancestors).flatMap(a => a.typeMems.filter(_._3).map(a -> _))
    val aliased = c.ancestors.flatMap(_.typeMems.filterNot(_._3).map(_._1)).toSet
    for ((_, (n, bound, _)) <- inherited.distinctBy(_._2._1) if !aliased(n) && chance(config.overrideChance)) {
      // subclasses of the bound among the inner classes (not type members: `type M = M` is cyclic)
      val subs = bound.toList.flatMap(b => typeMembers(c, viaThis = true).collect { case (sn, Some(s)) if sn == s.name && s.ancestors.contains(b) => sn })
      val alias: Tp = bound match {
        case Some(b) => TRef(None, pick(b.name :: subs), Nil)
        case None    => Iterator.continually(genType(Scope(Some(c)), 1)._1).find(t => !mentionsName(t, n)).get
      }
      c.typeMems += ((n, bound, false))
      c.body += TypeMem(n, Some(alias), None)
    }
  }

  private def mentionsName(tp: Tp, n: String): Boolean = tp match {
    case TRef(_, m, args) => m == n || args.exists(mentionsName(_, n))
    case TProj(q, m)      => m == n || mentionsName(q, n)
    case TSingle(_)       => false
    case TWith(ps, refs)  => ps.exists(mentionsName(_, n)) || refs.exists(r => mentionsName(r._2, n))
    case TBuiltin(_)      => false
  }

  /** Values of a compound type with an abstract part are volatile, so they don't form stable paths. */
  private def pathClass(tp: Tp, cls: Option[Cls]): Option[Cls] = tp match {
    case _: TWith => None
    case _        => cls
  }

  private def mentionsBareThis(tp: Tp): Boolean = {
    def path(p: Path): Boolean = p match {
      case PThis(None) => true
      case PSel(q, _)  => path(q)
      case _           => false
    }
    tp match {
      case TRef(pre, _, args) => pre.exists(path) || args.exists(mentionsBareThis)
      case TProj(q, _)        => mentionsBareThis(q)
      case TSingle(p)         => path(p)
      case TWith(ps, refs)    => ps.exists(mentionsBareThis) || refs.exists(r => mentionsBareThis(r._2))
      case TBuiltin(_)        => false
    }
  }

  private def addQuery(scope: Scope, id: String, tp: Tp): Unit = scope.cls match {
    case None    => topQueries += Query(id, tp)
    case Some(c) => c.body += Query(id, tp)
  }

  /** Stable paths in `scope`, with the class of the object they denote and whether they see the self type. */
  private def paths(scope: Scope): List[(Path, Cls, Boolean)] = {
    val roots: List[(Path, Cls, Boolean)] =
      topVals.toList.collect { case (n, Some(c), _) => (PId(n), c, false) } ++
        scope.cls.toList.flatMap { c =>
          List((PThis(None), c, true), (PThis(Some(c.name)), c, true)) ++
            (if (c.selfs.nonEmpty) List((PId("self"), c, true)) else Nil)
        }
    def extend(p: (Path, Cls, Boolean), depth: Int): List[(Path, Cls, Boolean)] =
      if (depth == 0) List(p)
      else p :: valMembers(p._2, p._3).collect { case (v, Some(vc)) => (PSel(p._1, v), vc, false) }.flatMap(extend(_, depth - 1))
    roots.flatMap(extend(_, config.maxPathDepth - 1))
  }

  /** A type expression valid in `scope`, with the class of its instances when the generator knows it. */
  private def genType(scope: Scope, depth: Int, classOnly: Boolean = false): (Tp, Option[Cls]) = {
    val ps = paths(scope)
    val options = ListBuffer.empty[(Double, () => (Tp, Option[Cls]))]
    if (!classOnly) options += config.builtinWeight -> (() => (TBuiltin(pick(List("Any", "AnyRef", "Nothing", "Null", "Int", "String"))): Tp) -> (None: Option[Cls]))
    options += 1.0 -> { () => val c = pick((traits ++ composites).toSeq); TRef(None, c.name, Nil) -> Some(c) }
    val projectable = (traits ++ composites).filter(c => typeMembers(c, viaThis = false).nonEmpty)
    if (projectable.nonEmpty) options += 1.5 -> { () =>
      val c = pick(projectable.toSeq); val (m, mc) = pick(typeMembers(c, viaThis = false))
      TProj(TRef(None, c.name, Nil), m) -> mc
    }
    val selectable = ps.filter(p => typeMembers(p._2, p._3).nonEmpty)
    if (selectable.nonEmpty) options += 3.0 -> { () =>
      val (p, c, viaThis) = pick(selectable); val (m, mc) = pick(typeMembers(c, viaThis))
      TRef(Some(p), m, Nil) -> mc
    }
    scope.cls.foreach { c =>
      val ms = typeMembers(c, viaThis = true)
      if (ms.nonEmpty) options += 1.5 -> { () => val (m, mc) = pick(ms); TRef(None, m, Nil) -> mc }
    }
    if (ps.nonEmpty) options += 1.5 -> { () => val (p, c, _) = pick(ps); TSingle(p) -> Some(c) }
    if (depth > 0) {
      options += 1.0 -> { () =>
        val b = pick(library); TRef(None, b.name, List(genType(scope, depth - 1)._1)) -> Some(b)
      }
      options += 0.7 -> { () =>
        val (a, ac) = genType(scope, depth - 1, classOnly = true)
        val (b, _) = genType(scope, depth - 1, classOnly = true)
        val refinable = ac.toList.flatMap(c => c.ancestors.flatMap(_.typeMems.filter(_._3).map(_._1)))
        val refs = if (refinable.nonEmpty && chance(0.3)) List(pick(refinable) -> genType(scope, depth - 1)._1) else Nil
        TWith(List(a, b), refs) -> ac
      }
      // C { type M = T }, refining an abstract type member of C
      val refinableClasses = (traits ++ composites).filter(_.ancestors.exists(_.typeMems.exists(_._3)))
      if (refinableClasses.nonEmpty) options += config.refinementWeight -> { () =>
        val c = pick(refinableClasses.toSeq)
        val m = pick(c.ancestors.flatMap(_.typeMems.filter(_._3).map(_._1)))
        TWith(List(TRef(None, c.name, Nil)), List(m -> genType(scope, depth - 1)._1)) -> Some(c)
      }
    }
    val total = options.map(_._1).sum
    var x = rnd.nextDouble() * total
    options.find { case (w, _) => x -= w; x < 0 }.getOrElse(options.last)._2()
  }

  /** Other spellings of `tp`, in the same or another scope. */
  private def variants(scope: Scope, tp: Tp, cls: Option[Cls]): Option[(Tp, Scope)] = {
    val alternatives: List[(Tp, Scope)] = tp match {
      // p.M ~ C#M, where C is p's class
      case TRef(Some(p), m, Nil) =>
        paths(scope).find(pc => pc._1 == p && typeMembers(pc._2, viaThis = false).exists(_._1 == m))
          .toList.map(pc => TProj(pc._2.topRef, m) -> Scope(None)) ++
          // this.M ~ M ~ C.this.M
          (p match {
            case PThis(_) => scope.cls.toList.flatMap(c => List(TRef(None, m, Nil) -> scope, TRef(Some(PThis(Some(c.name))), m, Nil) -> scope))
            case _ => Nil
          })
      // M ~ this.M (inside a class)
      case TRef(None, m, Nil) if scope.cls.isDefined && typeMembers(scope.cls.get, viaThis = true).exists(_._1 == m) =>
        List(TRef(Some(PThis(None)), m, Nil) -> scope)
      // p.type ~ the class of p (only <:)
      case TSingle(_) => cls.toList.map(c => c.topRef -> scope)
      case TRef(None, b, List(arg)) if variance.contains(b) =>
        variants(scope, arg, None).toList.map { case (a, s) => TRef(None, b, List(a)) -> s }
      case TWith(List(a, b), Nil) => List(TWith(List(b, a), Nil) -> scope)
      case _ => Nil
    }
    pickOpt(alternatives)
  }

  private def render(): List[Member] = {
    val lib = library.map { b =>
      ClassMem(ClassDef(ConcreteClass, b.name, List(TParam("A", variance(b.name))), Nil, None, Nil))
    }
    def classDef(c: Cls): ClassDef = ClassDef(
      c.kind, c.name, Nil, c.parentTps,
      if (c.selfs.isEmpty) None else Some(if (c.selfs.size == 1) TRef(None, c.selfs.head.name, Nil) else TWith(c.selfs.map(s => TRef(None, s.name, Nil)), Nil)),
      c.inner.toList.map(i => ClassMem(classDef(i))) ++ c.body.toList
    )
    for (t <- traits) t.parentTps = t.parents.map(p => TRef(None, p.name, Nil))
    val finalVals = topVals.toList.map { case (n, _, tp) => ValMem(n, tp) }
    lib ++ traits.map(t => ClassMem(classDef(t))) ++ composites.map(c => ClassMem(classDef(c))) ++ finalVals ++ topQueries
  }
}

object Gen {
  final case class Config(
    maxTraits: Int = 4,
    selfTypeChance: Double = 0.4,
    overrideChance: Double = 0.4,
    maxPathDepth: Int = 3,
    queriesPerProgram: Int = 8,
    pairsPerProgram: Int = 16,
    refinementWeight: Double = 0.6,
    builtinWeight: Double = 0.7,
  )
}
