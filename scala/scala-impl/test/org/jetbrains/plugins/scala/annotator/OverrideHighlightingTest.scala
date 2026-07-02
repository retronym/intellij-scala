package org.jetbrains.plugins.scala.annotator

class OverrideHighlightingTest extends ScalaHighlightingTestBase {
  import Message._

  def testScl13051(): Unit = {
    val code =
      s"""
         |trait Base {
         |  def foo: Int = 42
         |}
         |
         |class AClass extends Base {
         |  override def foo: String = "42"
         |}
       """.stripMargin
    assertMatches(errorsFromScalaCode(code)) {
      case Error(_, "Overriding type String does not conform to base type Int") :: Nil =>
    }
  }

  def testScl13051_1(): Unit = {
    val code =
      s"""
         |trait T1 {
         |  val foo: T1
         |}
         |trait T2 extends T1 {
         |  override val foo: T2
         |}
       """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // SCL-21947 (whittled from the scala/scala reflect Universe cake): the override's
  // param/return is `SymbolTable.this.RefinedType` (internal.Types is self: SymbolTable,
  // so `this` is the self type), while the inherited abstract member's, substituted,
  // is `Types.this.RefinedType`. These denote the same instance (SymbolTable extends
  // Types and Types's self type is SymbolTable), so ScThisType equivalence must treat
  // them as equal — otherwise "unapply overrides nothing". scalac accepts the code.
  def testSCL21947Cake(): Unit = {
    val code =
      """
        |package api {
        |  trait Types { self: Universe =>
        |    type Type
        |    type RefinedType
        |    abstract class RefinedTypeExtractor {
        |      def unapply(tpe: RefinedType): Type
        |    }
        |  }
        |  abstract class Universe extends Types
        |}
        |package internal {
        |  trait Types extends api.Types { self: SymbolTable =>
        |    abstract class Type
        |    abstract class RefinedType extends RefinedTypeExtractor {
        |      override def unapply(tpe: RefinedType): Type
        |    }
        |  }
        |  abstract class SymbolTable extends api.Universe with Types
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // SCL-21947, second shape (scala/scala internal.Types LazyType.complete): the
  // overridden member's path-dependent param `Symbol` comes from a SIBLING trait
  // reached via the self type. `complete` is concrete in `Type` and abstract-
  // overridden in `LazyType extends Type`; both params are `SymbolTable.this.Symbol`.
  // scalac accepts it; IntelliJ reported "complete overrides nothing".
  def testSCL21947Complete(): Unit = {
    val code =
      """
        |package internal {
        |  trait Symbols { self: SymbolTable => type Symbol }
        |  trait Types { self: SymbolTable =>
        |    abstract class Type {
        |      def complete(sym: Symbol): Unit = ()
        |    }
        |    abstract class LazyType extends Type {
        |      override def complete(sym: Symbol): Unit
        |    }
        |  }
        |  abstract class SymbolTable extends Symbols with Types
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // An abstract member re-abstracting a CONCRETE inherited member in a nested
  // class. scalac accepts it; IntelliJ reported "f overrides nothing" because the
  // member table keeps the concrete super as the slot's primary node, and plain
  // superSignatures (unlike superSignaturesIncludingSelfType) lacked the by-signature
  // fallback. Not path-dependent — the root of the LazyType.complete report above.
  def testReabstractNested(): Unit = {
    val code =
      """
        |trait Holder {
        |  abstract class A { def f(x: Int): Unit = () }
        |  abstract class B extends A { override def f(x: Int): Unit }
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // SCL-21947, third shape (scala/scala Infer.inferTypedPattern). The receiver
  // `typer` is the global `object typer extends analyzer.Typer`, reached via the
  // abstract `val global: Global`. Calling `typer.applyTypeToWildcards(pattp)`,
  // IntelliJ computes the param type as seen through that singleton receiver as
  // `global.analyzer.global.analyzer.global.Type` (never collapsing the
  // `analyzer.global: Global.this.type` singleton path back to `global`, and
  // re-applying the rewrite twice), then reports a false type mismatch against the
  // argument `global.Type`. scalac accepts it.
  def testSCL21947Inferencer(): Unit = {
    val code =
      """
        |trait Typers { self: Analyzer =>
        |  import global._
        |  abstract class Typer {
        |    def applyTypeToWildcards(tp: Type): Type = tp
        |  }
        |}
        |trait Infer { self: Analyzer =>
        |  import global._
        |  class Inferencer {
        |    def inferTypedPattern(pattp: Type): Type =
        |      typer.applyTypeToWildcards(pattp)
        |  }
        |}
        |trait Analyzer extends Typers with Infer {
        |  val global: Global
        |}
        |class Global {
        |  type Type
        |  lazy val analyzer = new { val global: Global.this.type = Global.this } with Analyzer
        |  object typer extends analyzer.Typer
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // asSeenFrom trace of the Inferencer shape — the IntelliJ counterpart of
  // scala/scala's `AsSeenFromTest.inferencer` LoggingAsSeenFromMap. Sets
  // `scala.asf.trace` so `ThisTypeSubstitution` prints its firings (`ASF fire`)
  // and `hasRecursiveThisType` blocks (`ASF GUARD`) to stderr.
  //
  // === What both engines compute ===
  //
  // The parameter type of `applyTypeToWildcards` (declared `Typers.this.global.Type`)
  // as seen through the receiver `typer`, reached via the abstract `val global`, i.e.
  // asSeenFrom prefix `Analyzer.this.global.typer.type`, class `Typer`.
  //
  // === scalac (AsSeenFromMap.thisTypeAsSeen) — one pass, owner-chain walk, stops ===
  //
  //   apply((tp): Typers.this.global.Type : MethodType)
  //     apply(Typers.this.global.Type : AbstractNoArgsTypeRef)
  //       apply(Typers.this.global.type : UniqueSingleType)
  //         apply(Typers.this.type : UniqueThisType)
  //           thisTypeAsSeen(Typers.this.type)
  //             matchesPrefixAndClass(pre=…global.typer.type, class=Typer)(candidate=Typers) = false
  //             matchesPrefixAndClass(pre=…global.analyzer.type, class=Typers)(candidate=Typers) = true
  //           = Analyzer.this.global.analyzer.type
  //         = Analyzer.this.global.analyzer.type
  //       = Analyzer.this.global.analyzer.global.type
  //     = Analyzer.this.global.analyzer.global.Type
  //   = (tp: Analyzer.this.global.analyzer.global.Type): Analyzer.this.global.analyzer.global.Type
  //   RESULT param type = Analyzer.this.global.analyzer.global.Type   // ONE analyzer.global. Done.
  //
  // `thisTypeAsSeen`'s loop climbs the symbol OWNER chain (`clazz -> clazz.owner`),
  // taking `(pre baseType clazz).prefix` at each step, and returns the moment
  // `matchesPrefixAndClass` succeeds. It never re-applies itself to its own output —
  // the recursion is over a finite, acyclic owner chain, so there is nothing to guard.
  //
  // === IntelliJ (ThisTypeSubstitution over the generic recursiveUpdate engine) ===
  //
  // Same indented idiom (thisTypeAsSeen opens a level; the owner-chain climb and the
  // isMoreNarrow narrowing decisions nest one deeper; `= result` closes; GUARD is the
  // brake that has no scalac analog). A clean firing mirrors scalac's owner walk:
  //
  //   thisTypeAsSeen(Typers.this.type)  [pre=…global.typer.type, seenFromClass=Typer]
  //     (pre baseType Typer).prefix = …global.analyzer.type  -> climb owner to Typers
  //     baseWalk: clazz=Typers terminal  -> narrow against pre=…global.analyzer.type
  //     isMoreNarrow(pre=…global.analyzer.type, Typers.this.type) = true  -> …global.analyzer.type
  //   = …global.analyzer.type
  //
  // The regrowth's true source is NOT the leaf/recursiveUpdate reassembly (swapping
  // ThisTypeSubstitution to a non-leaf SimpleUpdate changes nothing — measured). It is
  // that the owner-chain step `(pre baseType clazz)` is a LIVE recompute here, and
  // `BaseTypes.baseType` itself invokes asSeenFrom. The `baseType(...)` trace bracket
  // makes it patent — the nested firings are inside the base-type computation, and `pre`
  // is already growing there:
  //
  //   thisTypeAsSeen(Typers.this.type)  [pre=…global.typer.type, seenFromClass=Typer]
  //     baseType(pre=…global.typer.type, Typer)  [scalac: cached BaseTypeSeq lookup, no re-entry | IntelliJ: live recompute, re-enters asSeenFrom ↓]
  //       thisTypeAsSeen(Global.this.type)  [pre=…global.type, seenFromClass=Global]        ← RE-ENTRY
  //         ...
  //       thisTypeAsSeen(Global.this.type)  [pre=…global.analyzer.type]                     ← pre already grew
  //         ...
  //     = Some(…global.analyzer.Typer)                                                      ← baseType result
  //     (pre baseType Typer).prefix = …global.analyzer.type  -> climb owner to Typers
  //   = …global.analyzer.type
  //
  // Across the whole computation `pre` runs `analyzer.type` -> `analyzer.global.analyzer.type`
  // -> `analyzer.global.analyzer.global.analyzer.global.type` — the doubling the SCL-21947
  // fix commit describes — with GUARD (`hasRecursiveThisType = true -> BLOCK`) the only
  // thing halting it.
  //
  // === Why the guard exists here but not in scalac ===
  //
  // scalac's `thisTypeAsSeen` climbs the same owner chain, but its `(pre baseType clazz)` is
  // a CACHED `BaseTypeSeq` array lookup — inert data, it never re-enters the map. So the walk
  // is over a finite owner chain and returns once `matchesPrefixAndClass` succeeds; there is
  // nothing to guard. IntelliJ's `BaseTypes.baseType` is an UNCACHED recompute that re-invokes
  // asSeenFrom, so each level re-anchors a `this` inside a `pre` that already embeds `…analyzer`,
  // accreting another `analyzer.global`. `hasRecursiveThisType` (the `GUARD` lines) is the
  // emergency brake over that re-entry; remove it and this exact shape StackOverflows. The
  // principled fix that would retire it is a cached, closed-form `baseType` feeding the
  // owner-chain walk (à la scalac's `(pre baseType clazz).prefix` over a cached seq), so the
  // step is inert data and the walk cannot re-enter asSeenFrom.
  def testScratchInferencerTrace(): Unit = {
    System.setProperty("scala.asf.trace", "true")
    System.setProperty("scala.asf.origin", "analyzer\\.global") // one-shot stack at first doubled target
    System.setProperty("scala.asf.nocanon", "true") // canonicalize-at-mint is ON by default; disable so this
                                                    // test still demonstrates the un-canonicalized doubling
    try errorsFromScalaCode(
      """
        |trait Typers { self: Analyzer =>
        |  import global._
        |  abstract class Typer { def applyTypeToWildcards(tp: Type): Type = tp }
        |}
        |trait Infer { self: Analyzer =>
        |  import global._
        |  class Inferencer { def inferTypedPattern(pattp: Type): Type = typer.applyTypeToWildcards(pattp) }
        |}
        |trait Analyzer extends Typers with Infer { val global: Global }
        |class Global {
        |  type Type
        |  lazy val analyzer = new { val global: Global.this.type = Global.this } with Analyzer
        |  object typer extends analyzer.Typer
        |}
      """.stripMargin)
    finally {
      System.clearProperty("scala.asf.trace")
      System.clearProperty("scala.asf.origin")
      System.clearProperty("scala.asf.nocanon")
    }
  }

  // Same fixture under the (now default-on) canonicalize-at-mint: singleton
  // val-path spellings collapse where projections are re-minted, so no grown
  // targets are ever constructed (doubling eliminated; deepest spelling equals
  // scalac's one-hop answer). Traces the CANON collapses.
  def testScratchInferencerTraceCanon(): Unit = {
    System.setProperty("scala.asf.trace", "true")
    try {
      val errors = errorsFromScalaCode(
        """
          |trait Typers { self: Analyzer =>
          |  import global._
          |  abstract class Typer { def applyTypeToWildcards(tp: Type): Type = tp }
          |}
          |trait Infer { self: Analyzer =>
          |  import global._
          |  class Inferencer { def inferTypedPattern(pattp: Type): Type = typer.applyTypeToWildcards(pattp) }
          |}
          |trait Analyzer extends Typers with Infer { val global: Global }
          |class Global {
          |  type Type
          |  lazy val analyzer = new { val global: Global.this.type = Global.this } with Analyzer
          |  object typer extends analyzer.Typer
          |}
        """.stripMargin)
      System.err.println(s"CANON-ERRORS -> ${errors.map(_.toString)}")
    } finally {
      System.clearProperty("scala.asf.trace")
    }
  }

  // PROBE 3c RESULT: canonicalize ON + hasRecursiveThisType OFF still
  // StackOverflows (pure updateProjectionType/recursiveUpdateImpl descent of an
  // unboundedly deep type). So mint-point canonicalization subsumes only the
  // guard's SPELLING-growth role, not its TERMINATION role: even the canonical
  // spelling `Global.this.analyzer.type` contains `Global.this`, and with arm-1
  // off, rewriting `Global.this` leaves inside it self-embeds the target — the
  // 1-arg/null-seenFromClass isMoreNarrow walk returns the WHOLE target, whereas
  // scalac's lockstep (pre, clazz) walk only ever STRIPS prefixes (`.prefix` per
  // climb) and structurally cannot self-embed. The test records the outcome
  // instead of failing the suite.
  // EXPOSITIONAL: the REMNANT growth — what still grows under canonicalize-at-mint
  // (default-on), i.e. the channel the guard's termination role suppresses. The
  // structural guard is replaced by a pure depth cap (scala.asf.maxdepth) so the
  // self-embedding rounds can be WATCHED in the trace instead of blocked at first
  // contact. StackOverflow is caught and recorded in case the cap is insufficient.
  //
  // ═══ IntelliJ, one growth round (this test's trace; nesting depth ≈ 2 of 10) ═══
  //
  //   NEW #1d91d21d  target=Infer.this.global.analyzer.global.analyzer.type  seenFromClass=<null>
  //        at BaseProcessor.processTypeImpl:315 < ...       ◄── round N's input: a 2× spelling, recirculated
  //                                                         ◄── as a fresh 1-arg substitutor target by resolution
  //   GUARD#1d91d21d hasRecursiveThisType(Global.this, pre=…2×…) = false
  //   thisTypeAsSeen(Global.this.type)#1d91d21d  [pre=…analyzer.global.analyzer.type]
  //     isMoreNarrow(…2×…, Global.this.type) = false  -> climb enclosing to …analyzer.global.type
  //     isMoreNarrow(…analyzer.global.type, Global.this.type) = true
  //   = Infer.this.global.analyzer.global.type              ◄── the Global.this rewrite is BENIGN: the climb
  //                                                         ◄── strips the trailing .analyzer — walk working
  //   GUARD#1d91d21d hasRecursiveThisType(Infer.this, pre=…2×…) = false
  //                                                         ◄── ★ THE ADMISSION. In production arm-1 answers
  //                                                         ◄── TRUE: target is ROOTED at Infer.this and we
  //                                                         ◄── are about to rewrite Infer.this.
  //   thisTypeAsSeen(Infer.this.type)#1d91d21d  [pre=…analyzer.global.analyzer.type]
  //     isMoreNarrow(…2×…, Infer.this.type) = true  -> …2×…
  //                                                         ◄── ★ THE SELF-EMBEDDING REWRITE: the path's class
  //                                                         ◄── (Analyzer) extends Infer, so the WHOLE target
  //                                                         ◄── stands in for Infer.this — defensible by
  //                                                         ◄── subtyping, poison by structure: the target now
  //                                                         ◄── contains itself where its root was.
  //   = Infer.this.global.analyzer.global.analyzer.type
  //   NEW #78d1c3e8  target=Infer.this.global.analyzer.global.analyzer.global.type
  //        at BaseProcessor.processTypeImpl:315 < ...       ◄── ★ THE +1: resolution projects .global onto the
  //                                                         ◄── poisoned path and recirculates it as round
  //                                                         ◄── N+1's target: 2× -> 3×. Minted by resolution-
  //                                                         ◄── side ScProjectionType construction, NOT by
  //                                                         ◄── updateProjectionType — the canonicalize-at-mint
  //                                                         ◄── hook never sees it (4 CANON collapses in the
  //                                                         ◄── whole run, all at 1×). 60+ segments later:
  //                                                         ◄── StackOverflowError, from mere descent.
  //   Depth never nears the cap of 10 — growth is SEQUENTIAL recirculation (round N's
  //   output -> round N+1's input through fresh top-level substitutors), not nesting.
  //   No depth limit can bound it; arm-1 keys on the PRECONDITION (target rooted at the
  //   this-type being rewritten), which is why it is irreplaceable by depth or spelling
  //   heuristics.
  //
  // ═══ scalac, the same example (scala/scala AsSeenFromTest.remnantFixpoint) ═══
  //
  //   ===== ROUND 1: pre = Analyzer.this.global.type =====
  //   select .analyzer -> Analyzer.this.global.analyzer.type
  //   member 'global' info = Analyzer.this.global.type      ◄── the refinement decl was re-anchored ONCE, to
  //                                                         ◄── the selection prefix, by the cached copied-
  //                                                         ◄── refinement widen — memberType, materialized
  //   apply(Analyzer.this.global.type : NullaryMethodType)
  //     apply(Analyzer.this.global.type : UniqueSingleType)
  //       apply(Analyzer.this.type : UniqueThisType)
  //         thisTypeAsSeen(Analyzer.this.type)
  //           matchesPrefixAndClass(...)(candidate=trait Analyzer) = false
  //         = Analyzer.this.type                            ◄── nothing to do; info already canonical
  //   underlying(pre.analyzer.global) = Analyzer.this.global.type   ◄── == pre. NET GROWTH: 0
  //   ===== ROUND 2: pre = Analyzer.this.global.type =====  ◄── FIXPOINT — byte-identical to round 1
  //   ===== ROUND 3: pre = Analyzer.this.global.type =====  ◄── and again
  //
  //   POISON STEP: Infer.this asSeenFrom (pre=Analyzer.this.global.analyzer.type, clazz=Infer)
  //   = Analyzer.this.global.analyzer.type                  ◄── scalac performs the IDENTICAL root-to-path
  //                                                         ◄── rewrite — once. The divergence is entirely in
  //                                                         ◄── what happens next: scalac consumes the result;
  //                                                         ◄── IntelliJ recirculates it as a fresh target
  //                                                         ◄── whose root gets rewritten AGAIN.
  //   DEEP = Analyzer.this.global.(analyzer.global.)×3type  (built by hand)
  //   underlying(deep.analyzer.global) = deep               ◄── ONE layer stripped — exactly what selection
  //                                                         ◄── added. scalac does NOT eagerly collapse deep
  //                                                         ◄── to P0; it merely never grows a spelling.
  //
  // ═══ The invariant separating the engines ═══
  //
  //   scalac:    underlying(pre.analyzer.global) == pre      for ANY pre  -> net 0/round -> fixpoint, no guard
  //   IntelliJ:  next-target(pre.analyzer.global) == pre+1 layer          -> net +1/round -> guard or SOE
  //
  //   Round-trip NEUTRALITY, not canonicalization, is scalac's actual discipline: the
  //   copied-refinement mechanism bakes the once-re-anchored member info into the cached
  //   widened type, making `underlying` the exact inverse of selection. (Canonicalize-at-
  //   mint is thus MORE aggressive than scalac — semantically sound, but the faithful
  //   product-fix target is neutrality of designatorSingletonType: underlying(pre.member)
  //   anchored at pre — a cheaper contract to enforce and cache.)
  def testScratchRemnantGrowthTrace(): Unit = {
    System.setProperty("scala.asf.trace", "true")
    System.setProperty("scala.asf.maxdepth", "10")
    try {
      val errors = errorsFromScalaCode(
        """
          |trait Typers { self: Analyzer =>
          |  import global._
          |  abstract class Typer { def applyTypeToWildcards(tp: Type): Type = tp }
          |}
          |trait Infer { self: Analyzer =>
          |  import global._
          |  class Inferencer { def inferTypedPattern(pattp: Type): Type = typer.applyTypeToWildcards(pattp) }
          |}
          |trait Analyzer extends Typers with Infer { val global: Global }
          |class Global {
          |  type Type
          |  lazy val analyzer = new { val global: Global.this.type = Global.this } with Analyzer
          |  object typer extends analyzer.Typer
          |}
        """.stripMargin)
      System.err.println(s"REMNANT-ERRORS -> ${errors.map(_.toString)}")
    } catch {
      case _: StackOverflowError => System.err.println("REMNANT -> StackOverflowError (cap insufficient)")
    } finally {
      System.clearProperty("scala.asf.trace")
      System.clearProperty("scala.asf.maxdepth")
    }
  }

  // PROBE: leaf-only fusion contract ENFORCED (scala.asf.terminal — a rewriting
  // this-substitution's output is terminal for the rest of its fused chain) with
  // the structural guard OFF. If the fused pump is the growth mechanism, this
  // should complete where noguard alone StackOverflows. Also audits chains for
  // redundant (duplicate-target) this-substitutions.
  def testScratchTerminalNoGuard(): Unit = {
    System.setProperty("scala.asf.terminal", "true")
    System.setProperty("scala.asf.noguard", "true")
    System.setProperty("scala.asf.trace", "true")
    try {
      val errors = errorsFromScalaCode(
        """
          |trait Typers { self: Analyzer =>
          |  import global._
          |  abstract class Typer { def applyTypeToWildcards(tp: Type): Type = tp }
          |}
          |trait Infer { self: Analyzer =>
          |  import global._
          |  class Inferencer { def inferTypedPattern(pattp: Type): Type = typer.applyTypeToWildcards(pattp) }
          |}
          |trait Analyzer extends Typers with Infer { val global: Global }
          |class Global {
          |  type Type
          |  lazy val analyzer = new { val global: Global.this.type = Global.this } with Analyzer
          |  object typer extends analyzer.Typer
          |}
        """.stripMargin)
      System.err.println(s"TERMINAL-NOGUARD-ERRORS -> ${errors.map(_.toString)}")
    } catch {
      case _: StackOverflowError => System.err.println("TERMINAL-NOGUARD -> StackOverflowError")
    } finally {
      System.clearProperty("scala.asf.terminal")
      System.clearProperty("scala.asf.noguard")
      System.clearProperty("scala.asf.trace")
    }
  }

  def testScratchInferencerCanonNoGuard(): Unit = {
    System.setProperty("scala.asf.noguard", "true") // canonicalize-at-mint is default-on
    try {
      val errors = errorsFromScalaCode(
        """
          |trait Typers { self: Analyzer =>
          |  import global._
          |  abstract class Typer { def applyTypeToWildcards(tp: Type): Type = tp }
          |}
          |trait Infer { self: Analyzer =>
          |  import global._
          |  class Inferencer { def inferTypedPattern(pattp: Type): Type = typer.applyTypeToWildcards(pattp) }
          |}
          |trait Analyzer extends Typers with Infer { val global: Global }
          |class Global {
          |  type Type
          |  lazy val analyzer = new { val global: Global.this.type = Global.this } with Analyzer
          |  object typer extends analyzer.Typer
          |}
        """.stripMargin)
      System.err.println(s"CANON-NOGUARD-ERRORS -> ${errors.map(_.toString)}")
    } catch {
      case _: StackOverflowError =>
        System.err.println("CANON-NOGUARD -> StackOverflowError (guard's termination role NOT subsumed by canonicalization)")
    } finally {
      System.clearProperty("scala.asf.noguard")
    }
  }

  // SCL-21947, fourth shape: the singleton val-path `gen.global` (refined to
  // `Global.this.type`) again fails to collapse to `global`, but this time the
  // conformance crosses inheritance: `gen.global.Block <: Tree` (= `global.Tree`)
  // because `Block extends Tree`. The same-member case (`gen.global.Tree`) is fine;
  // the across-inheritance case was a false "type mismatch". scalac accepts it.
  def testSCL21947GenBlock(): Unit = {
    val code =
      """
        |trait Gen { val global: Global }
        |trait Typers { self: Analyzer =>
        |  import global._
        |  def x: Tree = (null: gen.global.Tree)
        |  def y: Tree = (null: gen.global.Block)
        |}
        |trait Analyzer extends Typers { val global: Global }
        |class Global {
        |  class Tree
        |  class Block extends Tree
        |  lazy val gen = new { val global: Global.this.type = Global.this } with Gen
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // SCL-21947, fifth shape: like GenBlock, but the Block-typed value comes from a
  // METHOD return (`gen.blk: Block`, as-seen-from `gen`) via an intermediate val,
  // not a written `gen.global.Block`. The asSeenFrom-computed type must still
  // collapse `gen.global` to `global` for `temp <: Tree` to hold. scalac accepts it.
  def testSCL21947GenBlkMethod(): Unit = {
    val code =
      """
        |abstract class TreeGen {
        |  val global: SymbolTable
        |  import global._
        |  def blk: Block = null
        |}
        |abstract class SymbolTable {
        |  class Tree
        |  class Block extends Tree
        |  val gen = new TreeGen { val global: SymbolTable.this.type = SymbolTable.this }
        |}
        |trait Analyzer { val global: SymbolTable }
        |trait Typers { self: Analyzer =>
        |  import global._
        |  val temp = gen.blk
        |  val tree: Tree = temp
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // SCL-21947, sixth shape (scala/scala Typers + Global's `override object gen`):
  // `gen` is a `val` in SymbolTable, overridden as an `object` in Global with the
  // early-init `val global: Global.this.type`. Reaching `gen.blk` via `import
  // global._` binds `gen` to the SymbolTable val (whose refinement is relative to
  // SymbolTable), so the prefix `gen.global` does not collapse to `global` and
  // `temp <: Tree` was a false mismatch. scalac accepts it.
  def testSCL21947GenObject(): Unit = {
    val code =
      """
        |trait IGen {
        |  val global: SymbolTable
        |  import global._
        |  def blk: Block = null
        |}
        |trait NscGen extends IGen { val global: Global }
        |class SymbolTable {
        |  class Tree
        |  class Block extends Tree
        |  val gen = new IGen { val global: SymbolTable.this.type = SymbolTable.this }
        |}
        |class Global extends SymbolTable {
        |  override object gen extends { val global: Global.this.type = Global.this } with NscGen
        |}
        |trait Analyzer { val global: Global }
        |trait Typers { self: Analyzer =>
        |  import global._
        |  val temp = gen.blk
        |  val tree: Tree = temp
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // SCL-21947, seventh shape (scala/scala nsc ast/TreeGen): the failing conformance
  // sits INSIDE the nsc `TreeGen` itself (which extends `reflect.internal.TreeGen`),
  // so `this` is a TreeGen and `gen` (= `global.gen`, an override-object TreeGen),
  // `Tree`, and `mkAttributedIdent` all come through that TreeGen's `import global._`.
  // `gen.mkAttributedIdent(null): gen.global.RefTree` must conform to substituteThis's
  // `to: Tree` (RefTree extends SymTree extends Tree). IntelliJ reported "Required:
  // Tree, Found: RefTree". scalac accepts it. (The same call in a non-TreeGen context
  // is already green — this context computes the arg type differently.)
  //
  // ROOT CAUSE (fixed): `gen.mkAttributedIdent(null)` resolved to the unanchored
  // `gen.this.global.gen.global.RefTree` (base `This(gen)`) instead of the receiver
  // path `NscGen.this.global.gen.global.RefTree`. The re-anchoring substitution
  // `ScSubstitutor(NscGen.this.global.gen)` was being suppressed by
  // `ThisTypeSubstitution.hasRecursiveThisType`: the target path contains
  // `This(NscGen)` and `object gen` inherits `NscGen`, so the inheritor-direction
  // guard (added for SCL-18532, a runaway-recursion perf fix on the nsc cake) wrongly
  // treated re-anchoring `This(gen)` as recursive. Object this-types are terminal and
  // re-anchor exactly once, so that guard no longer fires for objects. With the base
  // anchored, the existing override-aware singleton collapse finishes the conformance.
  def testSCL21947TreeGen(): Unit = {
    val code =
      """
        |trait Trees { self: SymbolTable =>
        |  abstract class Tree { def substituteThis(clazz: AnyRef, to: Tree): Tree = null }
        |  abstract class SymTree extends Tree
        |  trait NameTree
        |  trait RefTree extends SymTree with NameTree
        |}
        |class SymbolTable extends Trees {
        |  val gen = new IGen { val global: SymbolTable.this.type = SymbolTable.this }
        |}
        |trait IGen {
        |  val global: SymbolTable
        |  import global._
        |  def mkAttributedIdent(sym: AnyRef): RefTree = null
        |}
        |trait NscGen extends IGen {
        |  val global: Global
        |  import global._
        |  def test(tree: Tree): Unit = {
        |    tree.substituteThis(null, gen.mkAttributedIdent(null))
        |  }
        |}
        |class Global extends SymbolTable {
        |  override object gen extends { val global: Global.this.type = Global.this } with NscGen
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // SCL-21947, eighth shape (scala/scala nsc BrowsingLoaders.enterIfNew): the
  // path-dependent param `Symbol` is bound through a SINGLETON ALIAS, not a self type.
  // `SymbolLoaders` declares `val symbolTable: SymbolTable` and `import symbolTable._`,
  // so the inherited abstract member's param is `symbolTable.Symbol`. The override (via
  // `import global._`) writes `global.Symbol`. They are the same type only because the
  // intermediate `GlobalSymbolLoaders` aliases `val symbolTable: global.type = global`,
  // which scalac collapses (`symbolTable.Symbol =:= global.Symbol`). scalac accepts it.
  //
  // ROOT CAUSE (fixed): override matching compares params via EQUIVALENCE, not
  // conformance. `ScProjectionType.equivInner` sees both projections share element
  // `Symbol`, so it recurses to the prefixes `symbolTable` =?= `global`. Its singleton
  // collapse, `checkDesignatorType`, took the prefix designator's RAW declared type
  // (`actualSubst(td.type())`) — but `td` is the *abstract* `SymbolLoaders.symbolTable:
  // SymbolTable` (the designator points to the declaration-site symbol; asSeenFrom does
  // not re-resolve member overrides inside a prefix). `SymbolTable` is not a singleton,
  // so the collapse bailed and equiv returned Left -> "overrides nothing". Fix: equivInner
  // now also consults the override-aware `designatorSingletonType`
  // (`ScProjectionType.overrideSingletonOf`, scalac's `pre.memberType`), which the
  // conformance side already used. Confirmed by a differential: declaring `symbolTable:
  // global.type` directly in `SymbolLoaders` (no override) was already green.
  def testSCL21947BrowsingLoaders(): Unit = {
    val code =
      """
        |trait SymbolTable {
        |  type Symbol <: Null
        |}
        |abstract class SymbolLoaders {
        |  val symbolTable: SymbolTable
        |  import symbolTable._
        |  protected def useSymbol(sym: Symbol): Unit
        |}
        |abstract class GlobalSymbolLoaders extends SymbolLoaders {
        |  val global: SymbolTable
        |  val symbolTable: global.type = global
        |}
        |abstract class BrowsingLoaders extends GlobalSymbolLoaders {
        |  val global: SymbolTable
        |  import global._
        |  override protected def useSymbol(sym: Symbol): Unit
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // SCL-21947, ninth shape (scala/scala reflect AnnotationInfos.Annotatable /
  // Symbols.Symbol). A generic cake trait `Annotatable[Self] { self: Self => def
  // foo(): Self }` lives in component `AnnotationInfos` (self: SymbolTable); the
  // sibling component `Symbols` declares `Symbol extends Annotatable[Symbol]`, which
  // overrides `foo` returning `this.type` (covariant over `Self` = `Symbol`, since
  // `Symbol.this.type <:< Symbol`). IntelliJ reported "foo overrides nothing".
  // scalac accepts it.
  def testSCL21947Annotatable(): Unit = {
    val code =
      """
        |trait AnnotationInfos { self: SymbolTable =>
        |  trait Annotatable[Self] { self: Self =>
        |    def foo(): Self
        |  }
        |}
        |trait Symbols { self: SymbolTable =>
        |  abstract class Symbol extends Annotatable[Symbol] {
        |    override def foo(): this.type = this
        |  }
        |}
        |trait SymbolTable extends AnnotationInfos with Symbols
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // SCL-21947, tenth shape (scala/scala nsc MutableSettings): a refinement placed on
  // the UPPER BOUND of an abstract type member, refining a member (`type T`) that the
  // refined component inherits TRANSITIVELY.
  //
  //   type Setting        <: SettingValue
  //   type BooleanSetting <: Setting { type T = Boolean }
  //
  // `def value: T` lives in `SettingValue` (where `T` is abstract, from
  // `AbsSettingValue`). Selecting `.value` on a `BooleanSetting` must read the bound's
  // `{ type T = Boolean }`, so its type is `BooleanSetting#T` (=:= Boolean) and the
  // assignment to `Boolean` holds. IntelliJ instead left the prefix as the raw
  // `SettingValue.this`, yielding the abstract `SettingValue.this.T` — "Expression of
  // type SettingValue.this.T doesn't conform to expected type Boolean". scalac accepts.
  //
  // ROOT CAUSE (fixed): re-anchoring `SettingValue.this` onto the receiver goes through
  // `ThisTypeSubstitution`, whose `hasSameOrInheritor` scans the compound's components
  // for `SettingValue`. It widened a component that is a TYPE PARAMETER to its bound but
  // not one that is an abstract TYPE ALIAS (`type Setting <: SettingValue`), so it never
  // saw `SettingValue` under `Setting`'s bound and skipped the substitution. The
  // `BooleanSetting1` form (`Setting with SettingValue { type T = Boolean }`) was already
  // green because `SettingValue` is then a direct (class) component.
  def testSCL21947MutableSettings(): Unit = {
    val code =
      """
        |abstract class AbsSettings {
        |  class AbsSettingValue { type T }
        |  trait SettingValue extends AbsSettingValue { def value: T }
        |}
        |abstract class MutableSettings extends AbsSettings {
        |  type Setting <: SettingValue
        |  type BooleanSetting  <: Setting {type T = Boolean}
        |  type BooleanSetting1 <: Setting with SettingValue {type T = Boolean}
        |  val viaBound:    Boolean = (??? : BooleanSetting).value
        |  val viaCompound: Boolean = (??? : BooleanSetting1).value
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // SCL-21947, eleventh shape (scala/scala nsc ExplicitOuter.OuterPathTransformer):
  // selecting an inner class through an OBJECT member reached on a stable val path.
  //
  //   class C2 extends global.explicitOuter.OuterPathTransformer(null: global.analyzer.Typer)
  //
  // `OuterPathTransformer`'s ctor param is `analyzer.Typer` = `ExplicitOuter.this.global.
  // analyzer.Typer`. As-seen-from the receiver `global.explicitOuter`, that must become
  // `global.explicitOuter.global.analyzer.Typer` and then collapse (`explicitOuter.global
  // =:= global`) to `global.analyzer.Typer` — matching the argument. scalac accepts it.
  //
  // ROOT CAUSE (fixed): resolving the qualifier `global` (a stable val) recorded its
  // `fromType` as the WIDENED declared type `Global`, not the singleton path
  // `Repro.this.global.type`. So the next selection — the OBJECT `explicitOuter` — was
  // projected as `Global#explicitOuter` (dropping the instance prefix) in
  // `ScStableCodeReferenceImpl.processQualifierResolveResult`. The ctor's substitutor then
  // re-anchored `ExplicitOuter.this` onto `Global#explicitOuter`, yielding the un-collapsible
  // `Global#explicitOuter.global.analyzer.Typer` -> false "type mismatch". Fix: for a stable
  // qualifier, record the singleton path as `fromType` (member lookup still runs over the
  // widened type), so the object selection keeps the path and asSeenFrom collapses it.
  def testSCL21947OuterPathTransformer(): Unit = {
    val code =
      """
        |trait Symbols { self: SymbolTable =>
        |  class Symbol
        |}
        |trait Trees { self: SymbolTable =>
        |  abstract class AstTransformer {
        |    def currentClass: Symbol = ???
        |  }
        |}
        |abstract class SymbolTable extends Symbols with Trees
        |trait Typers { self: Analyzer =>
        |  class Typer
        |}
        |trait Analyzer extends Typers {
        |  val global: Global
        |}
        |trait TypingTransformers {
        |  val global: Global
        |  import global._
        |  protected def newRootLocalTyper(unit: CompilationUnit): global.analyzer.Typer = ???
        |  abstract class TypingTransformer(initLocalTyper: global.analyzer.Typer) extends global.AstTransformer {
        |    def this(unit: CompilationUnit) = this(newRootLocalTyper(unit))
        |  }
        |}
        |trait ExplicitOuter extends TypingTransformers {
        |  import global._
        |  abstract class OuterPathTransformer(initLocalTyper: analyzer.Typer) extends TypingTransformer(initLocalTyper)
        |}
        |abstract class Global extends SymbolTable {
        |  class CompilationUnit
        |  lazy val analyzer = new { val global: Global.this.type = Global.this } with Analyzer
        |  object explicitOuter extends { val global: Global.this.type = Global.this } with ExplicitOuter
        |}
        |abstract class SubComponent {
        |  val global: Global
        |}
        |abstract class Repro extends SubComponent {
        |  abstract class C2
        |    extends global.explicitOuter.OuterPathTransformer(null: global.analyzer.Typer)
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // SCL-21947, twelfth shape: abstract type member `type Symbol >: Null` overridden
  // by `class Symbol { def foo = 42 }`. Selecting `.foo` on `currentClass` (whose
  // result type is the abstract `Symbol`) must resolve through the overriding class
  // member, mirroring scalac's `pre.memberType(sym)`.
  def testSCL21947AbstractTypeMemberOverriddenByClass(): Unit = {
    val code =
      """
        |trait Symbols { self: SymbolTable =>
        |  class Symbol { def foo = 42 }
        |}
        |trait ApiUniverse extends ApiTrees {
        |  type Symbol >: Null
        |}
        |trait ApiTrees { self: ApiUniverse =>
        |  abstract class ApiTransformer {
        |    def currentClass: Symbol = ???
        |  }
        |}
        |trait Trees { self: SymbolTable =>
        |  abstract class AstTransformer extends ApiTransformer
        |}
        |abstract class SymbolTable extends Symbols with Trees with ApiUniverse
        |trait Typers { self: Analyzer =>
        |  class Typer
        |}
        |trait Analyzer extends Typers {
        |  val global: Global
        |}
        |trait TypingTransformers {
        |  val global: Global
        |  import global._
        |  protected def newRootLocalTyper(unit: CompilationUnit): global.analyzer.Typer = ???
        |  abstract class TypingTransformer(initLocalTyper: global.analyzer.Typer) extends global.AstTransformer {
        |    def this(unit: CompilationUnit) = this(newRootLocalTyper(unit))
        |  }
        |}
        |trait ExplicitOuter extends TypingTransformers {
        |  import global._
        |  abstract class OuterPathTransformer(initLocalTyper: analyzer.Typer) extends TypingTransformer(initLocalTyper)
        |}
        |abstract class Global extends SymbolTable {
        |  class CompilationUnit
        |  lazy val analyzer = new { val global: Global.this.type = Global.this } with Analyzer
        |  object explicitOuter extends { val global: Global.this.type = Global.this } with ExplicitOuter
        |}
        |abstract class SubComponent {
        |  val global: Global
        |}
        |abstract class Repro extends SubComponent with TypingTransformers {
        |  import global._
        |  abstract class C2
        |    extends global.explicitOuter.OuterPathTransformer(null: global.analyzer.Typer) {
        |    val x: Symbol = currentClass
        |    x.foo
        |    currentClass.foo
        |  }
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // SCL-21947: override of a method whose parameter/return types are abstract type
  // members (Symbol, Tree) from ApiUniverse, inherited through the cake pattern.
  // The override check must resolve the super method's signature types through the
  // correct prefix (scalac's asSeenFrom on memberType).
  def testSCL21947OverrideWithAbstractTypeMembers(): Unit = {
    val code =
      """
        |trait Symbols { self: SymbolTable =>
        |  class Symbol { def foo = 42 }
        |}
        |trait ApiUniverse extends ApiTrees {
        |  type Symbol >: Null
        |  type Tree >: Null
        |}
        |trait ApiTrees { self: ApiUniverse =>
        |  abstract class ApiTransformer {
        |    def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree]
        |  }
        |}
        |trait Trees { self: SymbolTable =>
        |  abstract class AstTransformer extends ApiTransformer {
        |    def m1(a: Symbol): Symbol
        |  }
        |}
        |abstract class SymbolTable extends Symbols with Trees with ApiUniverse
        |trait Typers { self: Analyzer =>
        |  class Typer
        |}
        |trait Analyzer extends Typers {
        |  val global: Global
        |}
        |trait TypingTransformers {
        |  val global: Global
        |  import global._
        |  protected def newRootLocalTyper(unit: CompilationUnit): global.analyzer.Typer = ???
        |  abstract class TypingTransformer(initLocalTyper: global.analyzer.Typer) extends global.AstTransformer {
        |    def this(unit: CompilationUnit) = this(newRootLocalTyper(unit))
        |  }
        |}
        |trait ExplicitOuter extends TypingTransformers {
        |  import global._
        |  abstract class OuterPathTransformer(initLocalTyper: analyzer.Typer) extends TypingTransformer(initLocalTyper)
        |}
        |abstract class Global extends SymbolTable {
        |  class CompilationUnit
        |  lazy val analyzer = new { val global: Global.this.type = Global.this } with Analyzer
        |  object explicitOuter extends { val global: Global.this.type = Global.this } with ExplicitOuter
        |}
        |abstract class SubComponent {
        |  val global: Global
        |}
        |abstract class Repro extends SubComponent with TypingTransformers {
        |  import global._
        |  abstract class C2
        |    extends global.explicitOuter.OuterPathTransformer(null: global.analyzer.Typer) {
        |    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = stats
        |  }
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // All-in-one cake path-dependent probe: the singleton `gen.global` is refined to
  // `SymbolTable.this.type`, so `gen.blk: gen.global.Block` must collapse to `Block`
  // (= `global.Block`, `Block <: Tree`) for each dimension below to type-check. scalac accepts
  // all. Exercises one tricky path-dependent type through: (A) conformance, (B) parameter
  // applicability, (C) type-parameter inference, (D) implicit-parameter resolution,
  // (E) implicit conversion / extension method, (F) override matching.
  def testCakePathDependentAllDimensions(): Unit = {
    val code =
      """
        |trait Trees { self: SymbolTable =>
        |  class Tree
        |  class Block extends Tree
        |}
        |abstract class SymbolTable extends Trees {
        |  val gen = new IGen { val global: SymbolTable.this.type = SymbolTable.this }
        |}
        |trait IGen {
        |  val global: SymbolTable
        |  import global._
        |  def blk: Block = null
        |}
        |trait Analyzer { val global: SymbolTable }
        |trait Typers { self: Analyzer =>
        |  import global._
        |
        |  // (A) conformance — gen.blk (: gen.global.Block) assigned to Tree
        |  val a: Tree = gen.blk
        |
        |  // (B) parameter applicability — gen.blk as an argument to a Tree parameter
        |  def takeTree(t: Tree): Unit = ()
        |  takeTree(gen.blk)
        |
        |  // (C) type-parameter inference — A inferred from gen.blk, result required as Tree
        |  def id[A](x: A): A = x
        |  val c: Tree = id(gen.blk)
        |
        |  // (D) implicit parameter — Show[Tree] satisfied from cake scope
        |  trait Show[T] { def show(t: T): String }
        |  implicit val showTree: Show[Tree] = new Show[Tree] { def show(t: Tree) = "" }
        |  def render[T](t: T)(implicit s: Show[T]): String = s.show(t)
        |  val d: String = render[Tree](gen.blk)
        |
        |  // (E) implicit conversion / extension method — gen.blk.pretty via treeOps: Tree => TreeOps
        |  class TreeOps(t: Tree) { def pretty: String = "" }
        |  implicit def treeOps(t: Tree): TreeOps = new TreeOps(t)
        |  val e: String = gen.blk.pretty
        |
        |  // (F) override matching — Tree-typed signature overridden through an anonymous class
        |  trait Transformer { def transform(t: Tree): Tree }
        |  val f: Transformer = new Transformer { override def transform(t: Tree): Tree = t }
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // PROBE (self-type this.type, implicit dimension): implicit search keys off the base types
  // of the target. `render(this)` infers T = AnimalBox.this.type, whose self-type contributes
  // base `Animal`; the needed `Pretty[Animal]` lives in `Animal`'s companion. scalac accepts.
  def testProbeSelfTypeThisImplicit(): Unit = {
    val code =
      """
        |trait Pretty[-T] { def s(t: T): String }
        |class Animal
        |object Animal { implicit val p: Pretty[Animal] = new Pretty[Animal] { def s(t: Animal) = "" } }
        |trait AnimalBox { self: Animal =>
        |  def render[T](t: T)(implicit p: Pretty[T]): String = p.s(t)
        |  val r: String = render(this)
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // Whittled from the scala/scala reflect cake (internal.Scopes#Scope +
  // runtime.SynchronizedOps#newScope): `class Scope protected[Scopes] ()` is instantiated as
  // `new Scope with SynchronizedScope` from `SynchronizedOps`, which is NOT a subclass of the
  // nested `Scope`. scalac accepts it because `new Scope with SynchronizedScope` defines an
  // anonymous SUBCLASS of `Scope`, and a protected constructor is reachable as the super-ctor
  // call from a subclass. IntelliJ reported a false "No constructor accessible from here".
  //
  // ROOT CAUSE (fixed): `ResolveUtils.checkProtected` skipped the enclosing `new` template
  // (jumping to the named outer class, which is not a subclass) whenever it had an empty body —
  // which wrongly lumped `new Scope() with X` in with the bare `new Scope()` direct
  // instantiation. The skip now also requires a single parent type, so the `with X` /`{}`
  // anonymous-subclass forms are recognized as the subclass that grants access.
  def testProtectedCtorAnonClass(): Unit = {
    val code =
      """
        |trait Scopes { self: SymbolTable =>
        |  class Scope protected[Scopes] ()
        |}
        |trait SymbolTable extends Scopes
        |trait T
        |trait SynchronizedOps extends SymbolTable {
        |  trait SynchronizedScope extends Scope
        |  def newScope:   Scope = new Scope() with SynchronizedScope // the original cake shape
        |  def viaMixin:   Scope = new Scope() with T                 // unrelated mixin is enough
        |  def viaBody:    Scope = new Scope() {}                     // body form (already green)
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  // Regression guard for the fix above: a *direct* `new Scope()` (single parent, no body) from a
  // non-subclass is genuinely inaccessible and must still be reported. scalac rejects it too.
  def testProtectedCtorDirectInstantiation(): Unit = {
    val code =
      """
        |trait Scopes { self: SymbolTable =>
        |  class Scope protected[Scopes] ()
        |}
        |trait SymbolTable extends Scopes
        |trait SynchronizedOps extends SymbolTable {
        |  def bad: Scope = new Scope()
        |}
      """.stripMargin
    assertMatches(errorsFromScalaCode(code)) {
      case Error(_, "No constructor accessible from here") :: Nil =>
    }
  }

  def testScl13051_2(): Unit = {
    val code =
      s"""
         |trait Base {
         |  def foo(x: Int): Int = 42
         |  def foo(x: String): String = "42"
         |}
         |class AClass extends Base {
         |  override def foo(x: Int): Int = 42
         |}
       """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  def testSCL13051_3(): Unit = {
    val code =
      s"""
         |trait Base[c] {
         |  def foo: c
         |}
         |
         |class AClass[a] extends Base[a]{
         |  override val foo: a = ???
         |}
       """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  def testSCL13051Setter(): Unit = {
    val code =
      s"""
         |abstract class A() {
         |  var x: Int
         |}
         |
         |abstract class B() extends A() {
         |  var xx: Int = 0;
         |  def x: Int = xx
         |  def x_=(y: Int) = xx = y;
         |}
       """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  def testSCL13051replaceDesignators(): Unit = {
    val code =
      """
        |trait Eater {
        |  type Food[T]
        |}
        |
        |trait Fruit {
        |  type Seed
        |}
        |
        |trait PipExtractor {
        |  def extract(a: Fruit): a.Seed
        |}
        |
        |trait LaserGuidedPipExtractor extends PipExtractor {
        |  def extract(f: Fruit): f.Seed
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  private def addType(over: String): String = {
    val split = over.split("=")
    split(0) + ": Int =" + split(1)
  }

  private def inheritWithStringToIntConversion(base: String, over: String) =
    s"""
        |class Test {
        |  implicit def s2i(s: String): Int = s.length
        |  trait Base { $base }
        |  trait Sub extends Base { $over }
        |  trait Sub2 extends Base { ${addType(over)} }
        |}
      """.stripMargin

  def testValInheritReturnType(): Unit = {
    assertNothing(errorsFromScalaCode(inheritWithStringToIntConversion("def foo: Int", "val foo = \"\"")))
  }

  def testValInheritReturnTypeParens(): Unit = {
    //TODO: this, for some reason, does not work in compiler
    assertMatches(errorsFromScalaCode(inheritWithStringToIntConversion("def foo(): Int", "val foo = \"\""))) {
      case Error(_, "Overriding type String does not conform to base type () => Int") :: Nil =>
    }
  }

  def testFunInheritReturnTypeParens(): Unit = {
    assertNothing(errorsFromScalaCode(inheritWithStringToIntConversion("def foo(): Int", "def foo = \"\"")))
  }

  def testFunInheritReturnType(): Unit = {
    assertNothing(errorsFromScalaCode(inheritWithStringToIntConversion("def foo: Int", "def foo = \"\"")))
  }

  def testParensFunInheritReturnType(): Unit = {
    assertNothing(errorsFromScalaCode(inheritWithStringToIntConversion("def foo: Int", "def foo() = \"\"")))
  }

  def testParensFunInheritReturnTypeParens(): Unit = {
    assertNothing(errorsFromScalaCode(inheritWithStringToIntConversion("def foo(): Int", "def foo() = \"\"")))
  }

  def testVarInheritReturnType(): Unit = {
    assertNothing(errorsFromScalaCode(inheritWithStringToIntConversion("def foo: Int", "var foo = \"\"")))
  }

  def testVarInheritReturnTypeParens(): Unit = {
    //TODO: this, for some reason, does not work in compiler
    assertMatches(errorsFromScalaCode(inheritWithStringToIntConversion("def foo(): Int", "var foo = \"\""))) {
      case Error(_, "Overriding type String does not conform to base type () => Int") :: Nil =>
    }
  }

  def testSCL14152(): Unit = {
    val code =
      """
        |sealed trait TagExpr
        |
        |sealed trait Composite extends TagExpr {
        |  def head: TagExpr
        |  def tail: Seq[TagExpr]
        |  def all: Seq[TagExpr] = head +: tail
        |}
        |
        |final case class And(head: TagExpr, tail: TagExpr*) extends Composite
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  def testSCL14922(): Unit = {
    val code =
      """
        |trait A {
        |  trait Internal
        |  val i = new Internal {}
        |}
        |trait B extends A {
        |  trait Internal extends super.Internal
        |  override val i = new Internal {}
        |}
        |trait C extends A {
        |  trait Internal extends super.Internal
        |  override val i = new Internal {}
        |}
        |trait D extends B with C {
        |  trait Internal extends super[B].Internal with super[C].Internal
        |  override val i = new Internal {}
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  def testSCL14707(): Unit = {
    val code =
      """
        |trait BaseComponent {
        |  trait BaseComponent {
        |    val abstractName: String
        |  }
        |}
        |
        |trait AbstractChildComponent extends BaseComponent {
        |  trait AbstractChildComponent extends AbstractChildComponent.super[BaseComponent].BaseComponent {
        |    def abstractMethod() : scala.Unit
        |  }
        |}
        |
        |trait ConcreteComponent extends AbstractChildComponent {
        |
        |  object ConcreteComponent extends AbstractChildComponent {
        |    override def abstractMethod(): Unit = ()
        |
        |    override val abstractName = "hello world"
        |  }
        |}
      """.stripMargin
    assertNothing(errorsFromScalaCode(code))
  }

  protected val SetterAndGetterTraitsCode =
    """trait Setter {
      |  def setValue(foo: String): Unit
      |}
      |
      |trait Getter {
      |  def getValue: String
      |}
      |
      |trait GetterWithSetter extends Getter with Setter
      |""".stripMargin

  // SCL-14462
  def testDontShowErrorForBeanPropertiesOverridingMethods(): Unit = {
    val code =
      s"""$SetterAndGetterTraitsCode
         |
         |import scala.beans.BeanProperty
         |
         |class A1 extends Getter { @BeanProperty var value = "foo" }
         |class B1 extends Setter { @BeanProperty var value = "foo" }
         |class C1 extends GetterWithSetter { @BeanProperty var value = "foo" }
         |
         |class A2(@BeanProperty val value: String) extends Getter
         |class B2(@BeanProperty var value: String) extends Setter
         |class C2(@BeanProperty var value: String) extends GetterWithSetter
         |""".stripMargin
    assertNoErrors(code)
  }

  def testShowErrorForBeanPropertiesOverridingMethodsWithTypeMissmatch(): Unit = {
    val code =
      s"""$SetterAndGetterTraitsCode
         |
         |import scala.beans.BeanProperty
         |
         |class A3 extends Getter { @BeanProperty var value: Int = 42 }
         |class B3 extends Setter { @BeanProperty var value: Int = 42 }
         |class C3 extends GetterWithSetter { @BeanProperty var value: Int = 42 }
         |
         |class A4(@BeanProperty val value: Int) extends Getter
         |class B4(@BeanProperty var value: Int) extends Setter
         |class C4(@BeanProperty var value: Int) extends GetterWithSetter
         |""".stripMargin
    assertErrors(code, Seq(
      Error("value", "Overriding type Int does not conform to base type String"),
      Error("class B3 extends Setter", "Class 'B3' must either be declared abstract or implement abstract member 'setValue(foo: String): Unit' in 'Setter'"),
      Error("class C3 extends GetterWithSetter", "Class 'C3' must either be declared abstract or implement abstract member 'setValue(foo: String): Unit' in 'Setter'"),
      Error("value", "Overriding type Int does not conform to base type String"),
      //
      Error("value", "Overriding type Int does not conform to base type String"),
      Error("class B4(@BeanProperty var value: Int) extends Setter", "Class 'B4' must either be declared abstract or implement abstract member 'setValue(foo: String): Unit' in 'Setter'"),
      Error("class C4(@BeanProperty var value: Int) extends GetterWithSetter", "Class 'C4' must either be declared abstract or implement abstract member 'setValue(foo: String): Unit' in 'Setter'"),
      Error("value", "Overriding type Int does not conform to base type String"),
    ): _*)
  }

  // Step 0 of FUSED-SUBST-SCALAC.md: capture the chain SCL-7043 needs, under
  // blanket terminal-output fusion (a rewriting this-substitution's output is
  // not processed by the remainder of its own chain). testSCL7043 is the one
  // suite counterexample to terminal-output curing the growth pump (591/592) —
  // this trace records WHICH this-substitution rewrite gets skipped that the
  // correct inferred type (Int, via the `T#Value` / Enumeration overload) needs.
  def testScratchSCL7043Trace(): Unit = {
    System.setProperty("scala.asf.trace", "true")
    System.setProperty("scala.asf.terminal", "true")
    try {
      val errors = errorsFromScalaCode(
        """
          |abstract class C[T] {
          |  def lee : T
          |}
          |
          |class CE[T <: Enumeration](val enum: T) extends C[T#Value] {
          |  def foo(t: T#Value) = 1
          |  def foo(s: String) = "text"
          |
          |  foo(enum.values.toList(0))
          |  def lee = enum.values.toList(0)
          |}
        """.stripMargin)
      System.err.println(s"SCL7043-ERRORS -> ${errors.map(_.toString)}")
    } finally {
      System.clearProperty("scala.asf.trace")
      System.clearProperty("scala.asf.terminal")
    }
  }

  // Same fixture, production behaviour (guard on, terminal off) — to compare
  // against testScratchSCL7043Trace and see what the GUARD does with the
  // `[update 1/2] Enumeration.this.type -> CE.this.enum.type` / `[update 2/2]
  // target=CE.this.enum.type sfc=CE` chain that terminal-output breaks.
  def testScratchSCL7043TraceProd(): Unit = {
    System.setProperty("scala.asf.trace", "true")
    try {
      val errors = errorsFromScalaCode(
        """
          |abstract class C[T] {
          |  def lee : T
          |}
          |
          |class CE[T <: Enumeration](val enum: T) extends C[T#Value] {
          |  def foo(t: T#Value) = 1
          |  def foo(s: String) = "text"
          |
          |  foo(enum.values.toList(0))
          |  def lee = enum.values.toList(0)
          |}
        """.stripMargin)
      System.err.println(s"SCL7043-PROD-ERRORS -> ${errors.map(_.toString)}")
    } finally {
      System.clearProperty("scala.asf.trace")
    }
  }

  // PROBE (-Dscala.asf.progress): the Progress postcondition guard REPLACES
  // hasRecursiveThisType entirely (the PF condition short-circuits the pre-scan
  // when progress is on). nocanon makes this the strongest form of the claim:
  // even with un-canonicalized doubling spellings recirculating, the postcondition
  // alone must terminate the pump. Without progress, guard-off + nocanon SOEs.
  def testScratchProgressPump(): Unit = {
    System.setProperty("scala.asf.progress", "true")
    System.setProperty("scala.asf.nocanon", "true")
    try {
      val errors = errorsFromScalaCode(
        """
          |trait Typers { self: Analyzer =>
          |  import global._
          |  abstract class Typer { def applyTypeToWildcards(tp: Type): Type = tp }
          |}
          |trait Infer { self: Analyzer =>
          |  import global._
          |  class Inferencer { def inferTypedPattern(pattp: Type): Type = typer.applyTypeToWildcards(pattp) }
          |}
          |trait Analyzer extends Typers with Infer { val global: Global }
          |class Global {
          |  type Type
          |  lazy val analyzer = new { val global: Global.this.type = Global.this } with Analyzer
          |  object typer extends analyzer.Typer
          |}
        """.stripMargin)
      System.err.println(s"PROGRESS-PUMP-ERRORS -> ${errors.map(_.toString)}")
      assertNothing(errors)
    } catch {
      case soe: StackOverflowError =>
        System.err.println("PROGRESS-PUMP -> StackOverflowError")
        throw soe
    } finally {
      System.clearProperty("scala.asf.progress")
      System.clearProperty("scala.asf.nocanon")
    }
  }

  // Progress mode on the terminal-probe's one counterexample: the sequential
  // re-anchor SCL-7043 needs must still be admitted (CE aggregates an Enumeration,
  // does not inherit one — the Progress inheritance test is false on its root).
  def testScratchSCL7043Progress(): Unit = {
    System.setProperty("scala.asf.progress", "true")
    try {
      val errors = errorsFromScalaCode(
        """
          |abstract class C[T] {
          |  def lee : T
          |}
          |
          |class CE[T <: Enumeration](val enum: T) extends C[T#Value] {
          |  def foo(t: T#Value) = 1
          |  def foo(s: String) = "text"
          |
          |  foo(enum.values.toList(0))
          |  def lee = enum.values.toList(0)
          |}
        """.stripMargin)
      System.err.println(s"SCL7043-PROGRESS-ERRORS -> ${errors.map(_.toString)}")
      assertNothing(errors)
    } finally {
      System.clearProperty("scala.asf.progress")
    }
  }

  // SCL7008: the one divergence when Progress replaces the guard suite-wide —
  // inferred spelling flips NM.this.Name -> F.this.Name (same instance via the
  // self-type; the golden pins scalac's NM.this spelling). Trace which firing
  // the old guard blocked that Progress admits.
  def testScratchSCL7008Trace(): Unit = {
    System.setProperty("scala.asf.trace", "true")
    try {
      val errors = errorsFromScalaCode(
        """
          |trait SCL7008 {
          |  trait N { self: F =>
          |    trait Name
          |  }
          |  trait SN { self: F =>
          |    object nme extends Z {
          |
          |    }
          |  }
          |  class F extends N with SN with NM
          |
          |  trait NM { self: F =>
          |    trait NMC
          |    trait Z extends NMC { self: nme.type =>
          |      def one(name: Name): Name = null
          |      def two(name: Name) = one(name)
          |    }
          |  }
          |}
        """.stripMargin)
      System.err.println(s"SCL7008-ERRORS -> ${errors.map(_.toString)}")
    } finally {
      System.clearProperty("scala.asf.trace")
    }
  }
}
