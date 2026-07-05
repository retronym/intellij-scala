# RESOLVED: the this-anchor loss was in `ThisTypeSubstitution.doUpdateThisTypeFromClass`, NOT `BaseTypes`

> **Status: FIXED (2026-07-05).** The original diagnosis in this doc (below the line)
> was WRONG — it blamed `BaseTypes.baseType` for dropping the anchor. The real bug was a
> missing branch in `ThisTypeSubstitution.doUpdateThisTypeFromClass`. Kept below for the
> repro and the (falsified) hypothesis trail.

## What was actually wrong

scalac's `AsSeenFromMap.toPrefix` returns `pre` at its FIRST non-skip step whenever
`(sym isNonBottomSubClass clazz) && (pre.widen.typeSymbol isNonBottomSubClass sym)` —
where `sym` is the this-type's own class and `clazz` is the member's owner. It tests
this at **every** step, BEFORE ever consulting `pre baseType clazz`.

For the repro's key firing (`thisTypeAsSeen(SymbolTable.this) [pre=Global.this.type,
seenFromClass=Definitions]`): `SymbolTable <: Definitions` ✓ and `Global <: SymbolTable`
✓, so scalac short-circuits to `Global.this.Type`. It NEVER calls `baseType`.

IntelliJ's `doUpdateThisTypeFromClass` had no analog of that first branch — it only
tested the exact terminal `clazz == thisTp.element`. With `clazz = Definitions ≠
SymbolTable` it dove into the `baseType` climb, walked `Definitions`'s owner chain up to
the enclosing `object repro`, and fell off as `UNMATCHED`, keeping the raw
`SymbolTable.this.Type`.

`baseType(Global.this.type, Definitions) = repro.this.Definitions` is **scalac-correct**
(`Global` does not inherit through `repro`, so the prefix stays `repro`); scalac simply
never consults it here. Fixing `BaseTypes` could not have fixed the FP: even a
`Global.this`-prefixed base type still leaves the recursion climbing to
`clazz.containingClass = repro` and failing `anchoredNarrowAdmitted`. Confirmed by the
trace.

## The fix

Added scalac's first-branch check to `doUpdateThisTypeFromClass`, before the baseType
climb:

```scala
else if (isInheritorDeep(thisTp.element, clazz) && isMoreNarrow(target, thisTp, Set.empty)) {
  doUpdateThisType(thisTp, target)   // narrows to `pre`, consumes (first-match)
}
```

- `isInheritorDeep(thisTp.element, clazz)` = scalac's `sym isNonBottomSubClass clazz`
  (PROPER subclass; the exact `==` case is still handled by the terminal branch below).
- `isMoreNarrow(target, thisTp, Set.empty)` = scalac's `pre.widen.typeSymbol
  isNonBottomSubClass sym`.

**The `thisTp.element <: clazz` guard is the discriminator against the cross-symbol
pump.** In the pump the this-class is NOT a subclass of the cursor (`Infer` is not a
subclass of `Typer`), so this branch is skipped and ANCHOR DISCIPLINE still blocks the
poison narrow. Without the guard, `isMoreNarrow` alone would fire on
`Global.this.analyzer.type` vs `Infer.this` (Analyzer inherits Infer) and re-open the pump.

Validated: full oracle 599/599, 0 failures; `testScratchSkeletorCakeCrossSymbolPumpMinimal`
golden unchanged (exactly one re-anchor round); `testInferredMemberTypeAnchor` now a real
`case Nil =>` assertion. `testSkeletorDefinitionsAnyTpeAscription` stays green.

---

# (ORIGINAL, FALSIFIED) Handoff: `BaseTypes.baseType` drops the this-anchor across multi-hop nested supertypes

Goal: fix `BaseTypesIterator`/`BaseTypes.baseType` so that walking from a concrete
this-type root (`Global.this.type`) down through several supertype hops to a class
nested in an enclosing singleton/instance (`object repro { trait Definitions ... }`)
yields that class's type **prefixed by the original root** (`Global.this.Definitions`),
not its own static declaration-site prefix (`repro.Definitions`). Right now it yields
the latter, which is wrong whenever a member of that ancestor class returns a further
this-type (`SymbolTable.this.Type`) that itself needs re-anchoring against the same root.

This is a **pre-existing** bug, unrelated to the `ANCHORLESS-ELIMINATION.md` /
`FUSED-SUBST-SCALAC.md` anchor-discipline work landed today — confirmed by reverting
today's `BaseTypes.scala` change (the `DesignatorOwner` singleton-widen case) and
observing the false positive persists identically. Read those docs for the surrounding
context (`ThisTypeSubstitution`'s anchor discipline, PROGRESS/CONSUMED, the TCK), but
treat this as an independent bug in a different piece of the machinery: `BaseTypes.scala`
itself, not `ThisTypeSubstitution`.

> NOTE (2026-07-05): the two claims in this section are the ones that turned out to be
> false. `baseType`'s `repro`-prefix is correct; the bug is in `ThisTypeSubstitution`.

---

## 1. The repro (confirmed false positive, hand-minimized by Jason)

```scala
object repro {
  trait Definitions {
    self: SymbolTable =>
    def foo = NoSymbol.tpe
  }
  trait SymbolTable extends Definitions {
    abstract class Symbol { def tpe: Type = ??? }
    object NoSymbol extends Symbol
    abstract class Type
  }
  trait Global extends SymbolTable

  val g: Global = ???
  g.foo: g.Type // IntelliJ: "Cannot upcast SymbolTable.this.Type to repro.g.Type" (scalac: compiles fine)
}
```

Key ingredients, all load-bearing (each simplification attempt below this shape failed
to reproduce — see §5):

- `foo` has **no explicit return type annotation** — its type is INFERRED as
  `SymbolTable.this.Type` (from `NoSymbol.tpe`, where `NoSymbol: Symbol` and `Symbol`/
  `Type` are both declared directly in `SymbolTable`).
- `foo` is declared in `Definitions`, which self-types to `SymbolTable`
  (`self: SymbolTable =>`) — **not** the class the this-type belongs to.
- `SymbolTable extends Definitions` (plain inheritance, not the self-type edge) —
  i.e. `Definitions` is a genuine transitive ancestor of `Global`, TWO hops up
  (`Global -> SymbolTable -> Definitions`).
- Everything is nested inside `object repro { ... }` — a singleton. This is essential:
  the bug only manifests when an ancestor several hops up the chain is a member of an
  enclosing object/class, so its "natural" declared prefix differs from the root
  this-type's prefix.

A regression test exists (uncommitted) at `testScratchInferredMemberTypeAnchor` in
[OverrideHighlightingTest.scala](scala/scala-impl/test/org/jetbrains/plugins/scala/annotator/OverrideHighlightingTest.scala)
— currently asserts nothing (prints to stderr with `scala.asf.trace` on); flip it into a
real assertion once the fix lands (`assertMatches(errors) { case Nil => }`).

A larger, real-world-derived (skeletor-extracted from the actual `~/code/scala`
compiled classes) fixture is at
[scala/scala-impl/testdata/annotator/anyTpeSkeletonCake/skeleton.scala](scala/scala-impl/testdata/annotator/anyTpeSkeletonCake/skeleton.scala)
with test `testSkeletorDefinitionsAnyTpeAscription` — this is the ORIGINAL report
(`global.definitions.AnyTpe: global.Type` in the real `scala.tools.nsc.typechecker.Typers`)
but does **NOT** reproduce the false positive (that specific real-world shape happens to
route around the bug — see §5). Keep it as a green real-world regression test regardless;
it is unrelated to whether this bug is fixed.

---

## 2. Root cause

`BaseTypesIterator` ([BaseTypes.scala](scala/scala-impl/src/org/jetbrains/plugins/scala/lang/psi/types/BaseTypes.scala))
walks supertypes hop-by-hop via `enqueueSupersForClass`:

```scala
private def enqueueSupersForClass(c: PsiClass, substitutor: ScSubstitutor = ScSubstitutor.empty): Unit = {
  val superTypes = c match {
    case td: ScTemplateDefinition => td.superTypes
    case _ => c.getSuperTypes.toSeq.map(_.toScType())
  }
  superTypes.foreach { st =>
    val substed = substitutor(st)
    enqueue(substed)
  }
}
```

`td.superTypes` returns each supertype **as syntactically declared** — e.g. for
`trait SymbolTable extends Definitions`, the literal (unprefixed) `Definitions`
reference, which PSI resolves to `Definitions`'s own natural/declaration-site type
(`repro.Definitions`, since `Definitions` is a member of `object repro`). The
`substitutor` threaded through each hop is only ever built for **type-argument**
substitution (see `ClassType.unapply` binding `p.substitutor` for a
`ScParameterizedType`, or `ScSubstitutor.empty` for a bare designator) — it is NEVER a
**this-type** substitution derived from the original root.

So the very first hop already loses the root: `ScThisType(Global)` matches
`case ScThisType(clazz) => classType = clazz.\`type\`().toOption` (Global's own static
type, not "Global.this"-prefixed in any way that survives), and `enqueueSupersForClass`
propagates from there with an empty substitutor at every step. For simple, non-nested
ancestor classes this is harmless (a top-level class's "natural" type and its
this-anchored type coincide — there's only one possible prefix). It is **wrong** the
moment an ancestor several hops up is nested inside an enclosing object/class, because
then the "natural" prefix (the enclosing singleton) and the root's prefix
(`Global.this.type`) are different types, and scalac's real `baseTypeSeq`/`asSeenFrom`
would use the latter.

Traced concretely (via `-Dscala.asf.trace`, set with `System.setProperty` in the test
body — sbt `-D` is not forwarded to the forked test JVM) in `ThisTypeSubstitution`'s
`doUpdateThisTypeFromClass`, re-anchoring `foo`'s inferred `SymbolTable.this.Type`
against `pre = Global.this.type`, `clazz = Definitions` (foo's declaring class):

```
baseType(pre=Global.this.type, Definitions)
  = Some(repro.Definitions)        <- WRONG, should be Global.this.Definitions
  .prefix = repro.this.type        <- WRONG, should be Global.this.type
  -> climb owner to `repro`, cursor exhausted without reaching SymbolTable
  -> UNMATCHED, keep SymbolTable.this.Type   (never re-anchored to g.Type)
```

This is a **different bug class** from the cross-symbol pump / anchor discipline work:
ANCHOR DISCIPLINE is doing exactly the right thing here (correctly refusing to guess and
falling back to UNMATCHED) — the input it's given (`baseType`'s wrong prefix) is simply
already wrong before ANCHOR DISCIPLINE ever sees it. Confirmed this is pre-existing and
NOT introduced by today's `ThisTypeSubstitution`/`BaseTypes` changes: reverting the
`BaseTypes.scala` singleton-widen case (`case owner: DesignatorOwner => ...`, this
session's `186f2f75b4`) leaves the false positive byte-identical.

---

## 3. Why the real-world `AnyTpe` report doesn't reproduce it (yet)

The original report (`global.definitions.AnyTpe: global.Type` in real scala/scala
`Typers.scala`) traces back to this same family, but a skeletor-extracted, self-compile-
verified repro of that exact real shape does NOT trigger the bug (see
`testSkeletorDefinitionsAnyTpeAscription`). Several progressively-closer hand-built
repros also failed to reproduce it before the minimal one in §1 was found:

- Direct `object definitions { val AnyTpe: Type = ??? }` — no error.
- `abstract class DefinitionsClass { lazy val AnyTpe: Type = ??? }` nested in
  `Definitions`, `object definitions extends DefinitionsClass` — no error.
- Adding the `StandardTypes`/`DefinitionsApi` abstract-override chain — no error.
- Distinguishing `Universe` (StandardDefinitions's self-type) from `SymbolTable`
  (Definitions's self-type) as separate traits — no error.
- The full real-world skeletor extraction (actual compiled classes, self-compile
  verified) — no error.

So the real `Typers.scala`/`Definitions.scala` shape apparently threads enough EXPLICIT
type annotations along its actual path that `BaseTypes.baseType` never needs to fall
back to the buggy multi-hop walk for the specific expression reported. The bug is real
and demonstrated (§1), but the exact conditions under which it fires in the ACTUAL real
compiler sources are still unconfirmed. Possible next step: bisect between the working
skeletor repro and the failing minimal repro by re-adding pieces of the minimal repro's
shape (self-type on the class carrying the this-type, inferred vs. explicit member
type, nesting depth of the ancestor) one at a time into the skeletor fixture, OR
just keep hand-minimizing forward from more of the real Analyzer/Global chain now that
the minimal trigger conditions in §1 are known.

---

## 4. Fix directions (not yet attempted)

`enqueueSupersForClass` needs to thread a **this-type substitution**, not just a
type-argument substitution, as it descends. Candidate approaches:

- When enqueuing supertypes from a `ScThisType(clazz)` root, thread an
  `ScSubstitutor` mapping `clazz`'s this-type (and transitively each intermediate
  hop's this-type) to the ORIGINAL root this-type, composing hop-by-hop — i.e. make
  this loop look more like scalac's real `baseTypeSeq`, which re-derives every
  ancestor's type via `asSeenFrom` the whole way, not just at the top.
  This is likely the same substitution machinery already used by
  `ThisTypeSubstitution`/`SubtypeUpdater` — worth checking whether `BaseTypes.scala`
  could reuse `ScSubstitutor.declarationAnchor`-style anchoring, or a purpose-built
  `ThisTypeSubstitution(rootThisType, hopClass)` applied to each `superTypes` entry
  before enqueueing.
- Verify this doesn't reintroduce the cross-symbol pump: `BaseTypes.baseType` is
  itself called FROM `ThisTypeSubstitution.doUpdateThisTypeFromClass` (the "live
  recompute, re-enters asSeenFrom" comment already flags this re-entrancy), so any
  fix here needs to re-run the FULL oracle (§ below) plus specifically
  `testScratchSkeletorCakeCrossSymbolPumpMinimal` (golden: exactly ONE re-anchor
  round) to make sure the new prefix-threading doesn't recreate unbounded growth.
- Consider whether this should live in `enqueueSupersForClass` itself (thread a this-
  subst alongside the existing arg-subst) or as a post-processing step in
  `BaseTypes.baseType`/`baseTypeSeq` that re-derives the correct prefix for whatever
  `enqueueSupersForClass` already found, given the original root `t`. The latter is
  probably cheaper/safer (touches fewer call sites) but needs to handle the "found at
  class C after N hops" bookkeeping the current flat iterator doesn't track.

---

## 5. Validation oracle

Same as `ANCHORLESS-ELIMINATION.md` §5 — from the repo ROOT
(`/Users/jz/code/intellij-scala`), batched in one quoted arg:

```
sbt --client "packageArtifact; testOnly org.jetbrains.plugins.scala.annotator.OverrideHighlightingTest org.jetbrains.plugins.scala.lang.typeSystemTck.TypeSystemTckTest org.jetbrains.plugins.scala.lang.typeInference.generated.TypeInferenceBugs5Test org.jetbrains.plugins.scala.lang.typeInference.Singleton* org.jetbrains.plugins.scala.lang.typeConformance.generated.*"
```

Baseline: 586/586, same pinned TCK diffs as before (0 hard failures). Add
`testScratchInferredMemberTypeAnchor` (flip to a real assertion once fixed) and
`testScl21947*`/`testSCL7043`/`testSCL6549`/`testScratchSkeletorCakeCrossSymbolPumpMinimal`
to the must-watch list — this fix touches the same `BaseTypes.baseType` call that those
depend on.

## 6. Gotchas

- `sbt -D` flags are not forwarded to the forked test JVM — set trace properties with
  `System.setProperty`/`System.clearProperty` inside the test body.
- Use the ROOT project's `testOnly`, not `scala-impl/testOnly` (missing Maven plugin
  extension point otherwise). `packageArtifact` is enough after editing main sources.
- The skeletor tool (`~/code/minimal`, skill `skeletor`) is the fastest way to get a
  self-compile-verified repro straight from real, already-compiled sources
  (`~/code/scala/build/quick/classes/{library,reflect,compiler}` was the classpath used
  here) — much more reliable than hand-guessing a real cake's structure. Use
  `--stub-below scala.tools.` / `--stub-below scala.reflect.` rather than a blanket
  `--stub-below scala.` (the latter stubs core library symbols like `scala.Any`/
  `scala.Predef` and breaks self-compile with cyclic-inheritance errors).
