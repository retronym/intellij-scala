# scalac ↔ IntelliJ type-system parity index

This document indexes the four scalac type-system primitives that the
`scala-typesystem-tck` branch targets — **`asSeenFrom`**, **`baseTypeSeq`**
(`computeBaseTypeSeq`), **`packedType`**, and **`memberType`** — and relates each to
its IntelliJ-Scala counterpart. For each it records what scalac does, where IntelliJ
implements the same idea, what this branch changed, *why*, and the likely remnant gaps.

The unifying observation: nearly every SCL-21947 false positive on this branch is a
place where IntelliJ approximates one of these four primitives more coarsely than
scalac, and the cake pattern (scala/scala's own compiler/reflect sources) is the
stress test that exposes the gap. scalac canonicalises path-dependent types through a
small number of well-factored operations on `Type`; IntelliJ re-derives the same facts
across several PSI subsystems (substitutors, projection types, conformance,
resolution), so a single scalac invariant has to be restored in several independent
places.

## At-a-glance mapping

| scalac (`scala/scala`, `Types.scala`) | IntelliJ-Scala | Primary file(s) |
|---|---|---|
| `Type.asSeenFrom` / `AsSeenFromMap.thisTypeAsSeen` | `ThisTypeSubstitution`, `ScSubstitutor` | `types/recursiveUpdate/ThisTypeSubstitution.scala` |
| `Type.baseType(clazz)` / `computeBaseTypeSeq` / `baseTypeSeq` | `BaseTypes.baseType` / `baseTypeSeq` / `iterator` | `types/BaseTypes.scala` |
| `Typer.packedType` (type avoidance) | `ScBlock.avoidLocalSingletons` | `psi/api/expr/ScBlock.scala` |
| `Type.memberType(sym)` (override-aware member type, asSeenFrom prefix) | `ScProjectionType.designatorSingletonType` / `overrideSingletonOf`; `BaseProcessor` member resolution | `types/api/designator/ScProjectionType.scala`, `resolve/processor/BaseProcessor.scala` |
| `TypeComparers.isSameType` / `isSubType` (prefix following, `sameThisInstance`) | `ScalaConformance`, `ScProjectionType.equivInner`, `ScThisType.equivInner` | `types/ScalaConformance.scala`, `…/designator/{ScProjectionType,ScThisType}.scala` |

`memberType` is the keystone. The other three lean on it: `asSeenFrom` rewrites a
member's `this`-prefixes onto the access path, `baseType` feeds `asSeenFrom` the prefix
to rewrite onto, and conformance follows a singleton path's *underlying* — all of which
require resolving the member to its **most-specific override**, not its static
declaration. IntelliJ's pre-branch designators pointed at the declaration site, so the
override was invisible; restoring `memberType` semantics is what most of the diff does.

---

## 1. `asSeenFrom` — `ThisTypeSubstitution`

**scalac.** `asSeenFrom(pre, clazz)` rewrites every `C.this` inside a member's type to
the corresponding prefix as seen from `pre`. `AsSeenFromMap.thisTypeAsSeen` walks the
base-type chain (`pre.baseType(clazz).prefix`) and — crucially — *keeps walking up the
enclosing-class chain until the prefix is empty* rather than bailing the first time
`pre baseType clazz` has no contribution. Object `this`-types re-anchor exactly once to
a concrete path and are otherwise terminal.

**IntelliJ.** `ThisTypeSubstitution(target, seenFromClass)` is the corresponding map:
`doUpdateThisTypeWithSubstitutor` looks up `target`'s base type at the `this`-type's
containing class and substitutes the prefix. `isMoreNarrow` / `hasSameOrInheritor` /
`hasRecursiveThisType` are the guards that decide whether a given `this` may be
re-anchored.

**Changes on this branch** (`ThisTypeSubstitution.scala`):

- **Use the merged base type, not the first iterator hit.** The lookup now calls
  `BaseTypes.baseType(target, clazz)` (scalac's `pre baseType clazz`) instead of
  `BaseTypes.iterator(target).find(...)`, so multiple/merged same-class contributions
  resolve to one deterministic prefix — directly mirroring `thisTypeAsSeen`.
- **Don't bail when `clazz` isn't a base type of `target`.** When `clazz` is an inner
  class reached through a prefixed projection base, the `case _` now falls through to
  `doUpdateThisType(thisTp, target)` (re-anchor against `target` directly, still guarded
  by `isMoreNarrow`) instead of returning `thisTp` unchanged — mirroring scalac's
  "keep walking until the prefix is empty." (SCL-21947 OuterPathTransformer
  `currentClass` shape.)
- **Objects are terminal in the recursion guard.** `hasRecursiveThisType` now treats an
  `object`'s `this` as terminal (`case _: ScThisType if clazz.is[ScObject] => false`),
  so the broad inheritor-direction suppression — added for SCL-18532 as a runaway-
  recursion *perf* fix — no longer wrongly blocks re-anchoring an object member reached
  through a path (shape 7, `gen.this -> pre.gen`). Genuine self-recursion
  (`ScThisType(\`clazz\`)`) is still always guarded.
- **Widen abstract type-alias compound components to their bound.**
  `hasSameOrInheritor` gained a `case ta: ScTypeAlias` arm so a `this`-type whose class
  sits under an abstract member's upper bound (`type Setting <: SettingValue`) still
  re-anchors (MutableSettings `BooleanSetting` shape), mirroring the existing
  `ScTypeParam` branch and the top-level `isMoreNarrow` alias case.
- **Refuse the self-referential `C.this`-typed path collapse.** In `isMoreNarrow`, a
  stable path whose declared type *is* `thisTp` (`val global: Global.this.type`) no
  longer narrows `thisTp` — collapsing `Global.this := <path>` is circular and, reached
  as a projection prefix, folds `Global.this` onto the whole projection, which the
  resolver's recursion guard then prunes (dropping all members). Refusing lets the map
  keep walking to a concrete outer prefix.

**Remnant gaps.**

- `hasRecursiveThisType` still resolves the **trait self-type tension** coarsely with
  `isSameOrInheritor`. The object case is carved out; the general direction-aware fix
  (SCL-18532 ↔ SCL-3654, where `testSCL3654` was disabled by the original perf fix) is
  explicitly left as a `TODO`. Needs a perf re-confirmation against `Typers.scala`.
- The "keep walking" fallthrough re-anchors against `target` directly; it is sound only
  because `isMoreNarrow` gates it. It is not a faithful enclosing-class *walk* — a
  deeper nest than the OuterPathTransformer shape could still under-resolve.

---

## 2. `computeBaseTypeSeq` — `BaseTypes`

**scalac.** `computeBaseTypeSeq` builds the linearised `baseTypeSeq`: one entry per base
class, more-derived first. When a class is reached through several parents with
different arguments/prefixes, `mergePrefixAndArgs` combines them **per type-argument by
variance** (covariant → `glb` of args, contravariant → `lub`, invariant → kept), so
`Box[Dog]` and `Box[Cat]` reached two ways merge to `Box[Dog with Cat]`, not the
intersection `Box[Dog] with Box[Cat]`.

**IntelliJ.** `BaseTypes.iterator` enumerates supertypes; `get`/`reduce` collapse them
to one-per-class. Pre-branch `reduce` kept the single *most-specific arm* and discarded
the others.

**Changes on this branch** (`BaseTypes.scala`):

- **`baseType(t, clazz)`** — new, the analogue of `t baseType clazz`: collect all
  same-class contributions and merge them deterministically (used by
  `ThisTypeSubstitution`, see §1).
- **`mergeSameClass`** — implements `mergePrefixAndArgs`: per-argument variance merge
  for a `ScTypeParametersOwner`, `glb` fallback otherwise. Replaces "keep one arm."
- **`baseTypeSeq(t)`** — new ordered, deduplicated, merged sequence; ordering key is
  `(-baseClassCount, qualifiedName)`, a subtyping-consistent order (a subtype has a
  superset of its supertype's base classes).
- **`reduce`** rewritten to group-by-class and `mergeSameClass` each group.
- **`ScThisType` includes the self type.** `X.this` is known to satisfy `X`'s self
  type, so its base types now include the self type's bases (`ScCompoundType(classType,
  selfType)`). Without this, members reachable only via the self type were missed,
  defeating the `seenFromClass` walk in §1.
- Opaque-alias handling switched to `isEffectivelyOpaque`.

**Remnant gaps.**

- **Ordering is by base-class count + name, not symbol id.** scalac's exact tie-break
  among unrelated classes is by symbol id; this is deterministic but not identical.
  The TCK's `baseClasses` (linearization) dimension flags one residual mismatch — the
  `ScThisType` self-type case `04 AnimalBoxThis` — because PSI's linearization order vs
  `MixinNodes.linearization` still differs there. (Latent under resolution/conformance,
  which have their own `selfType` fallbacks; surfaced only by the differential TCK.)
- `glb`/`lub` are IntelliJ's, not scalac's, so the *merged* arguments can still differ
  for incomparable bounds even when the structure matches.

---

## 3. `packedType` — `ScBlock` type avoidance

**scalac.** `packedType(tree, owner)` "packs" a block's type so that no symbol owned by
the block escapes into the enclosing definition's inferred type — local classes and
local stable `val`s with singleton types are widened/existentially abstracted at the
block boundary.

**IntelliJ.** `ScBlock.innerType` infers the block's type from its last expression.
Pre-branch it returned the last expression's type verbatim.

**Changes on this branch** (`ScBlock.scala`):

- **`avoidLocalSingletons`** — new: widen any singleton type whose `element` is owned by
  this block (`PsiTreeUtil.isAncestor(this, owner.element, strict)`), iterating to a
  fixpoint because the widened type may itself mention another block-local singleton.
  This is the type-avoidance slice of `packedType` for the singleton case (a
  block-local stable `val`'s `X.type`, e.g. from a `this.type`-returning member, no
  longer escapes its scope).
- Expected-type extraction for SAM/function blocks now also threads through
  `ContextFunctionType` (`extractExpectedTypeParams`).

**Remnant gaps.**

- **Only singletons are avoided.** scalac's `packedType` also abstracts **block-local
  classes/objects** (existential over the local type symbol). A block whose last
  expression has a *local-class* type still leaks that class. This is the most
  significant scope-narrowing gap.
- The fixpoint is bounded (`guard < 8`); pathological nesting is truncated rather than
  abstracted.

---

## 4. `memberType` — override-aware projection underlying

**scalac.** `pre.memberType(sym)` resolves `sym`'s type *as a member of `pre`*: it
re-resolves overrides (the most-specific member of that name in `pre`'s linearization)
and `asSeenFrom`s the result onto `pre`. A singleton path's *underlying* follows the
**resolved** member, not the static symbol; `TypeComparers` then compares singletons by
chasing that underlying (`equalSymsAndPrefixes` / `chaseDealiasedUnderlying`).

**IntelliJ (the keystone gap).** A designator/projection points at the **declaration-
site** element. `ScProjectionType.designatorSingletonType` was computed from
`element` — the *abstract* declaration (`IGen#global: SymbolTable`) or `None` for an
object — so an override further down the prefix (`val global: Global.this.type` from an
anonymous-class refinement, or `override object gen`) was invisible, and the singleton
path never collapsed.

**Changes on this branch:**

- **`ScProjectionType.overrideSingletonOf` + override-aware `designatorSingletonType`**
  (`ScProjectionType.scala`, commit `c4a1861a44`): resolve the most-specific member of
  `element.name` on the prefix's class via
  `TypeDefinitionMembers.getSignatures(cls).forName(name)`, take its type, and
  `asSeenFrom` the prefix (`ScSubstitutor(projected)`). This *is* `pre.memberType`. The
  redesign retired several conformance-side collapse helpers
  (`memberOverrideSingleton`, a `conformsProjectedPrefix` retry).
- **`BaseProcessor` member resolution** (commit `46c14967ac`): when a projected element
  is an abstract type alias, consult the prefix's overriding class member of the same
  name *before* falling back to the abstract bound — `pre.memberType` at resolution
  time, not just in conformance.
- **Equivalence consults it too** (`ScProjectionType.equivInner`,
  `checkOverrideSingleton`): override matching compares param types by **equivalence**,
  not conformance, so the conformance-side fixes didn't apply. `equivInner` now falls
  back to the override-aware `designatorSingletonType` on both comparison directions
  (the BrowsingLoaders "overrides nothing" case). Also: a class realising an abstract
  type member (`class Symbol` overriding `type Symbol >: Null`) is treated as the same
  element as the alias (`ScTypeAliasDeclaration` ↔ `PsiClass`, same name, same
  linearization).
- **`StableCodeReference` records the singleton path as `fromType`** (commit
  `c0b7c22aa3`): a stable val/object qualifier now records its singleton path
  (`Repro.this.global.type`), not the widened declared type (`Global`), so the next
  object selection keeps the instance prefix and the val-path collapse can fire
  (OuterPathTransformer shape).

**Remnant gaps.**

- **`overrideSingletonOf` returns only when the override is singleton-like.** It is a
  *fallback*, deliberately additive (never turns a pass into a fail). A non-singleton
  but still more-specific override is not surfaced through this path — only the Base
  Processor change covers the resolution-time analogue, and only for abstract aliases.
- **Two parallel code paths.** scalac has one `memberType`; IntelliJ now restores its
  effect in *resolution* (`BaseProcessor`), *conformance* (`ScalaConformance`
  collapse), and *equivalence* (`ScProjectionType.equivInner`). These are kept in sync
  by hand. The collapse helpers in `ScalaConformance` (`collapseSingletonPath`,
  `projectionSingleton`, `compoundRefinementSingleton`) are a partial duplication of
  the `designatorSingletonType` logic; the intended end-state (per the project notes) is
  for the override-aware `designatorSingletonType` to fully subsume them.
- **`getSignatures(cls).forName(name)` on the hot projection path.** Flagged
  performance concern: this now runs during projection conformance/equivalence.

---

## 5. Cross-cutting: cake `this`-instance equality (`sameThisInstance`)

Not one of the four primitives, but the soundness hinge that several of the above lean
on. scalac canonicalises cake `this`-references (tied by self types — `trait Types {
self: SymbolTable => }`, `class SymbolTable extends Types`) to a single symbol via
`asSeenFrom` + override matching. IntelliJ does **not** canonicalise, so this branch
adds `ScThisType.sameThisInstance` (self-type mutual-subtyping) and threads it through
`ScThisType.equivInner` and the conformance `ThisVisitor` widenings (now
`thisProjections = true`, so a member projected off `this` keeps a `this`-typed prefix).

**Known divergence / open soundness question.** `sameThisInstance` equates the *literal*
cake `this`-pair unconditionally, where scalac only canonicalises *within one instance*.
So PSI returns `SymbolTable.this.T =:= Types.this.T` **true** while scalac says **false**
(`<:<`-true-but-`=:=`-false). This is correct where override-matching needs it
(guarded by `testSCL21947Cake`) but observably more lenient than scalac for a raw
`=:=`. It cannot be a green TCK corpus entry (equivalence/conformance are deferral-free
hard assertions) and is recorded as a "Known divergence" in the TCK README. This is the
concrete evidence behind the "soundness of `sameThisInstance`" open question.

---

## 6. Verification — the differential TCK

`TypeSystemTckTest` (commit `43eff73a02`) runs a progressive corpus (vendored from
`~/code/scala-type-system-tck`) against the PSI type system and diffs against
scalac-generated goldens across four dimensions, which map onto the primitives above:

| TCK dimension | Exercises |
|---|---|
| `conformance` (hard) / `equivalence` (hard) | `TypeComparers`, the §4/§5 collapse + `sameThisInstance` |
| `baseTypeSeq` membership | §2 `BaseTypes` |
| `baseClasses` linearization (order-sensitive) | §2 ordering vs `MixinNodes.linearization` |
| `memberType` term probes | §1 `asSeenFrom` / `ThisTypeSubstitution` + §4 |

Current state on this branch: conformance and equivalence fully match scalac; the
`memberType` shapes (entries 16, 17, 19–22) pass, confirming the `seenFromClass` walk
is effective; the one residual linearization mismatch is the `ScThisType` self-type case
(`04 AnimalBoxThis`, §2). Render-only seams (e.g. PSI keeps an un-reduced `X#T`
projection where scalac shows the reduced `Boolean`) are pinned in a `Deferred` registry
rather than asserted.

---

## 7. Consolidated remnant-gap list

1. **`packedType` covers only singletons** — block-local *classes/objects* still escape
   the block boundary (§3). Largest correctness gap.
2. **`baseTypeSeq` tie-break + `ScThisType` linearization** — ordering by base-class
   count/name not symbol id; one residual TCK linearization mismatch (§2).
3. **Trait self-type recursion guard** — `hasRecursiveThisType` still coarse for traits;
   SCL-18532 ↔ SCL-3654 tension unresolved, needs perf re-confirmation (§1).
4. **`memberType` restored in three hand-synced places** — resolution / conformance /
   equivalence; conformance-side collapse helpers should be subsumed by the
   override-aware `designatorSingletonType` (§4).
5. **`sameThisInstance` over-equates literal cake `this`-pairs** vs scalac — open
   soundness question for raw `=:=`/`<:<` (§5).
6. **Performance** — override-aware singleton resolution (`getSignatures.forName`) now
   runs on the hot projection conformance/equivalence path (§4).
