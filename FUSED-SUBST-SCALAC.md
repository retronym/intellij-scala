# Handoff: model IntelliJ's fused this-substitution IN SCALAC (focused unit tests)

Goal: implement IntelliJ-Scala's fused substitution chains + this-type walks as small, unit-testable TypeMaps inside scalac (`~/code/scala`), reproduce both the self-embedding growth pump AND the one known case that legitimately needs sequential this-composition, and find the discriminator predicate that admits the second while blocking the first. That predicate is the sound replacement for IntelliJ's ad-hoc `hasRecursiveThisType` guard.

This is the "flip the script" plan: the in-situ IntelliJ investigation (branch `scala-typesystem-tck` in `~/code/intellij-scala`, commits `cb28b45738..da8621b4eb`) took the analysis as far as it cleanly goes; what remains needs isolated tests that IntelliJ's fixture harness is too coarse for.

---

## 1. What is already established (do not re-derive)

The evidence lives in the commit messages of `cb28b45738`, `54ed231c83`, `a3f4f79cfc`, `46a4210f07`, `590632a9d1`, `815354919c`, `4350b13f8d`, `d98a85b5c6`, `da8621b4eb` on `scala-typesystem-tck`, and in the long comments on `OverrideHighlightingTest.testScratchInferencerTrace` / `testScratchRemnantGrowthTrace` (which embed the annotated traces, IntelliJ and scalac side by side).

- **IntelliJ's `ThisTypeSubstitution(target, seenFromClass)`** is a leaf update over `ScThisType` inside the generic `recursiveUpdate` engine. Two walks: **anchored** (`seenFromClass != null` and nested: lockstep `(target baseType clazz).prefix` climb via `clazz.containingClass` — scalac-shaped) and **anchorless** (`isMoreNarrow` heuristic: per this-leaf, "is the target's class an inheritor of this leaf's class?" — if yes, return the WHOLE target).
- **Fused chains**: `ScSubstitutor` holds an `Array[Update]`; on `ReplaceWith(res)` the REMAINDER of the chain processes `res`. The engine's own warning (ScSubstitutor.scala ~L64) says fused updates must be leaf→leaf; `ThisTypeSubstitution` matches a leaf but produces a path — contract violated by design.
- **The growth pump** (`testScratchRemnantGrowthTrace`): every growth round is a fused PAIR — `[update 1/2]` rewrites `Global.this → <grown path>` (benign), `[update 2/2]` rewrites `Infer.this → <whole target>` (self-embedding: the target is rooted at `Infer.this`), then resolution projects `.global` on top and recirculates the +1 spelling as the next round's freshly-constructed substitutor target (`BaseProcessor.processTypeImpl:315`, `ScProjectionType.apply → actualElement`, synchronously inside the substitution pass). Growth is **sequential, not nested** (fires at depth ~2 under a cap of 10; reaches 60+ segments; ends in SOE from mere descent). No depth limit can bound it.
- **The poison rewrite itself is scalac-sanctioned**: `Infer.this.asSeenFrom(analyzerPath, Infer) == analyzerPath` in scalac too (see `AsSeenFromTest.remnantFixpoint`, POISON STEP). The divergence is recirculation, not the rewrite.
- **scalac's separating invariant is round-trip NEUTRALITY, not canonicalization**: `underlying(pre.analyzer.global) == pre` for ANY pre, because the copied-refinement decls are re-anchored ONCE to the selection prefix by the cached widen. Net growth per resolution round = 0 → fixpoint → no guard needed. scalac does NOT eagerly collapse a deep spelling.
- **Experiment ledger** (all property-gated probes still in the branch):
  - guard off (`-Dscala.asf.noguard`): SOE.
  - canonicalize-at-mint (now DEFAULT-ON, `-Dscala.asf.nocanon` disables): kills the spelling doubling (0 grown targets on the fixture) but NOT the pump; guard-off still SOEs.
  - anchoring (`da8621b4eb`, product change): `seenFromClass` threaded through 6 hot sites, null firings 127→1; does not stop the pump (necessarily — see poison-is-sanctioned above).
  - **terminal-output** (`-Dscala.asf.terminal`): a REWRITING this-substitution's output is not processed by the remainder of its chain. Kills the pump completely (guard-off + terminal: completes, zero growth, zero errors). Suite-wide: **591/592** — the single counterexample is `TypeInferenceBugs5Test.testSCL7043`, which fails under BOTH blanket-terminal and the refined variant (skip only subsequent this-substitutions, let type-param updates through). So sequential this-composition is load-bearing somewhere legitimate.
  - **chain redundancy**: 3224/3230 multi-this-subst chains have duplicate targets; the same INSTANCES (by identity) appear twice in one chain via wholesale `followed()` concatenation (9-update chains with 3 distinct targets). Dedup is a perf candidate but NOT trivially semantics-free (same subtlety as terminal).

**THE OPEN QUESTION**: what predicate distinguishes the pump's self-embedding rewrite from SCL-7043's legitimately-needed sequential re-anchor? Every structural shortcut tried (depth, spelling canonicalization, chain position, terminal) either misses the pump or breaks SCL-7043. IntelliJ's guard arm-1 ("target contains the this-type being rewritten") blocks the pump and passes SCL-7043 — but it is per-leaf, scans the whole target each firing (hot), and over-blocks in other shapes (the SCL-21947 family this branch fixed by carving out objects etc.). The scalac model should find the principled rule.

## 2. Step 0 (in intellij-scala): capture the SCL-7043 chain

We never captured WHAT chain SCL-7043 needs. Before modeling, get it:

- Fixture: `scala/scala-impl/testdata/typeInference/bugs5/SCL7043.scala` (`CE[T <: Enumeration](val enum: T) extends C[T#Value]`, overloaded `foo(enum.values.toList(0))`).
- Add a scratch test (pattern: `OverrideHighlightingTest.testScratchInferencerTrace`) that sets `System.setProperty("scala.asf.trace", "true")` AND `"scala.asf.terminal", "true"` in the test body (sbt `-D` is NOT forwarded to the forked JVM), runs the SCL7043 snippet, and captures: the CHAIN-AUDIT lines, the REWRITE `[k/n]` lines, and which rewrite is skipped under terminal that the correct result needs. That skipped rewrite IS the legitimate sequential case to model.
- Run: `sbt packageArtifact "testOnly org.jetbrains.plugins.scala.annotator.OverrideHighlightingTest -- --tests=<name>"` from the repo ROOT (root testOnly, NOT scala-impl/ — see SCL-21947.md §4; plugin loads from `./target/plugin`).
- Trace channels available (all stderr): `NEW #id target=.. seenFromClass=.. at <site>`, `thisTypeAsSeen(..)#id [pre=..]` indented walks, `GUARD#id ... -> BLOCK`, `REWRITE #id [update k/n]`, `CHAIN-AUDIT[n updates, m this-substs]`, `CANON`, `ORIGIN` (one-shot stack via `-Dscala.asf.origin=<regex>`).

## 3. The scalac-side build (gotchas)

- Repo `~/code/scala`, branch `2.13.x`, HEAD `370fa34b9e` ("trace logging for as-seen-from": TypeMaps.scala hooks made protected + `AsSeenFromTest`).
- **The working tree contains unrelated scratch that BREAKS the reflect build**: `src/reflect/scala/reflect/internal/Repro.scala` (a full copy of Types.scala — duplicate definitions) + `scratch/`, and modified `MutableSettings.scala` etc. with deliberate NOK lines. The dance:
  ```
  mv src/reflect/scala/reflect/internal/Repro.scala /tmp/stash/ && mv src/reflect/scala/reflect/internal/scratch /tmp/stash/
  git stash push -m tmp
  git checkout stash@{0} -- test/junit/scala/reflect/internal/AsSeenFromTest.scala   # keep the harness
  ... work, run ...
  git stash pop && mv /tmp/stash/Repro.scala src/reflect/scala/reflect/internal/ && mv /tmp/stash/scratch src/reflect/scala/reflect/internal/
  ```
  RESTORE EXACTLY when done; Jason's scratch is precious.
- Run tests: `/opt/homebrew/bin/sbt "junit/testOnly scala.reflect.internal.AsSeenFromTest -- --tests=<name>"` (first build ~1-2 min, then fast).
- Existing harness in `AsSeenFromTest.scala` (working tree, staged): `tracedAsSeenFrom(pre, member)` (LoggingAsSeenFromMap — indented apply/thisTypeAsSeen/matchesPrefixAndClass trace), `inferencerTypes` fixture (the exact Typers/Infer/Analyzer/Global cake), `inferencer` and `remnantFixpoint` tests. Build on these.

## 4. What to build

A small `FusedSubstMap` in test code (no production scalac changes needed):

- `sealed trait Upd`; `case class ThisTypeUpd(target: Type, anchor: Option[Symbol])`; `case class TypeParamUpd(from: List[Symbol], to: List[Type])`.
- `class FusedSubstMap(updates: List[Upd]) extends TypeMap` mirroring IntelliJ `recursiveUpdateImpl`: for each type node, first update that matches a leaf REPLACES it and the REMAINDER of the list processes the replacement; non-matching nodes descend (`mapOver`) with the full list. Mirror the prefix-descent of projections (SingleType/TypeRef prefixes) — that is where the pump lives.
- `ThisTypeUpd` semantics, faithful to IntelliJ:
  - anchored + nested anchor: lockstep `(target baseType clazz).prefix` climb over `clazz.owner` — cf. `ThisTypeSubstitution.doUpdateThisTypeFromClass`.
  - anchorless (or top-level anchor — note IntelliJ ALSO uses the heuristic when `clazz.containingClass == null`): the `isMoreNarrow` guess — `if (target.typeSymbol isSubClass th.sym) target else` climb `target`'s prefix chain and retry (cf. `doUpdateThisType`/`containingClassType`).
- A recirculation driver reproducing the resolution loop: `round(pre) = { path = select(select(pre, analyzer), global); apply FusedSubstMap built from path to member infos; next pre = the resulting path }` — assert +1 spelling growth per round with no guard, 0 with a working discriminator.
- Unit fixtures:
  1. **Pump**: `inferencerTypes` + the fused pair `[ThisTypeUpd(grown, Some(Global)), ThisTypeUpd(path, None)]` — must grow without a guard.
  2. **SCL-7043**: port the Enumeration/T#Value chain captured in Step 0 — must produce the correct member type ONLY when the later this-rewrite is allowed to process the earlier one's output.
  3. **Dup-chain**: the 9-update/3-distinct-target chain — establish whether removing instance-duplicates changes any result (dedup safety).
- Oracle: scalac's own `asSeenFrom`/`memberType` on the same inputs (`tracedAsSeenFrom`); goldens by construction.

## 5. Candidate discriminators to test (the research core)

Test each against fixtures 1 AND 2:

- **arm-1 as-is**: block when the target CONTAINS the this-type being rewritten (IntelliJ production). Expect: blocks pump, passes 7043 — confirm, then characterize WHY 7043's target doesn't contain its rewritten this.
- **root-of-spine**: block only when the rewritten this-type is the ROOT of the target's own prefix spine (vs. occurring in argument/refinement position). Cheaper than arm-1's full subtype scan; may be exactly equivalent on both fixtures — that would be the win (O(spine) vs O(type)).
- **round-trip neutrality as a postcondition**: allow the rewrite, but assert/enforce `underlying(result-path) == input-pre` à la scalac; reject rewrites that break neutrality. Possibly the most principled; needs the driver to check.
- **terminal-per-element**: the remainder may not rewrite this-types whose class occurs on the replacement's spine (weaker than blanket terminal). Check against 7043.

Deliverables: (a) the discriminator that passes both fixtures with a one-paragraph soundness argument; (b) a back-port sketch to `ThisTypeSubstitution` (replace `hasRecursiveThisType0`'s subtype scan); (c) the dedup-safety verdict for `followed()`.

## 5a. RESULTS (2026-07-02, scalac commits `e1d01cf9da` + `5b57ce1e52` on 2.13.x)

Step 0 and the scalac model are DONE; the discriminator research has a leading answer.

**Step 0 (SCL-7043 chain captured)** — scratch tests `testScratchSCL7043Trace` /
`testScratchSCL7043TraceProd` in `OverrideHighlightingTest`. The load-bearing chain is
`[update 1/2] Enumeration.this -> CE.this.enum.type (sfc=Enumeration)` followed by
`[update 2/2] target=CE.this.enum.type sfc=CE` re-anchoring the freshly introduced
`CE.this`. Surprise: the second rewrite is guard-BLOCKED in production too
(`hasRecursiveThisType(CE.this, pre=CE.this.enum.type) = true`) — arm-1 is NOT what
distinguishes production from terminal on this test. Under terminal the
`[update 4/6] Enumeration.this -> CE.this.enum.type` rewrite never fires AT ALL
(vs ~13 firings in prod): terminal's damage is a NON-LOCAL cascade — an upstream
substitutor application changes shape enough that the later chain never sees the
`Enumeration.this` leaf. The plan's "one pair, two rewrites" framing of the 7043
counterexample was wrong; the isolated model below captures the *legitimate
composition* it needs, not the cascade.

**The model** (`AsSeenFromTest`: `fusedSubstPump`, `fusedPairPump`, `crossSymbolPump`,
`sequentialReanchor`; guard modes `Arm1Exact | ProdGuard | RootOfSpineExact | Progress`):

* Fidelity notes discovered en route: (a) scalac's `SingletonType.prefix` delegates to
  `underlying.prefix` and climbs past the path's logical root — any spine walk must stop
  at the first `ThisType`; (b) production's anchored walk with `clazz == leaf's class`
  falls into the NARROWING climb (may STRIP), it does not return the whole target.
* `fusedPairPump` (the real trace shape: benign strip + self-embedding poison, one shared
  recirculated target): unguarded = super-linear growth (7→9→13→21→37). Under EVERY
  guard mode the round is BYTE-IDENTICAL (`nextP == P`) — scalac's neutrality invariant
  emerging from a guarded engine.
* **`crossSymbolPump` — the headline**: rewrite `Infer.this` (fresh each round from
  member declarations) against a target rooted at `Analyzer.this` (Analyzer inherits
  Infer, so `isMoreNarrow` fires and returns the whole target). Arm-1-exact,
  root-of-spine-exact AND the faithful production guard ALL MISS — the production
  inheritor arm tests `rewrittenClass.isSubClass(containedThisClass)`, the INVERSE
  direction of what the pump exploits — and the pump runs (9→11→13→15). The observed
  IntelliJ traces are saved by an accident of spelling: recirculated targets happen to be
  rooted at the SAME symbol as the rewritten leaf (`Infer.this.global...`). Nothing
  enforces that alignment. **Open question: build an IntelliJ fixture that recirculates a
  target whose root symbol differs from the rewritten this (e.g. member declared in
  `Typers`, path spelled through `Analyzer`/a subclass) — if reachable, production has an
  unguarded pump.**
* **The winning discriminator — `Progress` (a POSTcondition, not a target scan)**: block
  a this-rewrite iff the RETURNED type's spine root is `ThisType(c)` with
  `c.isSubClass(leafClass)`. Rationale: the rewrite claims to eliminate `leaf.this` but
  returns a type still rooted in a this-type denoting that same instance (via
  inheritance) — no progress, self-embedding, recirculation fuel. scalac's
  `thisTypeAsSeen` only ever STRIPS prefixes from `pre`, so its output structurally
  cannot remain rooted in the this it eliminates; Progress enforces exactly that
  property in an engine that (unlike scalac) substitutes whole targets. Scorecard:
  blocks the exact pump ✓, blocks the cross-symbol pump ✓ (only mode that does),
  admits benign strips ✓, admits SCL-7043's sequential re-anchor ✓ (`CE.this.en.type`
  is rooted at `CE.this` and CE does NOT inherit Enumeration — it AGGREGATES one;
  inheritance-rooted returns are fuel, aggregation-rooted returns are honest
  re-anchors). Cost: O(output spine) + one subclass test, vs arm-1's O(type-size)
  scan of the target per firing.

**Backport DONE as probes** (deliverable b) — intellij-scala commit `da7c96e8d9`.
Progress alone over-blocked 3 SCL-21947 shapes until refined with the leaf-exemption
(bare this-type returns always admitted: leaf→leaf narrowing `Types.this ->
Global.this` IS the legitimate cake re-anchor and carries nothing to recirculate).
With that, one residual failure remained: SCL-7008, expected `NM.this.Name` got
`F.this.Name` — and scalac (`-Xprint:typer`, verified) spells it `NM.this.Name`, so
the golden is true parity. Root cause was NOT Progress: bypassing the old guard lets
REDUNDANT chain elements (the `followed()` duplicate-target concatenations) re-narrow
an already-processed this (`NM.this ->(identity match) -> SN.this -> F.this`, where
scalac stops at the first match). Production only preserves `NM.this` by accident:
its guard-blocks truncate chains as a side effect.

**The second rule — `-Dscala.asf.consumed` (first-match-wins per this-class)**: once
a chain update MATCHES a this-leaf of class C on its target's own spine (identity
included), the remainder may not rewrite C's this again — but MAY rewrite this-types
of NEW classes the output introduced (SCL-7043's `CE.this` re-anchor survives). The
critical subtlety (found because consumption first broke SCL-7043 again): a match
reached only by ESCAPING the target through `ScThisType -> containingClass` hops is
scalac's UNMATCHED case — the walk fell off `pre`; scalac returns the this unchanged
and a later hop still applies — so exhaustion/escape matches do NOT consume
(SCL-7043's `[3/6]` identity exhaustion must not eat the load-bearing `[4/6]`).
This is what terminal-output was groping at: terminal killed ALL remainder
processing; consumed kills only same-class re-processing, which is why terminal
broke SCL-7043 and consumed does not. **This also settles the dedup question
(deliverable c) semantically**: duplicate-target chain elements are not merely
redundant — without consumption they actively over-narrow (SCL-7008); with
consumption their firings are suppressed exactly when they would diverge from
scalac. Physical dedup of `followed()` remains a perf-only follow-up.

**Validation, both probes forced on suite-wide (guard fully bypassed)**:
`OverrideHighlightingTest` 47/47, `TypeInferenceBugs5Test` 415/415 (incl. 7043 +
7008), `TypeSystemTckTest` green, `typeConformance.generated.*` +
`Singleton*ConformanceTest` 125/125. Progress alone also terminates the pump under
`nocanon` (previously SOE with guard off).

**Productization DONE** (`1d9ac8817f`): `hasRecursiveThisType` deleted (including
the object carve-out — an object-rooted return has a projection root, so PROGRESS
admits it by construction; the SCL-18532↔SCL-3654 tension retires with it);
PROGRESS + CONSUMED are the unconditional semantics; the `noguard`/`maxdepth`/
`terminal`/`progress`/`consumed` probe flags and obviated scratch probes deleted
(canonicalize-at-mint and the `scala.asf.trace` channel kept). Oracle after
deletion: 42 + 415 + 1 + 125, all green. Remaining before master: the SCL-18532
perf re-run on real scala/scala `Typers.scala` (expectation: improvement — both
rules are cheaper per firing and consumption prunes chain work), and a production
repro attempt of the cross-symbol pump (member declared in a base trait, path
spelled through an inheriting class).

## 5b. RESULTS (2026-07-05): cross-symbol pump CONFIRMED in production and FIXED — the third rule, ANCHOR DISCIPLINE

The §5a open item ("production repro attempt of the cross-symbol pump") landed, SOE'd, and got its rule.

**Production repro**: a skeletor extraction of the real Infer/Analyzer/Global cake, trimmed to 39 lines
(`~/code/minimal/target/runs/cake-compact/skeleton-minimal.scala`, test
`OverrideHighlightingTest.testScratchSkeletorCakeCrossSymbolPumpMinimal`). One batch annotation pass grew the
expected type from `Infer.this.global.Type` to `…global.analyzer.global.analyzer.global.Type`; on real
scala/scala sources the same shape is the ThisTypeSubstitution SOE. scalac oracle on the fixture: a bounded
mismatch, `required: Infer.this.global.analyzer.global.Type` — exactly ONE re-anchor round.

**Why PROGRESS+CONSUMED miss it (model/production gap resolved)**: production spells targets rooted at
`Global.this`/`Infer.this.global`, never `Analyzer.this` (the model fixture's root), so Progress's
root-inherits-leaf check never fires — Global inherits none of the cake traits. And the growth is not a
recirculation shape at all: it is a **per-firing soundness hole in the anchored walk**. Both bail-outs
(owner-cursor exhausted at top-level; `pre baseType clazz` empty) fell back to the anchorless `isMoreNarrow`
heuristic, which happily rewrote `Infer.this -> Global.this.analyzer.type` inside a chain whose anchor was
`Typer` — a cursor that can never reach Infer. scalac's `matchesPrefixAndClass` demands `clazz == candidate`:
that firing is UNMATCHED, the this-type is kept for a correctly-anchored hop. Each poison rewrite embeds a
fresh `Global.this` root that the chain remainder re-anchors: +2 spine segments per application, recirculated
without bound because every fresh spelling is a fresh cache key
(`ORIGIN` stacks: `updateProjectionType` rebuild -> eager `actualElement` / canonicalize probe / conformance
`collapseSingletonPath` -> `processTypeImpl` mints on ever-deeper prefixes).

**The rule — ANCHOR DISCIPLINE** (`ThisTypeSubstitution.anchoredNarrowAdmitted`): an anchored walk may fall
into the narrowing climb only if
(a) `cursorChainReaches` — its remaining `containingClass` chain can reach the leaf's class
(same-or-INHERITOR — the inheritor allowance compensates for IntelliJ spelling cake self-types through the
declaring trait, and for the SCL-21947 Trees/AstTransformer shape where the leaf is reachable further up the
cursor chain; `areClassesEquivalent` beside `==`, because an OBJECT member's declarationAnchor is a different
PSI handle than the ScThisType's ScObject — SCL-6549's `SCL6549` cursor failing to "reach" `SCL6549`), OR
(b) `targetDenotesLeafClass` — the target path denotes EXACTLY the leaf's class/object
(`implicitInstance.this` against pre `SCL6549.implicitInstance.type`): an honest re-spelling of the same
instance that scalac's correctly-anchored hop would produce; IntelliJ's fused chain reaches it through a
coarser anchor standing in for two sequential asSeenFroms. Exact class only — a strict-INHERITOR tip
(`Global.this.analyzer.type` vs `Infer.this`) is precisely the poison and stays blocked.
Otherwise: keep the this-type, `noteConsumes(false)` — scalac's UNMATCHED. Applied at BOTH
bail-outs; the anchorless walk (`clazz == null`) keeps the heuristic unchanged.

**Falsified en route** (kept out of the tree): making the substitution pass resolution-free
(raw projection rebuild without the eager alias collapse + canonicalization moved off the rebuild path)
was neither sufficient (the accretion re-entered through conformance-side collapse probes) nor safe
(broke alias-collapse goldens SCL-6549/7100/7268/7474). The re-entrant resolutions during rebuild are
bounded once the firings themselves are disciplined — the poison source, not the recirculation plumbing,
was the bug. Diagnostics that found it: `RecursiveUpdateDepthGuard` (SubtypeUpdater), the
`scala.asf.origin` one-shot stack hunt, and CHAIN-AUDIT.

**Validation**: repro test now asserts byte-exact scalac parity (one `.analyzer.global` round);
OverrideHighlightingTest + TypeSystemTckTest + TypeInferenceBugs5Test + Singleton*/typeConformance.generated
all green. scalac-side model note updated at `crossSymbolPump` (the model's `anchoredMatch` fallbacks
reproduce the unfixed behaviour; a faithful update returns None at the bail-outs unless the owner chain
reaches the leaf's symbol).

## 6. Non-goals / parked

- The `BaseTypes` caching design (PR #5 comment thread) — orthogonal; the pump exists even with cached base types.
- Retiring the conformance-side collapse helpers — blocked on profiling canonicalize-at-mint against real scala/scala sources (Jason checking manually).
- The remaining anchorless `ScSubstitutor(tp)` sites — enumerated in `da8621b4eb`'s message. UNPARKED: now its own handoff, ANCHORLESS-ELIMINATION.md (census -> per-site anchoring -> staged deletion of the `clazz == null` mode; includes the `targetDenotesLeafClass` redundancy re-test).
