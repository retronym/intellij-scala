# Handoff: eliminate the anchorless this-substitution mode (`clazz == null`)

Goal: every `ThisTypeSubstitution` firing is ANCHORED — `seenFromClass` derived from the member's declaration site, scalac's `sym.owner` in `sym.info.asSeenFrom(pre, sym.owner)` — so that the inheritance-only `isMoreNarrow` matching rule (the last rule in the engine with no grounding in scalac's asSeenFrom) survives only as the disciplined prefix-conformance test inside anchored walks. Endgame: the `clazz == null` entry in `ThisTypeSubstitution.doUpdateThisTypeFromClass` becomes a logged anomaly, then dead code.

Why this is the right next cut: scalac has NO anchorless mode. Every this-rewrite is `(pre, clazz)`-driven, and the class cursor is what carries "which declaration level is being re-anchored". Deciding a rewrite by "target's class inherits the leaf's class" WITHOUT a cursor is a guess — it cannot distinguish "this pre denotes the instance whose member I'm viewing" from "this pre happens to widen to a subtype of the leaf's trait". The second reading is exactly what fired the cross-symbol pump (FUSED-SUBST-SCALAC.md §5b). The ANCHOR DISCIPLINE fix gates that guess out of the anchored walk's two bail-outs; `clazz == null` is the one door left.

---

## 1. Established (do not re-derive)

- **ANCHOR DISCIPLINE is production** (`ThisTypeSubstitution.anchoredNarrowAdmitted` = `cursorChainReaches` ∥ `targetDenotesLeafClass`), gating both anchored-walk bail-outs (top-level cursor exhaustion; `pre baseType clazz` empty). Cross-symbol pump dead; golden test `OverrideHighlightingTest.testScratchSkeletorCakeCrossSymbolPumpMinimal` asserts byte-exact scalac parity (`expected: Infer.this.global.analyzer.global.Type`, ONE re-anchor round) on `~/code/minimal/target/runs/cake-compact/skeleton-minimal.scala`. Full oracle 597/597. Ledger: FUSED-SUBST-SCALAC.md §5b.
- **`da8621b4eb`** threaded `ScSubstitutor.declarationAnchor(member)` through the six hot anchorless sites (null firings 127 → 1 on the Inferencer fixture). Its commit message is the primary prior art — read it. Its "remaining anchorless sites" list is reproduced in §2 below.
- **Anchoring does NOT change rewrite outputs where both walks agree** — `da8621b4eb` verified 592/592 with no golden churn. The risk profile of this work is per-site anchor MIS-derivation (wrong declaring class), not systemic.
- **Trace tooling** (all in `ThisTypeSubstitution`): `scala.asf.trace` (set via `System.setProperty` INSIDE the test body — sbt `-D` is not forwarded to the forked JVM); `NEW #id target=… seenFromClass=<null>` construction lines with 3-frame call sites; `scala.asf.origin=<regex>` one-shot full-stack dump at the first matching target mint; `CHAIN-AUDIT`. `RecursiveUpdateDepthGuard` (SubtypeUpdater) converts runaway recursion into a structural dump instead of an SOE.

## 2. Step 0 — census of FIRINGS, not constructions — DONE (2026-07-05)

A null-anchored substitutor that never matches a this-leaf is harmless; only firings matter. Implemented as an identity-keyed call-site map + a per-site firing counter in `doUpdateThisTypeFromClass`'s `clazz == null` branch, gated on `-Dscala.asf.nullcensus` (off by default, zero prod cost), dumping a sorted table at JVM shutdown. Real census on the full oracle: **11 sites, ~7560 firings**, 85% from one site. See FUSED-SUBST-SCALAC.md §5c for the full ledger entry.

Actual sites found (differs from the pre-census guesses below — `BaseProcessor:259` and `ScExtractorPattern:148` never fired on this oracle; keep watching for them):

- `ResolveStateOps.substitutorWithThisType` 0-arg overload — **6454 firings**, all six callers listed below. ANCHORED.
- `ScProjectionType.processType`'s three early-return branches (lines ~81/84/91) — 933 firings combined. ANCHORED.
- `MostSpecificUtil.innerSrrForImplicitCandidate` — 74. ANCHORED.
- `ScalaBounds.ClassLike.getSuperClasses` — 53. ANCHORED.
- `PatternTypeInference.getPatternType`'s extractor-pattern this-type re-anchor — 45. ANCHORED.
- `ScParameterizedTypeElementAnnotator.annotate` — 1. ANCHORED.
- `ScalaConformance.workWithTypeAlias:845` — 1. **NOT anchored** — see §3 finding below.

## 3. Anchoring each site — DONE for 10 of 11 sites (2026-07-05)

The pattern from `da8621b4eb`: the anchor is the DECLARING class of the member whose type the substitutor will process — `ScSubstitutor.declarationAnchor(member)` or `member.findContextOfType(classOf[PsiClass])`. Per-site judgment, not mechanical:

- `SignatureProcessor` / implicit processors / `ConstructorResolveProcessor`: anchored at the resolved/implicit named element's declaring class (`ScSubstitutor.declarationAnchor`).
- `ScProjectionType.processType`'s three branches: each guard already proves `elementClazz.exists(areClassesEquivalent(_, clazz))`, so `clazz` itself — no derivation needed — is the exact anchor.
- `MostSpecificUtil`: anchor at the implicit candidate's (`r.element`'s) declaring class.
- `ScalaBounds.getSuperClasses`: anchor at the class itself when `getNamedElement` is a `PsiClass` (asSeenFrom on a class's OWN supertypes is `sym.info.asSeenFrom(pre, sym)`, not `sym.owner` — this site differs from the member-declaration pattern), else at its declaring class.
- `ScParameterizedTypeElementAnnotator`, `PatternTypeInference`: declaring class of the projected/unapply member.

**Remaining FINDING**: `ScalaConformance.workWithTypeAlias:845` — `sign.typeAlias`'s `nameContext` has no `PsiMember` (`ScSubstitutor.declarationAnchor` returns `null`), so it stayed anchorless even after the attempted fix. Only 1 firing on the full oracle. Next step: identify which TCK/test shape drives this (likely an anonymous-refinement or synthetic type alias with no real declaration site) and decide whether that's scalac's own answer (no `sym.owner` because there's no real `sym`) or a derivable anchor we haven't found yet.

`BaseProcessor:259` (tparam upper bound) never fired in this census — if it turns up in a future run, the guidance stands: type params are not class members, so anchor at the enclosing classes of the tparam's OWNER, not the tparam itself.

## 4. Endgame flips (staged)

1. Census reaches zero across the oracle → change the `clazz == null` branch to keep current behavior but `LOG.error` once per session (soak period), then → treat as scalac's UNMATCHED (keep the leaf, `noteConsumes(false)`), then → delete the branch and make the 1-arg constructors package-private/removed.
2. **Re-test admission (b)** (`targetDenotesLeafClass`, added for SCL-6549's `implicitInstance.this` vs `SCL6549.implicitInstance.type`): it is itself a compensation — a coarsely-anchored fused update standing in for the correctly-anchored hop scalac makes via `memberType` at `sym.owner`. With full anchoring, try removing it. If SCL-6549 still passes, the chain now carries the right hop; delete (b). If it fails, the chain composition is MISSING a hop — find who should have minted the `implicitInstance`-anchored update; (b) stays only if that is architecturally unreachable, and the comment should say so.
3. Out of scope, do not touch: the escaped-climb semantics inside anchored walks (`containingClassType` escape + CONSUMED's escape exemption) — load-bearing for SCL-7043; the SCL-18532 perf re-run on real `Typers.scala` (parked in §5a of the FUSED doc; anchoring may move it, measure separately).

## 5. Validation oracle

From the repo ROOT (`/Users/jz/code/intellij-scala` — `sbt --client` attaches per-directory; running from elsewhere hits the wrong build). Batch commands in ONE quoted arg with `;` (the client mis-parses separate args):

```
sbt --client "packageArtifact; testOnly org.jetbrains.plugins.scala.annotator.OverrideHighlightingTest org.jetbrains.plugins.scala.lang.typeSystemTck.TypeSystemTckTest org.jetbrains.plugins.scala.lang.typeInference.generated.TypeInferenceBugs5Test org.jetbrains.plugins.scala.lang.typeInference.Singleton* org.jetbrains.plugins.scala.lang.typeConformance.generated.*"
```

Baseline: **597/597**. Sensitive tests to watch individually: `testScratchSkeletorCakeCrossSymbolPumpMinimal` (scalac-parity golden — any spelling drift is a red flag), `testSCL6549` (anchor-sensitive, object shapes), `testSCL7008` (CONSUMED), `testSCL7043` (legitimate sequential re-anchor — the historical counterexample killer).

## 6. Gotchas

- **PSI identity**: an OBJECT member's `getContainingClass` is a different handle than the `ScThisType`'s `ScObject` — plain `==` on anchors silently fails (`SCL6549` cursor "not reaching" `SCL6549`). Use `ScEquivalenceUtil.areClassesEquivalent` beside `==` in any anchor/class comparison.
- Plugin loads from `./target/plugin`; root `testOnly`; plain `packageArtifact` suffices (SCL-21947.md §4).
- The scalac-side model (`~/code/scala` 2.13.x, `AsSeenFromTest` + ijmodel) still has `anchorlessMatch` as the fallback in `anchoredMatch` — faithful to the UNFIXED engine. If you extend the model, the resolved-gap note above `crossSymbolPump` says what a faithful post-fix update looks like (return None at the bail-outs unless the owner chain reaches the leaf's symbol). Build dance for that repo: FUSED-SUBST-SCALAC.md §3 — stash `Repro.scala`/`scratch/`, RESTORE EXACTLY, Jason's scratch is precious.

## 7. Deliverables

(a) the census: construction-site → firing-count table, before and after; (b) the anchoring patch, oracle green at 597 (+ any new tests for sites the census surfaces); (c) the staged endgame flip, log-only first; (d) the verdict on `targetDenotesLeafClass` redundancy, with the missing-hop analysis if it is not redundant; (e) a §5c ledger entry in FUSED-SUBST-SCALAC.md and an update to the `anchor-discipline-cross-symbol-pump` memory.
