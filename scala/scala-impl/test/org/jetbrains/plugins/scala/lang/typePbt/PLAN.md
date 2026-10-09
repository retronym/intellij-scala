# Property-based differential testing of the type system — PLAN

## Why

The type-system work in #723 was driven by a hand-written TCK and by dogfooding scala/scala. Both find bugs only in shapes someone thought to write. The failures were mostly the plugin's type operations disagreeing with scalac *and with each other* in cake code (path-dependent types, self types, this-types, nested classes). A generator of such programs, with scalac as the oracle and a shrinker, should find the shapes nobody wrote.

## Shape

- **Generator.** Builds a small program AST biased towards cakes: traits with abstract type members, inner classes extending inherited inner classes, self types, vals forming stable paths (depth ≥ 2), variance boxes, refinements. Then generates type expressions valid in a scope (top level, or inside a trait for `this`-rooted types) and queries over them.
- **Oracle.** scalac 2.13 (the fixture's version), embedded in the test in an isolated classloader over the compiler jars `DependencyManager` resolves. The oracle's own code is compiled at runtime by that scalac, so the build needs no new dependency. Programs scalac rejects are discarded (the rate is reported).
- **System under test.** The same source in the light fixture; query aliases read as `ScType`s, like `TypeSystemTckTest`.
- **Queries.** `<:<`, `=:=`, and `baseType(T, C)` existence (differential); `T <: baseType(T, C)` within the plugin (metamorphic).
- **Shrinker.** On the program AST: drop unrelated queries, declarations, parents, self types, members; replace type subterms by simpler ones. A step is kept if scalac still compiles it and the same disagreement reproduces.
- **Classifier.** Each shrunk failure gets features (which type forms it contains, query kind, direction: plugin unsound or incomplete) and a signature. A registry of known issues (predicates over the shrunk failure) labels it known/unknown; unknown ones are printed in full.

Scala 2.13 only for now. Not wired into the TCK.

## Steps

- [x] 1. scalac oracle in an isolated classloader
- [x] 2. program AST + printer + generator; discard-rate report (~1% discarded)
- [x] 3. plugin engine + differential `<:<` / `=:=`
- [x] 4. shrinker
- [x] 5. classifier + known-issue registry; triage first findings
- [x] 6. baseType queries (differential existence + metamorphic `T <: baseType(T, C)`)
- [ ] 7. tune the distribution (feature coverage counts)
- [ ] 8. shrinker leaves some compound-type findings large; improve
- [ ] 9. triage remaining UNKNOWNs

## Running

```
SCALA_PBT_SEED=2 SCALA_PBT_COUNT=150 SCALA_PBT_REPORT=/tmp/pbt.md sbt packageArtifact "testOnly org.jetbrains.plugins.scala.lang.typePbt.TypePbtTest"
```

Environment variables (or `-Dscala.pbt.*`): `SEED`, `COUNT`, `REPORT` (markdown report path), `SHOWDISCARDS`, `SHRINKBUDGET`, `FAILONUNKNOWN`. The report lists, per signature (check / direction / type forms), the smallest shrunk program, labelled with a known issue from `Classify.known` or UNKNOWN.

## Future work

- Typed results compared through scalac: render the plugin's `baseType` / `lub` / member type as source, splice it back as an alias, ask scalac `=:=`.
- Cache on/off agreement.
- Corpus-harvested types (types the plugin computes while highlighting scala/scala) as a second input distribution.
- Scala 3.
