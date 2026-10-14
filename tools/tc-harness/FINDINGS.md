# Findings

From exploratory use of the harness. "tip" is #723 (1c19c913ec), "base" its base on idea263.x (894b0e3ab2), scalac 2.13.18 / 3.3.8. Probes are under `probes/`.

## Regressions on the tip

### A projection to an alias no longer conforms to the abstract member it implements (a80f7ae6c0)

```scala
trait T0 { type M }; trait T1 extends T0; trait T2 { type M = Any }; trait K extends T1 with T2
trait Graph { type Edge }; object G1 extends Graph { type Edge = (Int, Int) }; class G2 extends Graph { type Edge = String }
object AP {
  val k: K = ???
  val mixin: T1#M = (??? : k.M)              // scalac ok, tip error
  val e1: G1.Edge = (1, 2)
  val viaObject: Graph#Edge = e1             // scalac ok, tip error
  val g2 = new G2; val e2: g2.Edge = "s"
  val viaVal: Graph#Edge = e2                // scalac ok, tip error
  val viaClass: Graph#Edge = (??? : G2#Edge) // scalac error, base ok (pre-existing)
}
```

scalac keeps the name-based arm for an alias member (`sym2.isAbstractType && isSubPre(pre1, pre2, sym2)` in `isSubType2`). The commit's premise ("scalac dealiases it first") came from the PBT oracle, which evaluates operands with `.dealias` and so expands `k.M` before `<:<` sees it; the harness oracle had the same bug (fixed: one-step `betaReduce`). `testMixedInAliasDoesNotConformToAbstractMemberOfSameName` pins the wrong answer. Probe: `probes/s2/alias-projection.scala`.

### An alias overriding an alias is no longer checked for equivalence (b3d1e57b10)

```scala
trait HasT { type T = Int }
class Diff extends HasT { override type T = String }   // scalac: incompatible type in overriding; tip: nothing
trait HasL { type L = List[Int] }
class DiffArg extends HasL { override type L = List[String] }   // same
trait HasP[X] { type P = X }
class Pd extends HasP[Int] { override type P = String }          // same
```

Base reports "cannot override final member" for every alias over alias (right by accident here, wrong for equivalent ones). The tip drops that error and, as its commit message says, doesn't check equivalence, so incompatible overrides now pass. Probe: `probes/pr723/override-var.scala`.

### A lambda's parameter escapes into its inferred type (likely 72904c0733)

```scala
trait Key { type V; def default: V }
def get(k: Key): k.V = k.default
val ks: List[Key] = ???
val vs = ks.map(k => get(k))        // tip: List[k.V]     base, scalac: List[Key#V]
val i = (k: Key) => get(k)          // tip: Key => k.V    base, scalac: Key => Key#V
val first = ks.map(k => get(k)).head // tip: k.V
```

Inside the lambda `get(k)` is now `k.V` (a stable path argument stands for its singleton), which is right, but the function literal's result isn't avoided as a block's is (54e2259e44): scalac packs the escaping parameter. Errors still agree in the probes; inferred types (hints, "add type annotation") are wrong. Probe: `probes/pr723/lambda-param-escape.scala`.

## Gaps in the tip's fixes

### Null ineligible for implicit conversion, but not through an alias (d88764da5b)

```scala
type N = Null
val b: N = null
val e3: Int = b            // scalac: an expression of type Null is ineligible for implicit conversion; tip accepts
val e4: Int = (null: N)    // same
def g: N = null
val e6: Int = g            // same
```

The check is `tp eq Null` in `ScExpression`; an alias (or a singleton of a val typed by one) isn't `Null` by identity.

## Pre-existing (both trees)

### Scala 3: an explicit-selector export followed by a wildcard export loses the first

```scala
class Printer { def print(s: String): String = s; type Out = String }
class Scanner { def scan(): Int = 1 }
class Copier {
  private val p = Printer(); private val s = Scanner()
  export p.{print, Out}
  export s.*
}
val c = Copier()
val x: c.Out = c.print("a")   // IntelliJ: Cannot resolve symbol Out / print; scalac ok
```

Either export alone, or the other order, is fine. Probe: `probes/s3/export-order.scala`.

On base the result depends on timing: run alone it fails every time, but with the tip server highlighting at the same moment (`tc.py --each --repeat 3`, both servers in parallel) base sometimes reports no errors for `export-order.scala` and `exports.scala`, with nothing logged. The tip fails every time. Possibly a race between highlighting passes in export resolution; not investigated.

### Scala 3: union types widened, primitives not Matchable

```scala
val u = if true then 1 else "s"   // scalac: Int | String, IntelliJ: Any
val m: Matchable = u              // IntelliJ: Any doesn't conform to Matchable
// IntelliJ: Int <: Matchable false, String <: Matchable false, Int | String <: Matchable false
```

Being looked at in a separate session. Probe: `probes/s3/unions.scala`.

### Bounds of inferred type arguments aren't checked

```scala
JavaBox.max(java.util.Arrays.asList(1, 2, 3))   // <E extends Comparable<? super E>> E max(Collection<? extends E>)
// scalac: inferred type arguments [Int] do not conform to method max's type parameter bounds; IntelliJ accepts
```

Probe: `probes/s2/java-generics.scala` (with `JavaBox.java`).

### lub drops a type member refinement with differing members

```scala
trait M { type T }
val ac = if (true) new M { type T = Int } else new M { type T = String }
// scalac: M{type T >: Int with String}, IntelliJ: M
```

### Others

- `class G2 extends Graph { type Edge = String }`: `(??? : G2#Edge): Graph#Edge` is an error in scalac 2 (base and tip accept it).
- Scala 3 inline: `transparent inline` result types and `compiletime.error` aren't modelled (by design).

## Fixed on the tip, confirmed

- lub of path-dependent classes: base `U.this.T`, tip and scalac `U#T` (`probes/s2/lub-paths.scala`).
- Refinement member through `self: Box { type A = Box.this.A }` (`probes/pr723/refinement-subst.scala`).
- `null` against an abstract type with no lower bound (`probes/pr723/null-alias.scala`).
- SCL-21947 cake override (`examples/SCL21947Cake.scala`).
