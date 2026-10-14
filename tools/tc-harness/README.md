# Type-checking harness

Experiment tooling, not for merge. It lets an agent, a script or a model (e.g. the Lean TCK) ask "what does IntelliJ make of this code, and does scalac agree?" in a fraction of a second, against two builds of the plugin at once.

## Why

Finding type-system bugs means trying many small programs. A JUnit test per idea costs a build and a cold project (tens of seconds) and gives one tree's answer. Here the IntelliJ test application stays up, the light project for an environment stays open (the JDK and libraries are indexed once), and each request only swaps in its source files. scalac runs in the same JVM, in an isolated classloader, in parallel with highlighting. A warm request is ~0.2–0.4 s for both.

Running one server per worktree (#723's tip and its base on `idea263.x`) gives a three-way comparison: a finding that both trees share is pre-existing, one only on the tip is a regression.

## Pieces

- `TcServerTest` (in `scala-impl/test/.../tcHarness`): a "test" that serves HTTP until shut down. It sets the production modes of the tip's runtime checks (`scala.types.substitutorInvariants=off`, `scala.types.recursionGuard.failHard=false`); on the base these properties are unused.
- `TcWorker`: one `ScalaLightCodeInsightFixtureTestCase` per environment, set up when a request first needs it and torn down when a request needs a different one.
- `TcOracle` + `testdata/tcHarness/Oracle{2,3}.scala`: scalac 2 (fresh `Global` per request; jar indexes are cached by scalac) and Scala 3 (one compiler, a `Run` per request). The oracle source is compiled by the requested compiler on first use and cached in `~/.cache/tc-harness`.
- `tc-server.sh <worktree> <port> [label]`: `packageArtifact` then the server, batch sbt, log in `<worktree>/target/tc-harness.log`. Restart it to pick up plugin changes.
- `tc.py`: the client.

## Running

```bash
tools/tc-harness/tc-server.sh ~/code/.worktrees/intellij-scala/tc-tip 8723 tip
```

```bash
tools/tc-harness/tc-server.sh ~/code/.worktrees/intellij-scala/tc-base 8724 base
```

Each worktree needs an untracked `local-test-sandbox.sbt` with its own `intellijPluginDirectory` (see the build-env notes); the first request for an environment also pays for the project setup (~5–10 s), later ones don't.

```bash
tools/tc-harness/tc.py tools/tc-harness/examples/SCL21947Cake.scala
```

```
== tip (1c19c913ec): intellij 196 ms, scalac 2.13.18 37 ms
  errors agree with scalac (0)
== base (894b0e3ab2): intellij 182 ms, scalac 2.13.18 61 ms
  INTELLIJ-ONLY SCL21947Cake.scala:16:20: Method 'unapply' overrides nothing
```

Options: `-s tip|base|<port>,...`, `--scala 2.13|3.3|2.13.16|...`, `--jdk 17|21|mock`, `--cp a.jar:b.jar`, `-O <scalac option>`, `-q '<query>'`, `-e '<inline source>'`, `--warnings`, `--json`, `--health`, `--shutdown`. Exit status 0 iff every server's errors match scalac's.

## Protocol

`POST /check` with

```json
{
  "scalaVersion": "2.13",
  "jdk": "17",
  "classpath": ["/abs/lib.jar"],
  "scalacOptions": ["-Xsource:3"],
  "files": [{"path": "a/A.scala", "text": "..."}, {"file": "/abs/B.scala"}],
  "queries": [{"kind": "conforms", "a": "List[Main.o.T]", "b": "Seq[Int]"},
              {"kind": "equiv", "a": "x", "b": "y"},
              {"kind": "baseType", "a": "x", "cls": "Seq"},
              {"kind": "type", "a": "x"}],
  "oracle": true,
  "warnings": false
}
```

The environment (`scalaVersion`, `jdk`, `classpath`, `scalacOptions`) is the cache key of the warm project. `jdk` is a real JDK found by `SmartJDKLoader` (env `JDK_17` etc.) and becomes scalac's `-release`; `mock` is IntelliJ's mock JDK, fast but nearly empty. `2.13` / `3.3` mean the latest release of that line known to the plugin's tests.

A query operand is the name of a definition in the sources — a type alias `N` or `__q_N` stands for its right-hand side, a val/def/object for its (result) type, a class for itself in `baseType` — or else a type expression, which the server puts in a synthetic `object __tc { type ... = <expr> }` file; so top-level paths work, locals don't. Names should be unique; the first in file order wins. A source line `// ? C a b` (also `E`, `B`, `T`) is a query, as in the type PBT's replay files.

The response has `intellij.diagnostics`, `scalac.diagnostics` (1-based `line`/`col`), `diff` (errors matched by file and line: `intellijOnly`, `scalacOnly`, `agree`), `queries` (each with `intellij`, `scalac` and a textual `agree`, which for printed types is only a hint), `intellij.loggedErrors` (platform `LOG.error`s during highlighting), timings and `setupMs`. Scalac answers queries even when the program has errors.

`GET /health` reports the label, the commit under test and the current environment; `POST /shutdown` stops the server.
