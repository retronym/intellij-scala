# Type-checking harness: plan

## DONE

- Long-lived server per worktree (`TcServerTest`), light project per environment, files swapped per request; env switch ~1–5 s, warm request ~0.2–0.4 s.
- scalac 2.13 and Scala 3 oracles in isolated classloaders, run in parallel with highlighting; errors diffed by file and line.
- Queries `conforms` / `equiv` / `baseType` / `type` over names or type expressions, answered by IntelliJ and scalac.
- Worktrees `tc-tip` (#723 tip 1c19c913ec + harness, branch `claude/tc-harness`) and `tc-base` (894b0e3ab2 + harness, branch `claude/tc-harness-base`); `tc.py` compares tip, base and scalac.
- Checked: SCL-21947 cake (`examples/SCL21947Cake.scala`) is red on base only.
- Exploratory refinements: `tc.py --each` over probe directories with per-file `//> tc` options and `--with` companions; `tc-ab.sh` rebuilds and restarts the pair.
- Oracle fixes found by using it: queries are answered at typer's end (a run with errors resets its symbols); alias operands expand one step (`dealias` changed answers); runs go through refchecks, diagnostics from a run without the query file; Scala 3 gets a fresh ContextBase per compile (definitions leaked between requests); request Java files are compiled by javac.
- Type answers are compared by scalac (mutual conformance with IntelliJ's canonical text), not as text.

## Findings

See FINDINGS.md.

## Future work

- A request that hangs or recurses forever blocks the server: no timeout or cancellation of highlighting yet. Restart it.
- Diagnostics are matched by line only; messages are not compared.
- A type answer whose IntelliJ canonical text doesn't type in scalac (this-types, locals, Scala 3 syntax in Scala 2) falls back to a textual comparison.
- scalac runs nothing after refchecks: errors from later phases are not compared. When typer reports errors scalac stops there, while IntelliJ still reports refchecks-like errors.
- Expression types: a query for the type of an arbitrary expression (by range or marker) rather than of a named definition.
- Only one environment is warm at a time; alternating between two pays the setup each time.
- Classpath entries are jars; Maven coordinates (resolved by `DependencyManager`) would be handy.
- Batch endpoint (many programs per request) for PBT-style sweeps, so the PBT generator can drive both trees.
