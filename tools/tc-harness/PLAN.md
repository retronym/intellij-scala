# Type-checking harness: plan

## DONE

- Long-lived server per worktree (`TcServerTest`), light project per environment, files swapped per request; env switch ~1–5 s, warm request ~0.2–0.4 s.
- scalac 2.13 and Scala 3 oracles in isolated classloaders, run in parallel with highlighting; errors diffed by file and line.
- Queries `conforms` / `equiv` / `baseType` / `type` over names or type expressions, answered by IntelliJ and scalac.
- Worktrees `tc-tip` (#723 tip 1c19c913ec + harness, branch `claude/tc-harness`) and `tc-base` (894b0e3ab2 + harness, branch `claude/tc-harness-base`); `tc.py` compares tip, base and scalac.
- Checked: SCL-21947 cake (`examples/SCL21947Cake.scala`) is red on base only.

## Findings so far

- Scala 3.3: `Int | String <: Matchable` is false in IntelliJ, true in scalac (both trees; not investigated).

## Future work

- A request that hangs or recurses forever blocks the server: no timeout or cancellation of highlighting yet. Restart it.
- Diagnostics are matched by line only; messages are not compared.
- `type` answers are compared as text, and the two printers differ (`A#T` vs `A.this.T`, prefixes); compare with `equiv` instead where it matters.
- Expression types: a query for the type of an arbitrary expression (by range or marker) rather than of a named definition.
- Only one environment is warm at a time; alternating between two pays the setup each time.
- Classpath entries are jars; Maven coordinates (resolved by `DependencyManager`) would be handy.
- Batch endpoint (many programs per request) for PBT-style sweeps, so the PBT generator can drive both trees.
