#!/bin/bash
# Restarts the A/B pair of harness servers after harness changes: stops both, replays the tip
# worktree's harness commits onto the base worktree's tree under test, then starts tip and base
# one after the other (each start runs an sbt build) and waits until each answers /health.
#
# Env (defaults are the #723 pair): TC_TIP_WT, TC_BASE_WT, TC_TIP_UNDER, TC_BASE_UNDER.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
W=$HOME/code/.worktrees/intellij-scala
tip=${TC_TIP_WT:-$W/tc-tip}; base=${TC_BASE_WT:-$W/tc-base}
tipUnder=${TC_TIP_UNDER:-1c19c913ec}; baseUnder=${TC_BASE_UNDER:-894b0e3ab2}

"$here/tc.py" --shutdown >/dev/null 2>&1 || true
for port in 8723 8724; do
  while lsof -iTCP:$port -sTCP:LISTEN >/dev/null 2>&1; do sleep 1; done
done
# the sbt JVMs exit a few seconds after their test ends
while lsof -a -d cwd -c java 2>/dev/null | grep -qE "$tip\$|$base\$"; do sleep 1; done

git -C "$tip" diff --quiet HEAD -- tools/tc-harness scala/scala-impl/test/org/jetbrains/plugins/scala/tcHarness scala/scala-impl/testdata/tcHarness \
  || { echo "uncommitted harness changes in $tip"; exit 1; }
git -C "$base" reset -q --hard "$baseUnder"
# only the harness commits (their titles say so); fixes under test on the tip stay off the base
git -C "$base" cherry-pick $(git -C "$tip" rev-list --reverse --grep="type-checking harness" "$tipUnder"..HEAD) >/dev/null
echo "base: $(git -C "$base" log --oneline -1)"

start() { # worktree port label
  nohup "$here/tc-server.sh" "$1" "$2" "$3" >/dev/null 2>&1 &
  until curl -s "http://127.0.0.1:$2/health" >/dev/null; do
    sleep 2
    if grep -qE '^\[error\]' "$1/target/tc-harness.log" 2>/dev/null; then grep -E '^\[error\]' "$1/target/tc-harness.log" | head; exit 1; fi
  done
  echo "$3: $(curl -s "http://127.0.0.1:$2/health" | tr -d '\n ')"
}
start "$tip" 8723 tip
start "$base" 8724 base
