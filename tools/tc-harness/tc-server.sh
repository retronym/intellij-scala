#!/bin/bash
# Starts the type-checking harness server in an intellij-scala worktree (foreground; run it in the
# background yourself). Usage: tc-server.sh <worktree> <port> [label]
# Packages the plugin first, so source changes in the worktree are picked up by a restart.
set -u
wt=$(cd "$1" && pwd); port=$2; label=${3:-$(basename "$wt")}
J=${TC_BUILD_JDK:-$HOME/.sdkman/candidates/java/25.0.4-tem}
export JDK_17=${JDK_17:-$HOME/.sdkman/candidates/java/17.0.20-tem}
export TC_PORT=$port TC_LABEL=$label
cd "$wt"
mkdir -p target
exec env PATH="$J/bin:$PATH" JAVA_HOME="$J" sbt --java-home "$J" -J-Xmx2g -batch -Dsbt.server.autostart=false \
  packageArtifact "testOnly org.jetbrains.plugins.scala.tcHarness.TcServerTest -- --tests=testServe" \
  > target/tc-harness.log 2>&1
