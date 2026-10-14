#!/usr/bin/env python3
"""Client for the type-checking harness (TcServerTest). See README.md.

  tc.py [options] FILE... | -e CODE      one request with all files
  tc.py --each [options] FILE|DIR...    one request per file, one summary line each
  tc.py --health | --shutdown

Sends the files to one or more harness servers and prints IntelliJ's errors against scalac's,
plus the answers to queries. With several servers (default: tip and base) it also shows where
the servers disagree with each other.

A source file may carry its own options in `//> tc <options>` lines (e.g. `//> tc --scala 3.3`)
and queries in `// ? C a b` lines.
"""
import argparse, json, os, re, shlex, sys, urllib.request, urllib.error
from concurrent.futures import ThreadPoolExecutor

SERVERS = {"tip": 8723, "base": 8724}


def post(port, path, body=None, timeout=600):
    data = None if body is None else json.dumps(body).encode()
    req = urllib.request.Request(f"http://127.0.0.1:{port}{path}", data=data,
                                 headers={"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:
            return json.loads(r.read())
    except urllib.error.HTTPError as e:
        return {"error": e.read().decode(errors="replace")}
    except (urllib.error.URLError, ConnectionError, TimeoutError) as e:
        return {"error": f"cannot reach port {port}: {e}"}


def parse_query(s):
    kinds = {"C": "conforms", "E": "equiv", "B": "baseType", "T": "type"}
    k, _, rest = s.strip().partition(" ")
    if k not in kinds:
        sys.exit(f"bad query {s!r}: use 'C a b', 'E a b', 'B a Cls' or 'T a'")
    # operands are separated by ' ; ' when they contain spaces, else by whitespace
    ops = [o.strip() for o in rest.split(" ; ")] if " ; " in rest else rest.split()
    kind = kinds[k]
    if kind == "type":
        return {"kind": kind, "a": " ".join(ops)}
    if len(ops) != 2:
        sys.exit(f"bad query {s!r}: two operands needed (use ' ; ' between operands with spaces)")
    return {"kind": kind, "a": ops[0], ("cls" if kind == "baseType" else "b"): ops[1]}


def file_queries(text):
    """`// ? C a b` lines in a source file are queries (the type PBT's replay format)."""
    return [parse_query(l.strip()[5:]) for l in text.splitlines() if l.strip().startswith("// ? ")]


INFERRED = re.compile(r"^\s*(?:(?:private|protected|override|final|implicit|lazy)\s+)*(?:val|var|def)\s+([A-Za-z_]\w*)\s*(?:\[[^\]=]*\])?(?:\([^)]*\))*\s*=(?!>)", re.M)


def inferred_queries(text, path):
    """A `type` query for each val/var/def whose type is inferred (no annotation); names should be unique.
    Each remembers its line (`_at`, client-side only): there, a scalac error makes scalac's answer noise."""
    seen = {}
    for m in INFERRED.finditer(text):
        seen.setdefault(m.group(1), text.count("\n", 0, m.start(1)) + 1)
    return [{"kind": "type", "a": n, "_at": (path, line)} for n, line in seen.items()]


def drop_noise(req, resps):
    """Inferred-type queries on a line where scalac reports an error compare an error type: drop them."""
    at = [q.get("_at") for q in req["queries"]]
    for r in resps:
        sc = r.get("scalac")
        if not sc or "queries" not in r:
            continue
        bad = {(d["file"], d["line"]) for d in sc["diagnostics"] if d["severity"] == "error"}
        r["queries"] = [q for q, a in zip(r["queries"], at) if a not in bad]
    return resps


def file_options(text):
    return [w for l in text.splitlines() if l.startswith("//> tc ") for w in shlex.split(l[7:])]


def make_parser():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("files", nargs="*")
    ap.add_argument("-e", "--expr", action="append", default=[], help="inline source (repeatable; Snippet<i>.scala)")
    ap.add_argument("-s", "--server", default="tip,base", help="comma-separated names (tip, base) or ports")
    ap.add_argument("--scala", default="2.13")
    ap.add_argument("--jdk", default="17", help="17, 21, ... or mock")
    ap.add_argument("--cp", default="", help="extra classpath jars, ':'-separated")
    ap.add_argument("-O", "--option", action="append", default=[], help="scalac option (repeatable)")
    ap.add_argument("-q", "--query", action="append", default=[], help="'C a b' | 'E a b' | 'B a Cls' | 'T a'")
    ap.add_argument("--no-oracle", action="store_true")
    ap.add_argument("--infer", action=argparse.BooleanOptionalAction, default=None,
                    help="query the type of every unannotated val/var/def (default: on with --each)")
    ap.add_argument("--warnings", action="store_true")
    ap.add_argument("--fresh", action="store_true", help="set the project up again first (rules out state from earlier requests)")
    ap.add_argument("--json", action="store_true", help="print raw responses")
    ap.add_argument("--with", dest="with_", action="append", default=[], help="extra file sent along (with --each or //> tc: relative to the probe)")
    ap.add_argument("--each", action="store_true", help="one request per file (directories: their *.scala)")
    ap.add_argument("-v", "--verbose", action="store_true", help="with --each: full report for files that disagree")
    ap.add_argument("--health", action="store_true")
    ap.add_argument("--shutdown", action="store_true")
    return ap


def fmt(d):
    return f"{d['file']}:{d['line']}:{d['col']}: {d['message']}"


def request(a, files):
    queries = [parse_query(q) for q in a.query] + [q for f in files for q in file_queries(f["text"])]
    if a.infer or (a.infer is None and a.each):
        explicit = {q["a"] for q in queries if q["kind"] == "type"}
        queries += [q for f in files if f["path"].endswith(".scala") for q in inferred_queries(f["text"], f["path"]) if q["a"] not in explicit]
    return {"scalaVersion": a.scala, "jdk": a.jdk, "classpath": [p for p in a.cp.split(":") if p],
            "scalacOptions": a.option, "files": files, "queries": queries,
            "oracle": not a.no_oracle, "warnings": a.warnings, "fresh": a.fresh}


def run(servers, req):
    body = dict(req, queries=[{k: v for k, v in q.items() if not k.startswith("_")} for q in req["queries"]])
    with ThreadPoolExecutor(len(servers)) as ex:
        return drop_noise(req, list(ex.map(lambda s: post(s[1], "/check", body), servers)))


def agrees(r):
    return "error" not in r and r.get("diff", {"agree": True})["agree"]


def report(servers, resps):
    scalac_printed = False
    for (name, _), r in zip(servers, resps):
        if "error" in r:
            print(f"== {name}: ERROR\n{r['error']}")
            continue
        ij = r["intellij"]
        sc = r.get("scalac")
        head = f"== {name} ({r['server']['commit']}): intellij {ij['ms']} ms"
        if r["setupMs"]:
            head += f" (+{r['setupMs']} ms project setup)"
        if sc:
            head += f", scalac {sc['version']} {sc['ms']} ms"
        print(head)
        if sc and not scalac_printed:
            scalac_printed = True
            if sc.get("crash"):
                print(f"  scalac CRASH {sc['crash']}")
            for d in sc["diagnostics"]:
                print(f"  scalac {d['severity']}: {fmt(d)}")
        diff = r.get("diff")
        if diff is None:
            for d in ij["diagnostics"]:
                print(f"  intellij {d['severity']}: {fmt(d)}")
        elif diff["agree"]:
            print(f"  errors agree with scalac ({sum(d['severity'] == 'error' for d in ij['diagnostics'])})")
        else:
            for d in diff["intellijOnly"]:
                print(f"  INTELLIJ-ONLY {fmt(d)}")
            for d in diff["scalacOnly"]:
                print(f"  SCALAC-ONLY   {fmt(d)}")
        for m in ij["loggedErrors"]:
            print(f"  LOGGED {m[:300]}")

    answered = [(n, r) for (n, _), r in zip(servers, resps) if "queries" in r]
    if answered:
        print("== queries")
        for i, q in enumerate(answered[0][1]["queries"]):
            ops = " ; ".join(q[k] for k in ("a", "b", "cls") if k in q)
            def cell(n, rq):
                same = rq.get("scalacSameType")
                mark = "" if same is None else " ≡" if same == "true" else " ≢" if same == "false" else " ?"
                return f"{n}={rq['intellij']}{mark}"
            cells = [cell(n, r["queries"][i]) for n, r in answered]
            sc = q.get("scalac")
            marks = "".join("·" if r["queries"][i].get("agree", True) else "≠" for _, r in answered)
            print(f"  {marks} {q['kind']} {ops}: " + "  ".join(cells) + (f"  scalac={sc}" if sc is not None else ""))


def summary(r):
    """`ok`, or what disagrees: `+i/-s` errors only in IntelliJ / only in scalac, `q≠n`, `LOG`, `ERR`."""
    if "error" in r:
        return "ERR"
    parts = []
    d = r.get("diff")
    if d and not d["agree"]:
        parts.append(f"+{len(d['intellijOnly'])}/-{len(d['scalacOnly'])}")
    if (r.get("scalac") or {}).get("crash"):
        parts.append("SCALAC-CRASH")
    qd = sum(1 for q in r.get("queries", []) if q.get("agree") is False)
    if qd:
        parts.append(f"q≠{qd}")
    if r["intellij"]["loggedErrors"]:
        parts.append("LOG")
    return ",".join(parts) or "ok"


def main():
    ap = make_parser()
    a = ap.parse_args()
    servers = [(s, SERVERS.get(s) or int(s)) for s in a.server.split(",")]
    if a.health or a.shutdown:
        for name, port in servers:
            print(name, json.dumps(post(port, "/shutdown" if a.shutdown else "/health", {} if a.shutdown else None, timeout=5)))
        return

    if a.each:
        paths = []
        for p in a.files:
            paths += sorted(os.path.join(p, f) for f in os.listdir(p) if f.endswith(".scala")) if os.path.isdir(p) else [p]
        bad = 0
        for p in paths:
            text = open(p).read()
            fa = ap.parse_args(sys.argv[1:] + file_options(text))
            extra = [os.path.join(os.path.dirname(p), w) for w in fa.with_ if w not in a.with_] + a.with_
            files = [{"path": os.path.basename(p), "text": text}] + [{"path": os.path.basename(w), "text": open(w).read()} for w in extra]
            resps = run(servers, request(fa, files))
            line = "  ".join(f"{n}={summary(r)}" for (n, _), r in zip(servers, resps))
            ok = all(summary(r) == "ok" for r in resps)
            bad += not ok
            print(f"{'  ' if ok else '✗ '}{p}: {line}", flush=True)
            for (n, _), r in zip(servers, resps):
                if "error" in r:
                    print(f"    {n}: {r['error'].strip().splitlines()[0][:300]}")
            if a.verbose and not ok:
                report(servers, resps)
        print(f"{bad}/{len(paths)} disagree")
        sys.exit(1 if bad else 0)

    files = [{"path": os.path.basename(f), "text": open(f).read()} for f in a.files + a.with_]
    files += [{"path": os.path.basename(w), "text": open(os.path.join(os.path.dirname(f), w)).read()}
              for f in a.files for w in ap.parse_args(file_options(open(f).read())).with_]
    files += [{"path": f"Snippet{i}.scala", "text": t} for i, t in enumerate(a.expr)]
    if not files:
        ap.error("no files")
    a = ap.parse_args(sys.argv[1:] + [o for f in files for o in file_options(f["text"]) if not o.startswith("--with")])
    resps = run(servers, request(a, files))
    if a.json:
        print(json.dumps(dict(zip([n for n, _ in servers], resps)), indent=2))
        return
    report(servers, resps)
    sys.exit(0 if all(agrees(r) for r in resps) else 1)


if __name__ == "__main__":
    main()
