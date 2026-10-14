#!/usr/bin/env python3
"""Client for the type-checking harness (TcServerTest). See README.md.

  tc.py [options] FILE... | -e CODE
  tc.py --health

Sends the files to one or more harness servers and prints IntelliJ's errors against scalac's,
plus the answers to queries. With several servers (default: tip and base) it also shows where
the servers disagree with each other.
"""
import argparse, json, os, sys, urllib.request, urllib.error
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
    except (urllib.error.URLError, ConnectionError) as e:
        return {"error": f"cannot reach port {port}: {e}"}


def parse_query(s):
    parts = s.split()
    kinds = {"C": "conforms", "E": "equiv", "B": "baseType", "T": "type"}
    if not parts or parts[0] not in kinds:
        sys.exit(f"bad query {s!r}: use 'C a b', 'E a b', 'B a Cls' or 'T a'")
    k = kinds[parts[0]]
    if k == "type":
        return {"kind": k, "a": parts[1]}
    if k == "baseType":
        return {"kind": k, "a": parts[1], "cls": parts[2]}
    return {"kind": k, "a": parts[1], "b": parts[2]}


def file_queries(text):
    """`// ? C a b` lines in a source file are queries (the type PBT's replay format)."""
    return [parse_query(l.strip()[5:]) for l in text.splitlines() if l.strip().startswith("// ? ")]


def fmt(d):
    return f"{d['file']}:{d['line']}:{d['col']}: {d['message']}"


def main():
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
    ap.add_argument("--warnings", action="store_true")
    ap.add_argument("--json", action="store_true", help="print raw responses")
    ap.add_argument("--health", action="store_true")
    ap.add_argument("--shutdown", action="store_true")
    a = ap.parse_args()

    servers = [(s, SERVERS.get(s) or int(s)) for s in a.server.split(",")]
    if a.health or a.shutdown:
        for name, port in servers:
            print(name, json.dumps(post(port, "/shutdown" if a.shutdown else "/health", {} if a.shutdown else None, timeout=5)))
        return

    files = [{"path": os.path.basename(f), "text": open(f).read()} for f in a.files]
    files += [{"path": f"Snippet{i}.scala", "text": t} for i, t in enumerate(a.expr)]
    if not files:
        ap.error("no files")
    queries = [parse_query(q) for q in a.query] + [q for f in files for q in file_queries(f["text"])]
    req = {"scalaVersion": a.scala, "jdk": a.jdk, "classpath": [p for p in a.cp.split(":") if p],
           "scalacOptions": a.option, "files": files, "queries": queries,
           "oracle": not a.no_oracle, "warnings": a.warnings}

    with ThreadPoolExecutor(len(servers)) as ex:
        resps = list(ex.map(lambda s: post(s[1], "/check", req), servers))
    if a.json:
        print(json.dumps(dict(zip([n for n, _ in servers], resps)), indent=2))
        return

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

    answered = [r for r in resps if "queries" in r]
    if answered:
        print("== queries")
        names = [n for (n, _), r in zip(servers, resps) if "queries" in r]
        for i, q in enumerate(answered[0]["queries"]):
            ops = " ".join(q[k] for k in ("a", "b", "cls") if k in q)
            cells = [f"{n}={r['queries'][i]['intellij']}" for n, r in zip(names, answered)]
            sc = q.get("scalac")
            marks = "".join("" if r["queries"][i].get("agree", True) else "≠" for r in answered)
            print(f"  {marks or ' ':2} {q['kind']} {ops}: " + "  ".join(cells) + (f"  scalac={sc}" if sc is not None else ""))
    sys.exit(0 if all("error" not in r and r.get("diff", {"agree": True})["agree"] for r in resps) else 1)


if __name__ == "__main__":
    main()
