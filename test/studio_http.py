"""Real Studio/native-store workflow. No browser or network service dependencies."""
import copy
import hashlib
import json
import pathlib
import queue
import subprocess
import sys
import tempfile
import threading
import time
import urllib.error
import urllib.request


def run(executable, assets, legacy=None, verify_frontend=True):
    with tempfile.TemporaryDirectory(prefix="catdb-studio-") as folder:
        root = pathlib.Path(folder)
        process = subprocess.Popen(
            [str(executable), "--assets", str(assets), "--data-dir", str(root)],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, encoding="utf-8",
        )
        lines = queue.Queue()
        threading.Thread(target=lambda: [lines.put(line) for line in process.stdout], daemon=True).start()
        try:
            deadline = time.monotonic() + 15
            url = None
            while time.monotonic() < deadline:
                try:
                    line = lines.get(timeout=0.5)
                except queue.Empty:
                    if process.poll() is not None:
                        raise AssertionError(process.stderr.read())
                    continue
                if line.startswith("Open http://"):
                    url = line[5:].strip()
                    break
            assert url, "Studio did not announce its HTTP endpoint"
            base, token = url.split("#", 1)

            def http(path, data=None, *, session=True, origin=None, extra=None):
                headers = {"X-CatDB-Session": token} if session else {}
                if origin:
                    headers["Origin"] = origin
                if extra:
                    headers.update(extra)
                if data is not None:
                    headers["Content-Type"] = "application/json"
                    data = json.dumps(data, ensure_ascii=False).encode()
                request = urllib.request.Request(base + path.lstrip("/"), data, headers)
                try:
                    with urllib.request.urlopen(request, timeout=35) as response:
                        return response.status, response.headers, response.read()
                except urllib.error.HTTPError as failure:
                    return failure.code, failure.headers, failure.read()

            def state():
                status, _, body = http("api/state")
                assert status == 200, body
                return json.loads(body)

            def command(tag, ok=True, **fields):
                fields.setdefault("expected", state()["version"])
                status, _, body = http("api/command", dict(tag=tag, **fields))
                assert status == 200, (status, body)
                result = json.loads(body)
                assert result["ok"] is ok, result
                return result

            def inspect(revision):
                return command("inspect", source=revision)["snapshot"]

            def ref(revision, obj, record):
                return dict(revision=revision, object=obj, record=record)

            def explain(revision, obj, record):
                return command("explain", ref=ref(revision, obj, record))["explanation"]

            def rows(snapshot, obj):
                return next(table["rows"] for table in snapshot["data"] if table["object"] == obj)

            def set_field(data, obj, record, arrow, target):
                table = next(t for t in data if t["object"] == obj)
                row = next(r for r in table["rows"] if r["id"] == record)
                row["fields"] = [[a, target if a == arrow else b] for a, b in row["fields"]]

            assert http("api/state", session=False)[0] == 403
            assert http("api/state", origin="https://example.invalid")[0] == 403
            navigation={"Sec-Fetch-Site":"cross-site","Sec-Fetch-Mode":"navigate","Sec-Fetch-Dest":"document"}
            assert http("",session=False,extra=navigation)[0] == 200
            assert http("index.html",session=False,origin="https://example.invalid",extra=navigation)[0] == 200
            assert http("",session=False,extra=dict(navigation,Host="example.invalid"))[0] == 403
            assert http("api/state",extra=navigation)[0] == 403
            assert http("api/command",{"tag":"cancel"},extra=navigation)[0] == 403
            assert http("studio-gui.wasm",session=False,extra=navigation)[0] == 403
            if verify_frontend:
                status, headers, body = http("studio-gui.wasm", session=False)
                assert status == 200 and body[:4] == b"\0asm", "Compiled Miso WASM is not being served"
                assert "wasm" in headers.get_content_type()
                assert http("index.js", session=False)[0] == 200
                print("PASS session/origin protection and real compiled WASM serving")
            else:
                assert http("index.html", session=False)[0] == 200
                print("PASS session/origin protection (native backend; fixture assets, no Miso/WASM check)")

            command("create", path="読書メモ.catdb", preset="reading")
            initial = inspect("r1")
            delta = command("run", name="cards", operation="delta", mapping="cards", source="r1", system="reading")["snapshot"]
            assert dict(dict(delta["labels"])["Card"])["n1"] == dict(dict(initial["labels"])["Note"])["n1"]
            assert len(rows(delta, "Card")) == 3
            assert [dict(r["fields"])["volume"] for r in rows(delta, "Card")] == ["b1", "b2", "b1"]
            witness = explain(delta["revision"], "Card", "n1")
            assert witness["kind"] == "PullRef" and witness["origins"][0]["ref"] == ref("r1", "Note", "n1")
            print("PASS real Delta preserves distinct cards and original-revision PullRef")

            before = http("api/backup")[2]
            before_size = (root / "読書メモ.catdb").stat().st_size
            version = state()["version"]
            bad = copy.deepcopy(initial["data"])
            set_field(bad, "Note", "n1", "book", "b2")
            rejected = command("preview", ok=False, source="r1", data=bad)
            assert rejected["problem"]["code"] == "EquationViolation", rejected
            rejected = command("apply", ok=False, system="reading", source="r1", data=bad)
            assert rejected["problem"]["code"] == "EquationViolation"
            assert state()["version"] == version and http("api/backup")[2] == before
            assert (root / "読書メモ.catdb").stat().st_size == before_size
            assert inspect("r1")["data"] == initial["data"]
            print("PASS bad n1.book draft rejected without a committed-data or file change")

            good = copy.deepcopy(bad)
            set_field(good, "Note", "n1", "quote", "q2")
            command("preview", source="r1", data=good)
            updated = command("apply", system="reading", source="r1", data=good, reason="本と引用を一括変更")["snapshot"]
            assert updated["data"] == good and inspect("r1")["data"] == initial["data"]
            assert next(v for v in state()["views"] if v["key"] == "cards")["freshness"] == "stale"
            assert inspect(delta["revision"])["data"] == delta["data"]
            refreshed = command("recompute", name="cards")["snapshot"]
            assert dict(rows(refreshed, "Card")[0]["fields"])["volume"] == "b2"
            assert dict(dict(refreshed["labels"])["Card"])["n1"] == dict(dict(initial["labels"])["Note"])["n1"]
            stale = command("apply", ok=False, expected=version, system="reading", source="r1", data=good)
            assert stale["problem"]["code"] == "StaleRevision"
            print("PASS atomic repair, immutable source/result snapshots, stale marker and explicit recomputation")

            invalid_mapping = dict(key="broken", **{"from": "cards", "to": "reading"}, objects=[["Card", "Note"], ["Book", "Book"]], arrows=[["volume", dict(start="Note", steps=["quote"])]] )
            before_check = http("api/backup")[2]
            command("checkMapping", ok=False, mapping=invalid_mapping)
            assert http("api/backup")[2] == before_check
            command("mapping", ok=False, mapping=invalid_mapping)
            direct_mapping = copy.deepcopy(invalid_mapping)
            direct_mapping["key"] = "direct"
            direct_mapping["arrows"][0][1]["steps"] = ["book"]
            checked_version = state()["version"]
            command("checkMapping", mapping=direct_mapping)
            assert state()["version"] == checked_version and http("api/backup")[2] == before_check
            command("mapping", mapping=direct_mapping)
            direct = command("run", name="direct-cards", operation="delta", mapping="direct", source="r1")["snapshot"]
            assert direct["data"] == delta["data"]
            print("PASS checked mapping rejection and equal path spellings give matching Delta data")

            latest = dict(state()["heads"])["reading"]
            command("backup", path="copy.catdb")
            backup = http("api/backup")[2]
            assert backup == (root / "copy.catdb").read_bytes()
            exported = json.loads(http("api/export")[2])
            assert exported["format"] == "catdb-studio-definitions-v1" and "r1" in exported["instances"]
            command("close")
            command("open", path="読書メモ.catdb")
            assert dict(state()["heads"])["reading"] == latest
            assert inspect(latest)["data"] == good
            assert explain(delta["revision"], "Card", "n1")["origins"][0]["ref"]["revision"] == "r1"
            assert len(state()["mappings"]) == 2 and len(state()["views"]) == 2
            print("PASS native backup, definition export, catalog persistence and close/reopen")

            # A schema that is unrelated to either fixture, with total references and deletion constraints.
            custom_schema = dict(objects=["Team", "Office"], arrows=[dict(id="office", **{"from":"Team", "to":"Office"})], equations=[])
            before_check = http("api/backup")[2]
            checked_version = state()["version"]
            command("checkSchema", name="custom", schema=custom_schema)
            assert state()["version"] == checked_version and http("api/backup")[2] == before_check
            cycle = copy.deepcopy(custom_schema)
            cycle["arrows"].append(dict(id="return", **{"from":"Office", "to":"Team"}))
            assert command("checkSchema", ok=False, name="cycle", schema=cycle)["problem"]["code"] == "CyclicSchema"
            bad_equation = copy.deepcopy(custom_schema)
            bad_equation["equations"] = [dict(left=dict(start="Team", steps=["missing"]), right=dict(start="Team", steps=["office"]))]
            command("checkSchema", ok=False, name="bad-equation", schema=bad_equation)
            assert state()["version"] == checked_version and http("api/backup")[2] == before_check
            print("PASS real definition checks reject cycles/invalid paths without changing catalog or file")
            created = command("schema", name="custom", schema=custom_schema)["snapshot"]
            custom_data = [dict(object="Team", rows=[dict(id="alpha", fields=[["office", "tokyo"]])]),dict(object="Office", rows=[dict(id="tokyo", fields=[])])]
            custom = command("apply", system="custom", source=created["revision"], data=custom_data)["snapshot"]
            assert all(not labels for _, labels in custom["labels"])
            dangling = copy.deepcopy(custom_data)
            dangling[1]["rows"] = []
            failure = command("apply", ok=False, system="custom", source=custom["revision"], data=dangling)
            assert failure["problem"]["code"] == "ReferenceViolation"
            assert inspect(custom["revision"])["data"] == custom_data
            duplicate = copy.deepcopy(custom_data)
            duplicate[0]["rows"].append(copy.deepcopy(duplicate[0]["rows"][0]))
            assert command("preview", ok=False, source=custom["revision"], data=duplicate)["problem"]["code"] == "InvalidInput"
            print("PASS custom schema, generic records, total-arrow deletion rejection and duplicate admission")

            command("create", path="families.catdb", preset="families")
            sigma = command("run", name="merged", operation="sigma", mapping="by-key", source="r1", system="families")["snapshot"]
            pi = command("run", name="compatible", operation="pi", mapping="by-key", source="r1", system="families")["snapshot"]
            assert len(rows(sigma, "Result")) == 2 and len(rows(pi, "Result")) == 2
            sigma_row = rows(sigma, "Result")[0]["id"]
            pi_row = rows(pi, "Result")[0]["id"]
            sigma_witness = explain(sigma["revision"], "Result", sigma_row)
            pi_witness = explain(pi["revision"], "Result", pi_row)
            assert sigma_witness["kind"] == "SigmaClass" and len(sigma_witness["origins"]) == 3
            assert pi_witness["kind"] == "PiFamily" and len(pi_witness["origins"]) == 3
            for origin in pi_witness["origins"]:
                r = origin["ref"]
                assert r["revision"] == "r1" and any(row["id"] == r["record"] for row in rows(inspect("r1"), r["object"]))
            part = command("generate", name="one-family", source=pi["revision"], object="Result", record=pi_row)["snapshot"]
            generated = explain(part["revision"], "Result", pi_row)
            assert generated["kind"] == "GeneratedRef" and generated["origins"][0]["ref"]["revision"] == pi["revision"]
            assert dict(dict(part["labels"])["Result"])[pi_row] == dict(dict(pi["labels"])["Result"])[pi_row]
            command("close")
            command("open", path="families.catdb")
            assert explain(pi["revision"], "Result", pi_row) == pi_witness
            print("PASS real Sigma classes, Pi compatible families, GeneratedRef and persisted original witnesses")

            empty = command("schema", name="empty", schema=dict(objects=[], arrows=[], equations=[]))["snapshot"]
            command("schema", name="unit", schema=dict(objects=["Unit"], arrows=[], equations=[]))
            command("mapping", mapping=dict(key="empty-to-unit", **{"from":"empty", "to":"unit"}, objects=[], arrows=[]))
            empty_pi = command("run", name="empty-family", operation="pi", mapping="empty-to-unit", source=empty["revision"])["snapshot"]
            assert len(rows(empty_pi, "Unit")) == 1
            witness = explain(empty_pi["revision"], "Unit", rows(empty_pi, "Unit")[0]["id"])
            assert witness["kind"] == "PiFamily" and witness["origins"] == []
            print("PASS empty Pi family is represented explicitly")

            # A genuinely bounded search, cancelled through a concurrent HTTP request.
            objects = [f"Part{n}" for n in range(12)]
            wide = command("schema", name="wide", schema=dict(objects=objects, arrows=[], equations=[]))["snapshot"]
            wide_data = [dict(object=o, rows=[dict(id=str(n), fields=[]) for n in range(12)]) for o in objects]
            wide = command("apply", system="wide", source=wide["revision"], data=wide_data)["snapshot"]
            command("mapping", mapping=dict(key="wide-to-unit", **{"from":"wide", "to":"unit"}, objects=[[o,"Unit"] for o in objects], arrows=[]))
            before = http("api/backup")[2]
            result_box = []
            def long_request():
                try:
                    result_box.append(command("run", ok=False, name="cancelled-search", operation="pi", mapping="wide-to-unit", source=wide["revision"]))
                except Exception as failure:
                    result_box.append(failure)
            worker = threading.Thread(target=long_request)
            worker.start()
            saw_busy = False
            for _ in range(100):
                if state()["busy"]:
                    saw_busy = True
                    status, _, cancel_body = http("api/command", {"tag":"cancel"})
                    assert status == 200
                    cancel_result = json.loads(cancel_body)
                    break
                if not worker.is_alive():
                    break
                time.sleep(0.002)
            worker.join(timeout=35)
            assert not worker.is_alive() and result_box and not isinstance(result_box[0], Exception), result_box
            cancelled = result_box[0]["problem"]["code"] == "Cancelled"
            assert result_box[0]["problem"]["code"] in ["Cancelled", "BudgetExceeded"], result_box
            if cancelled:
                assert saw_busy and cancel_result["ok"]
            assert http("api/backup")[2] == before and not state()["busy"]
            bounded = command("run", ok=False, name="bounded-search", operation="pi", mapping="wide-to-unit", source=wide["revision"])
            assert bounded["problem"]["code"] == "BudgetExceeded", bounded
            assert http("api/backup")[2] == before and all(v["key"] not in ["cancelled-search", "bounded-search"] for v in state()["views"])
            print("PASS concurrent cancellation saves no partial result" if cancelled else "NOTE bounded search finished before cancellation; cancellation race not exercised")
            print("PASS real Pi budget failure saves no partial result")

            if legacy:
                original = root / "legacy.catdb"
                original.write_bytes(pathlib.Path(legacy).read_bytes())
                source_hash = hashlib.sha256(original.read_bytes()).digest()
                command("open", path=str(original))
                assert state()["legacy"] is not None
                command("adoptLegacy", path="adopted.catdb", name="imported")
                assert state()["legacy"] is None and dict(state()["heads"])["imported"] == "r1"
                assert hashlib.sha256(original.read_bytes()).digest() == source_hash
                print("PASS explicit legacy adoption preserves the original native file")
            print("All Studio HTTP checks passed.")
        finally:
            process.terminate()
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()


if __name__ == "__main__":
    if len(sys.argv) < 3:
        raise SystemExit("studio_http.py CATDB-STUDIO-EXECUTABLE (COMPILED-WEB | --api-only) [LEGACY.catdb]")
    executable = pathlib.Path(sys.argv[1]).resolve()
    legacy = sys.argv[3] if len(sys.argv)>3 else None
    if sys.argv[2] == "--api-only":
        # Carrier assets let the normal native launcher start. They are never
        # presented as the real frontend and perform no application logic.
        with tempfile.TemporaryDirectory(prefix="catdb-studio-http-assets-") as folder:
            assets = pathlib.Path(folder)
            (assets / "index.html").write_text("<!doctype html><title>Studio backend test fixture</title>", encoding="utf-8")
            (assets / "studio-gui.wasm").write_bytes(b"\0asm\1\0\0\0")
            run(executable, assets, legacy, verify_frontend=False)
    else:
        run(executable, pathlib.Path(sys.argv[2]).resolve(), legacy)
