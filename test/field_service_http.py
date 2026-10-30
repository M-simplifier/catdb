"""Field-service consumer checks; the dispatch oracle is ordinary domain enumeration."""
import argparse
import copy
import json
import pathlib
import queue
import subprocess
import tempfile
import threading
import urllib.error
import urllib.request


def tables(snapshot):
    return {t["object"]:{r["id"]:dict(r["fields"]) for r in t["rows"]} for t in snapshot["data"]}


def expected_dispatches(snapshot):
    """No CatDB migration/path planner is used here."""
    data = tables(snapshot)
    choices = set()
    for ticket, requirement in data["依頼"].items():
        customer = data["サポート顧客"][requirement["ticketCustomer"]]["supportKey"]
        region = data["顧客キー"][customer]["customerRegion"]
        for qualification, skill in data["資格"].items():
            if skill["qualificationProduct"] != requirement["ticketProduct"]:
                continue
            for availability, available in data["稼働枠"].items():
                if (available["availabilityEngineer"] == skill["qualificationEngineer"]
                    and available["availabilitySlot"] == requirement["ticketSlot"]
                    and available["availabilityRegion"] == region):
                    choices.add((ticket, qualification, availability))
    return choices


def actual_dispatches(course):
    return {(r["ticket"],r["qualification"],r["availability"]) for r in course["runs"][0]["rows"]}


def run(executable, assets, prepare):
    with tempfile.TemporaryDirectory(prefix="catdb-field-service-") as folder:
        process = subprocess.Popen([str(executable),"--assets",str(assets),"--data-dir",folder],
            stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True,encoding="utf-8")
        output=queue.Queue()
        threading.Thread(target=lambda:[output.put(line) for line in process.stdout],daemon=True).start()
        try:
            url=None
            for _ in range(30):
                try: line=output.get(timeout=0.5)
                except queue.Empty:
                    assert process.poll() is None, process.stderr.read()
                    continue
                if line.startswith("Open http://"):
                    url=line[5:].strip(); break
            assert url
            base,token=url.split("#")
            def request(path,payload=None):
                headers={"X-CatDB-Session":token}
                if payload is not None: headers["Content-Type"]="application/json"
                req=urllib.request.Request(base+path, None if payload is None else json.dumps(payload,ensure_ascii=False).encode(),headers)
                with urllib.request.urlopen(req,timeout=35) as response: return response.read()
            def state(): return json.loads(request("api/state"))
            def command(tag,ok=True,**fields):
                fields.setdefault("expected",state()["version"])
                result=json.loads(request("api/command",dict(tag=tag,**fields)))
                assert result["ok"] is ok,result
                return result
            def inspect(rev): return command("inspect",source=rev)["snapshot"]
            def explain(ref): return command("explain",ref=ref)["explanation"]
            def save_sample(name):
                if prepare:
                    prepare.mkdir(parents=True,exist_ok=True)
                    path=prepare/name
                    assert not path.exists(),str(path)
                    path.write_bytes(request("api/backup"))
            def set_field(data,obj,row,arrow,value):
                record=next(r for t in data if t["object"]==obj for r in t["rows"] if r["id"]==row)
                record["fields"]=[[a,value if a==arrow else v] for a,v in record["fields"]]
            def raw(): return inspect(state()["course"]["source"])
            command("create",path="field-service.catdb",preset="field-service")
            source=raw(); assert len(tables(source)["依頼"])==12 and not state()["course"]["runs"]
            save_sample("field-service-source.catdb")
            sigma=command("fieldStage",stage="sigma")["snapshot"]
            sd=tables(sigma)
            assert len(sd["顧客"])==6 and len(sd["契約"])==12 and len(sd["依頼"])==12
            key_rows={}
            for key in sd["顧客"]:
                origin=explain(dict(revision=sigma["revision"],object="顧客",record=key))
                assert origin["kind"]=="SigmaClass"
                original_keys=[o["ref"]["record"] for o in origin["origins"] if o["ref"]["object"]=="顧客キー"]
                assert len(original_keys)==1
                key_rows[original_keys[0]]=key
            assert key_rows["K01｜青葉メディカル"]!=key_rows["K02｜青葉メディカルサービス・別法人"]
            print("PASS Sigma explicitly identifies aliases into six customers and retains twelve contracts/tickets")
            delta=command("fieldStage",stage="delta")["snapshot"]
            for key,row in tables(delta)["依頼"].items():
                normalized_ticket=sd["依頼"][key]
                assert row["tr"]==sd["顧客"][normalized_ticket["ticketCustomer"]]["customerRegion"]
                assert row["tp"]==normalized_ticket["ticketProduct"]
            print("PASS Delta really reads the composed customer-to-region path")
            pi=command("fieldStage",stage="pi")["snapshot"]
            course=state()["course"]
            assert actual_dispatches(course)==expected_dispatches(source)
            assert len(actual_dispatches(course))==13
            assert dict(course["ticketCounts"])["依頼01｜青葉の定期点検"]==2
            assert dict(course["ticketCounts"])["依頼11｜森のケアのセンサー交換"]==0
            assert dict(course["ticketCounts"])["依頼12｜北辰の冷却装置修理"]==0
            assert [c["name"] for c in course["nearMiss"] if not c["matches"]]==["製品"]
            near=("依頼02｜青葉のセンサー交換","資格03｜佐藤・ポンプ","稼働03｜佐藤・東・月曜午前")
            assert near not in actual_dispatches(course)
            first=course["runs"][0]["rows"][0]
            witness=explain(first["row"])
            assert witness["kind"]=="PiFamily" and len(witness["origins"])==7
            for origin in first["origins"]:
                assert origin["revision"]==source["revision"]
                assert origin["record"] in tables(inspect(origin["revision"]))[origin["object"]]
            for arrow,projection in first["projections"]:
                assert tables(pi)["候補"][first["row"]["record"]][arrow]==projection["record"]
            save_sample("field-service-complete.catdb")
            print("PASS Pi = independent feasible-triple oracle; two-candidate and no-candidate cases; one-condition near miss")
            initial_pi=copy.deepcopy(pi)
            command("close"); command("open",path="field-service.catdb")
            assert state()["course"]["runs"][0]==course["runs"][0]
            added=copy.deepcopy(source["data"])
            next(t for t in added if t["object"]=="資格")["rows"].append(dict(id="資格13｜佐藤・センサー",fields=[["qualificationEngineer","佐藤"],["qualificationProduct","センサー"]]))
            version=state()["version"]
            command("preview",source=source["revision"],data=added)
            updated=command("apply",system="現場サービス原本",source=source["revision"],data=added)["snapshot"]
            assert all(v["freshness"]=="stale" for v in state()["views"])
            assert all(v["capturedSource"]==source["revision"] for v in state()["views"])
            command("fieldStage",ok=False,stage="delta")
            assert command("fieldStage",ok=False,stage="all",expected=version)["problem"]["code"]=="StaleRevision"
            command("fieldStage",stage="all")
            after=state()["course"]
            assert actual_dispatches(after)==expected_dispatches(updated) and len(actual_dispatches(after))==15
            assert all(v["capturedSource"]==updated["revision"] for v in state()["views"])
            assert dict(after["ticketCounts"])["依頼11｜森のケアのセンサー交換"]==1
            assert after["runs"][1]["revision"]==initial_pi["revision"]
            assert inspect(initial_pi["revision"])["data"]==initial_pi["data"]
            print("PASS source change marks all three stages stale; atomic pipeline rerun gives fifteen candidates and preserves old run")
            broken=copy.deepcopy(updated["data"])
            set_field(broken,"依頼","依頼01｜青葉の定期点検","ticketProduct","センサー")
            before=request("api/backup")
            rejection=command("preview",ok=False,source=updated["revision"],data=broken)
            assert rejection["problem"]["code"]=="EquationViolation" and request("api/backup")==before
            set_field(broken,"依頼","依頼01｜青葉の定期点検","ticketContract","契約02｜青葉・センサー")
            repaired=command("apply",system="現場サービス原本",source=updated["revision"],data=broken)["snapshot"]
            command("fieldStage",stage="all")
            final=state()["course"]
            assert actual_dispatches(final)==expected_dispatches(repaired)
            candidate=final["runs"][0]["rows"][0]
            part=command("generate",name="持ち出す一候補",source=final["runs"][0]["revision"],object="候補",record=candidate["row"]["record"])["snapshot"]
            assert sum(len(t["rows"]) for t in part["data"])==8
            assert explain(dict(revision=part["revision"],object="候補",record=candidate["row"]["record"]))["kind"]=="GeneratedRef"
            exported=json.loads(request("api/export")); assert len(exported["mappings"])==3
            command("close"); command("open",path="field-service.catdb")
            assert state()["course"]["runs"][0]==final["runs"][0]
            print("PASS rejected inconsistent draft, explicit atomic repair, generated closed eight-row part, export and reopen")
            print(json.dumps({"initialCandidates":[[r["ticket"],r["engineer"]] for r in course["runs"][0]["rows"]],"afterQualification":len(after["runs"][0]["rows"]),"afterRepair":len(final["runs"][0]["rows"])},ensure_ascii=True))
        finally:
            process.terminate()
            try: process.wait(timeout=10)
            except subprocess.TimeoutExpired: process.kill(); process.wait()


if __name__=="__main__":
    parser=argparse.ArgumentParser()
    parser.add_argument("executable",type=pathlib.Path)
    parser.add_argument("assets",type=pathlib.Path,nargs="?")
    parser.add_argument("--api-only",action="store_true")
    parser.add_argument("--prepare-dir",type=pathlib.Path)
    args=parser.parse_args()
    if args.api_only:
        with tempfile.TemporaryDirectory(prefix="catdb-field-http-assets-") as folder:
            assets=pathlib.Path(folder)
            (assets/"index.html").write_text("<!doctype html><title>Field service backend fixture</title>",encoding="utf-8")
            (assets/"studio-gui.wasm").write_bytes(b"\0asm\1\0\0\0")
            print("Native backend check: fixture assets, no Miso/WASM assertion")
            run(args.executable.resolve(),assets,args.prepare_dir)
    elif args.assets:
        run(args.executable.resolve(),args.assets.resolve(),args.prepare_dir)
    else:
        parser.error("provide compiled assets or --api-only")
