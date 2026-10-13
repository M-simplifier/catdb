"""A small real-server smoke test. Standard library only."""
import json, pathlib, socket, subprocess, sys, tempfile, time, urllib.request, urllib.error

exe = str(pathlib.Path(sys.argv[1]).resolve())
assets = str(pathlib.Path(sys.argv[2]).resolve())
with socket.socket() as sock:
    sock.bind(("127.0.0.1", 0))
    port = sock.getsockname()[1]
base = f"http://127.0.0.1:{port}/api/"
def request(path, body=None):
    data = None if body is None else json.dumps(body).encode()
    req = urllib.request.Request(base + path, data, {"Content-Type": "application/json", "X-CatDB-Local": "1"})
    with urllib.request.urlopen(req, timeout=10) as r:
        return r.read()
def value(path, body=None):
    return json.loads(request(path, body))
def start(path):
    proc = subprocess.Popen([exe,str(path),"--assets",assets,"--port",str(port),"--demo"],
                            stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    for _ in range(100):
        if proc.poll() is not None:
            raise RuntimeError(proc.stderr.read().decode())
        try:
            value("state")
            return proc
        except (OSError, urllib.error.URLError):
            time.sleep(.1)
    proc.kill()
    raise RuntimeError("Server did not become ready")
def stop(proc):
    proc.terminate()
    try: proc.wait(timeout=5)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait()
    proc.stderr.close()
with tempfile.TemporaryDirectory() as folder:
    db=pathlib.Path(folder)/"workspace.catdb"
    proc=start(db)
    try:
        assert value("state")["workspace"]["stats"]["people"]==5
        value("command",{"action":"link","left":"Team directory/2","right":"Contractors/2"})
        assert value("state")["workspace"]["stats"]["people"]==4
        try:
            value("command",{"action":"assign","ticket":"REQ-101","record":"Team directory/2"})
            raise AssertionError("Cross-team assignment was accepted")
        except urllib.error.HTTPError as error:
            assert error.code==400
        value("command",{"action":"assign","ticket":"REQ-101","record":"Team directory/1"})
        handoff=request("public")
        assert b"Maya Chen" in handoff and b"@example.test" not in handoff and b"Noah Quinn" not in handoff
        backup=pathlib.Path(folder)/"backup.catdb"
        backup.write_bytes(request("backup"))
        assert backup.read_bytes().startswith(b"CATTX001")
    finally: stop(proc)
    for path in [db,backup]:
        proc=start(path)
        try:
            state=value("state")["workspace"]
            assert state["stats"]["people"]==4
            assert next(t for t in state["tickets"] if t["id"]=="REQ-101")["assignedRecord"]=="Team directory/1"
        finally: stop(proc)
print("PASS: HTTP workflow, rejected assignment, handoff, reopen, native backup")
