"""Generate fictional release samples through the ordinary native Studio API."""
import argparse
import json
import pathlib
import queue
import subprocess
import tempfile
import threading
import urllib.request


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("executable", type=pathlib.Path)
    parser.add_argument("assets", type=pathlib.Path)
    parser.add_argument("output", type=pathlib.Path)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    names = ["reading-notes.catdb", "mapping-example.catdb", "field-service-source.catdb", "field-service-complete.catdb"]
    if any((args.output / name).exists() for name in names):
        raise SystemExit("Choose a fresh sample directory. Existing samples are never replaced.")
    with tempfile.TemporaryDirectory(prefix="catdb-release-samples-") as temp:
        process = subprocess.Popen([str(args.executable.resolve()), "--assets", str(args.assets.resolve()), "--data-dir", temp], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, encoding="utf-8")
        lines = queue.Queue()
        threading.Thread(target=lambda: [lines.put(line) for line in process.stdout], daemon=True).start()
        try:
            url = None
            for _ in range(30):
                try:
                    line = lines.get(timeout=0.5)
                except queue.Empty:
                    if process.poll() is not None:
                        raise RuntimeError(process.stderr.read())
                    continue
                if line.startswith("Open "):
                    url = line[5:].strip()
                    break
            if not url:
                raise RuntimeError("Studio did not announce a local endpoint.")
            base, token = url.split("#")

            def request(path, data=None):
                body = None if data is None else json.dumps(data).encode()
                req = urllib.request.Request(base + path, body, {"X-CatDB-Session": token, "Content-Type": "application/json"})
                with urllib.request.urlopen(req, timeout=35) as response:
                    return response.read()

            def command(tag, **fields):
                version = json.loads(request("api/state"))["version"]
                result = json.loads(request("api/command", dict(tag=tag, expected=version, **fields)))
                if not result["ok"]:
                    raise RuntimeError(result)

            def save(name):
                with (args.output / name).open("xb") as sample:
                    sample.write(request("api/backup"))

            for preset, name in [("reading", names[0]), ("families", names[1]), ("field-service", names[2])]:
                command("create", path=name, preset=preset)
                save(name)
            command("fieldStage", stage="all")
            save(names[3])
            command("close")
            print("Prepared four fictional native samples, including the actually computed field-service result.")
        finally:
            process.terminate()
            process.wait(timeout=10)


if __name__ == "__main__":
    main()
