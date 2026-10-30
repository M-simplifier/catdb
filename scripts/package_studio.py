"""Prepare a fresh Windows bundle and matching-source archive. Never publishes."""
import argparse
import hashlib
import io
import json
import os
import pathlib
import re
import shutil
import subprocess
import tarfile
import zipfile

VERSION = "0.3.0-poc"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo", type=pathlib.Path, required=True)
    parser.add_argument("--build", type=pathlib.Path, required=True)
    parser.add_argument("--web", type=pathlib.Path, required=True)
    parser.add_argument("--output", type=pathlib.Path, required=True)
    parser.add_argument("--ghc", type=pathlib.Path, required=True)
    parser.add_argument("--ghc-source", type=pathlib.Path, required=True)
    parser.add_argument("--cache", type=pathlib.Path, required=True)
    parser.add_argument("--field-service-samples", type=pathlib.Path)
    args = parser.parse_args()
    repo, build, web = args.repo.resolve(), args.build.resolve(), args.web.resolve()
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    field_service=args.field_service_samples is not None
    stage = output / ("app" if field_service else f"catdb-{VERSION}-windows")
    binary_zip = output / ("catdb-field-service-windows.zip" if field_service else f"catdb-{VERSION}-windows.zip")
    source_zip = output / ("catdb-field-service-corresponding-source.zip" if field_service else f"catdb-{VERSION}-corresponding-source.zip")
    if any(p.exists() for p in (stage, binary_zip, source_zip)):
        raise SystemExit("Use a fresh output directory; an existing package is never replaced.")
    if (web / "studio-gui.wasm").read_bytes()[:4] != b"\0asm":
        raise SystemExit("The compiled Miso frontend is missing.")
    for name in ["WASM_NOTICES.txt", "WASM_DEPENDENCIES.json"]:
        if not (web / "licenses" / name).is_file():
            raise SystemExit(f"Missing frontend notices: {name}")
    plan = json.loads((build / "dist/cache/plan.json").read_text())
    if plan["compiler-id"] != "ghc-9.6.7":
        raise SystemExit("This native corresponding-source recipe targets the verified GHC 9.6.7 release build.")
    units = {p["id"]:p for p in plan["install-plan"]}
    seen = set()
    def visit(key):
        if key in seen:
            return
        seen.add(key)
        for dep in units[key].get("depends", []):
            visit(dep)
    executables = ["catdb-studio"] if field_service else ["catdb-studio", "catdb", "relay-desk"]
    for p in units.values():
        if p["pkg-name"] == "catdb" and p.get("component-name") in ["exe:"+exe for exe in executables]:
            visit(p["id"])
    dependencies = sorted({(units[k]["pkg-name"],units[k]["pkg-version"]):units[k] for k in seen}.values(), key=lambda p:p["pkg-name"].lower())
    manifest, texts, archives = [], [], []
    ghc = args.ghc.resolve()
    docs = ghc / "lib/doc/x86_64-windows-ghc-9.6.7"
    ghc_license = (docs / "ghc-9.6.7/LICENSE").read_text()
    for p in dependencies:
        name, version = p["pkg-name"], p["pkg-version"]
        if name == "catdb":
            continue
        notices = []
        if p["type"] == "pre-existing":
            conf = (ghc / "lib/package.conf.d" / (p["id"]+".conf")).read_text()
            match = re.search(r"^license:\s*(.+)", conf, re.M)
            license_name = match.group(1).strip() if match else "see GHC license"
            for file in (docs / f"{name}-{version}").glob("*"):
                if file.is_file() and file.name.upper().startswith(("LICENSE", "COPYING")):
                    notices.append((file.name, file.read_text(errors="replace")))
            if not notices:
                notices = [("GHC LICENSE", ghc_license)]
        else:
            archive = args.cache / name / version / f"{name}-{version}.tar.gz"
            if not archive.is_file():
                raise SystemExit(f"Missing corresponding source archive: {name}-{version}")
            archives.append(archive)
            with tarfile.open(archive) as tar:
                cabal = tar.extractfile(f"{name}-{version}/{name}.cabal").read().decode()
                match = re.search(r"^license:\s*(.+)", cabal, re.M | re.I)
                license_name = match.group(1).strip() if match else "see license text"
                for member in tar.getmembers():
                    if member.isfile() and pathlib.PurePosixPath(member.name).name.upper().startswith(("LICENSE", "COPYING")):
                        notices.append((member.name,tar.extractfile(member).read().decode(errors="replace")))
        if not notices:
            raise SystemExit(f"Missing license text: {name}-{version}")
        manifest.append(dict(name=name,version=version,license=license_name))
        texts.append(f"\n{'='*72}\n{name} {version} — {license_name}\nhttps://hackage.haskell.org/package/{name}-{version}\n")
        texts.extend(f"\n--- {label} ---\n{body}\n" for label,body in notices)
    for kind in ["crt", "libffi", "llvm"]:
        for file in (ghc / "mingw/share/licenses" / kind).glob("*"):
            if file.is_file():
                texts.append(f"\n--- Toolchain / {kind} / {file.name} ---\n"+file.read_text(errors="replace"))
    # Exact GNU texts from the matching GMP source distributed in GHC's archive.
    with tarfile.open(args.ghc_source) as ghc_tar:
        member = next(m for m in ghc_tar.getmembers() if m.name.endswith("/gmp-tarballs/gmp-6.3.0-nodoc.tar.xz"))
        with tarfile.open(fileobj=io.BytesIO(ghc_tar.extractfile(member).read())) as gmp_tar:
            gnu = {name:gmp_tar.extractfile("gmp-6.3.0/"+name).read() for name in ["COPYINGv3", "COPYING.LESSERv3"]}
    stage.mkdir()
    for exe in executables:
        shutil.copy2(build / (exe+".exe"), stage / (exe+".exe"))
        strip = shutil.which("llvm-strip")
        installed_strip = pathlib.Path(os.environ.get("ProgramFiles", r"C:\Program Files")) / "LLVM/bin/llvm-strip.exe"
        if not strip and installed_strip.is_file():
            strip = str(installed_strip)
        if strip:
            subprocess.run([strip, str(stage / (exe+".exe"))], check=True)
    shutil.copytree(web, stage / "studio-web")
    if not field_service:
        shutil.copytree(repo / "reference/web", stage / "relay-web")
        shutil.copytree(repo / "samples", stage / "samples")
    else:
        (stage/"samples").mkdir()
        for sample in ["field-service-source.catdb","field-service-complete.catdb"]:
            shutil.copy2(args.field_service_samples/sample,stage/"samples"/sample)
    shutil.copytree(repo / "docs", stage / "docs")
    launchers=["Start Field Service.cmd","Start Completed Example.cmd"] if field_service else ["Start CatDB Studio.cmd","Start Relay Desk.cmd"]
    for name in ["LICENSE", "README.md", "STATUS.md", "THIRD_PARTY_NOTICES.md"]+launchers:
        shutil.copy2(repo / name, stage / name)
    for name, body in gnu.items():
        (stage / name).write_bytes(body)
    (stage / "THIRD_PARTY_NOTICES.txt").write_text("CatDB native Windows binary dependency notices\nOriginal CatDB source is MIT. Dependencies retain their licenses.\nGMP 6.3.0 is conveyed under LGPL-3.0-or-later; corresponding source accompanies this release.\n"+"".join(texts), encoding="utf-8")
    (stage / "DEPENDENCIES.json").write_text(json.dumps(manifest,indent=2),encoding="utf-8")
    commit = subprocess.check_output(["git","rev-parse","HEAD"],cwd=repo,text=True).strip()
    (stage / "BUILD_INFO.txt").write_text(f"CatDB {VERSION}\nSource commit: {commit}\nNative compiler: ghc-9.6.7\nFrontend: Miso 1.12.0.0, pinned flake efae40b82c1452891f78e79c139d6421983878a6\n",encoding="utf-8")
    (stage / "README.txt").write_text(f"""CatDB Studio {VERSION}

Double-click Start CatDB Studio.cmd. Your browser opens the local application.
If it does not, use the launch URL in the console. No Haskell setup is required.
Keep studio-web beside catdb-studio.exe. Ctrl+C or closing the console stops it.
A bare database filename is saved under data/ beside the program. No database is
silently created until you choose Create. Existing files are never overwritten.
Drafts are unsaved browser proposals; Apply commits a checked new revision.

日本語: Start CatDB Studio.cmdを起動してください。最初は「読書メモ」の
見本か空のDBを作れます。保存先のフルパスは「ファイルと書き出し」で確認できます。
ガイド: docs/studio.ja.md / English: docs/studio.md

Relay Desk remains an application example. Start Relay Desk.cmd launches it.
CatDB source/UI/site are MIT; dependencies retain their own licenses.
Read THIRD_PARTY_NOTICES.txt and studio-web/licenses/ for full notice texts.
Native GHC uses GMP 6.3.0, LGPL-3.0-or-later. Download the matching
catdb-{VERSION}-corresponding-source.zip for sources and rebuilding/recombination
instructions. Modification and reverse engineering to debug modifications are
permitted. This is a working PoC with the limits in STATUS.md.

https://github.com/M-simplifier/catdb
Contributions through issues only; external PRs are not accepted.
""",encoding="utf-8")
    if field_service:
        (stage/"README.txt").write_text("""CatDB Studio — Field Service hands-on development build

Start Field Service.cmd opens the original fictional data and a guided course.
It copies samples/field-service-source.catdb to data/field-service.catdb only
when that working file does not exist. Existing work is never reseeded.
Start Completed Example.cmd separately opens a saved, actually computed result.
These launchers do not open or modify any other Studio installation or file.

日本語: Start Field Service.cmdで開始してください。
「体験の道順」で、原本の確認、Σ、Δ、Π、資格の追加、再計算、
矛盾する下書きの拒否と修正、元の版への由来を実際に試せます。
詳しい手順と初期データの結果: docs/field-service.ja.md

All data is fictional. Human-readable names are record IDs, not scalar columns.
The engine performs real migrations. Candidates are individually feasible
triples, not a conflict-free assignment plan or optimizer. Same-name matching
does not establish identity; explicit crosswalk keys are the given premise.
Keep studio-web beside catdb-studio.exe. No compiler is needed to run.
Ctrl+C or closing the console stops the local app. Applied revisions persist;
browser drafts are not saved. Source commit is recorded in BUILD_INFO.txt.

Original source: MIT. Dependencies retain their licenses; see notices and GNU
license texts. The matching corresponding-source archive provides native/GMP
sources and modification/recombination instructions. This development package
does not create a new public tag or release.
""",encoding="utf-8")
    with zipfile.ZipFile(binary_zip,"w",zipfile.ZIP_DEFLATED,compresslevel=6) as archive:
        for file in sorted(stage.rglob("*")):
            if file.is_file():
                archive.write(file, stage.name+"/"+file.relative_to(stage).as_posix())
    tracked = subprocess.check_output(["git","ls-files","-z"],cwd=repo).decode().split("\0")
    rebuild = f"""# Corresponding source for CatDB {VERSION}

Native binaries use GHC 9.6.7. Original CatDB code is MIT. Dependencies retain
their own licenses; GMP is conveyed under LGPL-3.0-or-later. Modification and
reverse engineering for debugging modifications are permitted.

The catdb/ tree is the complete tracked source at {commit}. dependencies/ holds
the exact native Hackage source closure. ghc-9.6.7-src.tar.xz contains the matching
GHC runtime/boot libraries, GMP 6.3.0 source and its GHC build patches. The native
dependency manifest and archive hashes are included.

With GHC 9.6.7 and Cabal installed, from this directory:
  cabal build --project-file=cabal.project.sources catdb:exe:catdb-studio catdb:exe:catdb catdb:exe:relay-desk
Use cabal list-bin with the same project file to locate each executable.
On Windows, use a writable TEMP/TMP. Build tools may need downloading.

For the frontend, keep the complete catdb/ tree, enter catdb/studio/frontend,
then run nix develop .#wasm --command bash ./build.sh. On Windows use Ubuntu
WSL and catdb/build-frontend.ps1. The pinned flake selects the exact GHC-WASM
shell; Miso 1.12.0 is pinned independently. Browser WASI shim source and all
frontend notice texts are in static/. Keep built studio/web as studio-web beside
the native executable. The frontend uses the Native bignum backend, not GMP.

To rebuild and recombine the native executables with modified static GMP:
1. Extract ghc-9.6.7-src.tar.xz. Modify and repack
   libraries/ghc-bignum/gmp/gmp-tarballs/gmp-6.3.0-nodoc.tar.xz with its original
   top-level gmp-6.3.0 directory. Retain/review the adjacent GHC patches.
2. Follow the included Hadrian Windows prerequisites (bootstrap GHC, Cabal,
   MSYS2/build tools). From the GHC root run ./boot and
   ./configure --enable-tarballs-autodownload, then
   hadrian/build.bat -j --bignum=gmp binary-dist
3. Select that rebuilt compiler and use a fresh Cabal store to avoid reusing
   old GHC/GMP objects:
   cabal --store-dir=rebuilt-store build --project-file=cabal.project.sources catdb:exe:catdb-studio catdb:exe:catdb catdb:exe:relay-desk
   Copy the unchanged web assets beside the rebuilt executables.

The application build was tested; a complete modified-compiler rebuild was not
performed. These are upstream build entry points, not a bit-identical-output
claim. See GHC's included hadrian/README.md for platform prerequisites.
"""
    hashes = {}
    with zipfile.ZipFile(source_zip,"w",zipfile.ZIP_DEFLATED,compresslevel=6) as archive:
        for name in tracked:
            if name:
                archive.write(repo / name,"catdb/"+name)
        for file in archives+[args.ghc_source]:
            name = ("dependencies/" if file!=args.ghc_source else "")+file.name
            archive.write(file,name,compress_type=zipfile.ZIP_STORED)
            hashes[name] = hashlib.sha256(file.read_bytes()).hexdigest()
        for name,body in gnu.items():
            archive.writestr(name,body)
        archive.writestr("REBUILD.md",rebuild)
        archive.writestr("DEPENDENCIES.json",json.dumps(manifest,indent=2))
        archive.writestr("SOURCE-SHA256.json",json.dumps(hashes,indent=2))
        archive.writestr("cabal.project.sources","packages: catdb\n          dependencies/*.tar.gz\n")
    sums = "".join(hashlib.sha256(p.read_bytes()).hexdigest()+"  "+p.name+"\n" for p in [binary_zip,source_zip])
    (output / f"catdb-{VERSION}-SHA256SUMS.txt").write_text(sums,encoding="utf-8")
    print(json.dumps(dict(commit=commit, nativeDependencies=len(manifest), packages=[str(binary_zip),str(source_zip)], bytes=[binary_zip.stat().st_size,source_zip.stat().st_size]),indent=2))


if __name__ == "__main__":
    main()
