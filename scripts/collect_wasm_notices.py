"""Collect redistributable notices for the pinned frontend build, from its plan."""
import argparse
import concurrent.futures
import hashlib
import io
import json
import pathlib
import re
import tarfile
import urllib.error
import urllib.request


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--plan", type=pathlib.Path, required=True)
    parser.add_argument("--cache", type=pathlib.Path, required=True)
    parser.add_argument("--output", type=pathlib.Path, required=True)
    args = parser.parse_args()
    args.cache.mkdir(parents=True, exist_ok=True)
    args.output.mkdir(parents=True, exist_ok=True)
    plan = json.loads(args.plan.read_text())
    ghc_commit = "05e0ef08e100cf3bd2150adcf8e53ba9ad30519a"
    ghc = f"https://raw.githubusercontent.com/ghc/ghc/{ghc_commit}/"
    boot = {"base", "ghc-boot-th", "ghc-experimental", "ghc-internal", "ghc-prim", "rts", "template-haskell", "system-cxx-std-lib"}

    def fetch(url):
        cached = args.cache / hashlib.sha256(url.encode()).hexdigest()
        if not cached.exists():
            request = urllib.request.Request(url, headers={"User-Agent": "CatDB-release-notices"})
            cached.write_bytes(urllib.request.urlopen(request, timeout=45).read())
        return cached.read_bytes()

    ghc_license = fetch(ghc + "LICENSE").decode()

    def package(p):
        name, version = p["pkg-name"], p["pkg-version"]
        if name == "studio-gui":
            return None
        if name == "miso":
            url = "https://raw.githubusercontent.com/dmjio/miso/9f1222293fe92e7d22272e884ac97c205876b943/LICENSE"
            return dict(name=name, version=version, license="BSD-3-Clause", notices=[(url, fetch(url).decode())])
        if name in boot:
            url = ghc + ("rts/LICENSE" if name == "rts" else f"libraries/{name}/LICENSE")
            try:
                body = fetch(url).decode()
            except urllib.error.HTTPError as error:
                if error.code != 404:
                    raise
                url, body = ghc + "LICENSE", ghc_license
            return dict(name=name, version=version, license="BSD-3-Clause", notices=[(url, body)])
        url = f"https://hackage-content.haskell.org/package/{name}-{version}.tar.gz"
        notices = []
        with tarfile.open(fileobj=io.BytesIO(fetch(url))) as archive:
            cabal = archive.extractfile(f"{name}-{version}/{name}.cabal").read().decode()
            match = re.search(r"^license:\s*(.+)", cabal, re.M | re.I)
            license_name = match.group(1).strip() if match else "see license text"
            for member in archive.getmembers():
                if member.isfile() and pathlib.PurePosixPath(member.name).name.upper().startswith(("LICENSE", "COPYING")):
                    notices.append((url + " / " + member.name, archive.extractfile(member).read().decode(errors="replace")))
        if not notices:
            raise RuntimeError(f"No license text for {name}-{version}")
        return dict(name=name, version=version, license=license_name, notices=notices)

    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        packages = [p for p in pool.map(package, plan["install-plan"]) if p]
    extra_urls = [
        "https://raw.githubusercontent.com/llvm/llvm-project/llvmorg-21.1.8/llvm/LICENSE.TXT",
        "https://raw.githubusercontent.com/llvm/llvm-project/llvmorg-21.1.8/libcxx/LICENSE.TXT",
        "https://raw.githubusercontent.com/llvm/llvm-project/llvmorg-21.1.8/libcxxabi/LICENSE.TXT",
    ]
    # Record the exact revision from which these upstream notice texts are read.
    libc = json.loads(fetch("https://api.github.com/repos/WebAssembly/wasi-libc/commits/main"))["sha"]
    extra_urls += [f"https://raw.githubusercontent.com/WebAssembly/wasi-libc/{libc}/{path}" for path in
                   ["LICENSE", "LICENSE-APACHE", "LICENSE-APACHE-LLVM", "LICENSE-MIT", "libc-top-half/musl/COPYRIGHT"]]
    extras = [(url, fetch(url).decode()) for url in extra_urls]
    texts = ["CatDB Studio WebAssembly runtime notices\n",
             f"GHC-WASM {plan['compiler-id']}, source commit {ghc_commit}.\n",
             "The exposed selected bignum backend is Native, not GMP.\n",
             "WASI SDK distribution: haskell-wasm/ghc-wasm-bindists 20251219T213239.\n",
             "Clang 21.1.8-wasi-sdk, custom LLVM commit abacb164cdfb155c778c9fd197682353415bce5b.\n",
             f"WASI libc upstream notice texts were collected at {libc}; this identifies the notice source, not an independently established libc binary revision.\n"]
    for package_info in packages:
        texts.append(f"\n{'='*72}\n{package_info['name']} {package_info['version']} — {package_info['license']}\n")
        for url, body in package_info["notices"]:
            texts.append(f"\n{url}\n{body}\n")
    for url, body in extras:
        texts.append(f"\n{'='*72}\n{url}\n{body}\n")
    (args.output / "WASM_NOTICES.txt").write_text("".join(texts), encoding="utf-8")
    manifest = dict(compiler=plan["compiler-id"], ghcCommit=ghc_commit,
                    packages=[{k:v for k,v in p.items() if k != "notices"} for p in packages], noticeSources=extra_urls)
    (args.output / "WASM_DEPENDENCIES.json").write_text(json.dumps(manifest, indent=2), encoding="utf-8")
    print(f"Collected {len(packages)} Haskell/WASM package notices and WASI/LLVM notices.")


if __name__ == "__main__":
    main()
