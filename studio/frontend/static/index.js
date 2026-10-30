import { WASI, OpenFile, File, ConsoleStdout } from "./vendor/wasi/index.js";
import ghcWasmJSFFI from "./ghc_wasm_jsffi.js";

const supplied = location.hash.slice(1);
if (/^[a-f0-9]{64}$/.test(supplied)) sessionStorage.setItem("catdb-session", supplied);
if (supplied) history.replaceState(null, "", location.pathname);
let dirty = false;
window.catdbBrowser = {
  token: () => sessionStorage.getItem("catdb-session") || "",
  setDirty: (value) => { dirty = value; },
  async download(kind) {
    const response = await fetch(`/api/${kind}`, { headers: { "X-CatDB-Session": window.catdbBrowser.token() } });
    if (!response.ok) throw new Error(`ダウンロードに失敗しました (${response.status})`);
    const url = URL.createObjectURL(await response.blob());
    const link = document.createElement("a");
    link.href = url;
    link.download = kind === "backup" ? "catdb-backup.catdb" : "catdb-definitions.json";
    link.click();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
  },
};
addEventListener("beforeunload", (event) => {
  if (dirty) { event.preventDefault(); event.returnValue = ""; }
});
try {
  const wasi = new WASI([], ["GHCRTS=-H32m"], [
    new OpenFile(new File([])),
    ConsoleStdout.lineBuffered((message) => console.info(`[catdb] ${message}`)),
    ConsoleStdout.lineBuffered((message) => console.warn(`[catdb] ${message}`)),
  ]);
  const exports = {};
  const response = await fetch("/studio-gui.wasm");
  if (!response.ok) throw new Error(`WASM ${response.status}`);
  const { instance } = await WebAssembly.instantiate(await response.arrayBuffer(), {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghcWasmJSFFI(exports),
  });
  Object.assign(exports, instance.exports);
  wasi.initialize(instance);
  document.getElementById("boot-display")?.remove();
  await instance.exports.hs_start();
} catch (error) {
  console.error(error);
  document.body.insertAdjacentHTML("beforeend", '<div class="boot-display"><b>画面を起動できませんでした</b><p>配布フォルダーからCatDB Studioを起動し直し、表示されたURLを開いてください。</p></div>');
}
