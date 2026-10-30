'use strict';

// Educational path illustration only. The native Studio owns validation/storage.
const isJapanese = document.documentElement.lang === 'ja';
const pathButton = document.getElementById('path-button');
if (pathButton) {
  let split = false;
  pathButton.addEventListener('click', () => {
    split = !split;
    document.getElementById('path-demo').classList.toggle('bad', split);
    pathButton.setAttribute('aria-pressed', String(split));
    document.getElementById('direct-book').replaceChildren(document.createTextNode(split ? 'Book b2' : 'Book b1'));
    document.getElementById('path-equals').textContent = split ? '≠' : '＝';
    document.getElementById('path-message').textContent = isJapanese
      ? (split ? '直接はb2、引用経由はb1。この下書きは保存できません。引用も意図的に直すか、変更を破棄します。' : 'このパス等式は、すべてのNoteの行に適用されます。')
      : (split ? 'Direct: b2. Through the quote: b1. This draft cannot be committed. Deliberately repair the quote too, or discard the change.' : 'The declared equation applies to every Note record.');
    pathButton.textContent = isJapanese
      ? (split ? '下書きを破棄して戻す ↶' : '直接の本だけを b2 に変える ↓')
      : (split ? 'Discard this draft ↶' : 'Change only the direct book to b2 ↓');
  });
}

const byId = id => document.getElementById(id);
const copy = isJapanese ? {copied:'起動コマンドをコピーしました。',selected:'コマンドを選択しました。Ctrl+Cでコピーできます。'} : {copied:'Start command copied.',selected:'Command selected. Press Ctrl+C to copy.'};
let toastTimeout;
function toast(message) {
  byId('toast').textContent = message;
  byId('toast').classList.add('show');
  clearTimeout(toastTimeout);
  toastTimeout = setTimeout(() => byId('toast').classList.remove('show'), 3000);
}
byId('copy-command').addEventListener('click', async () => {
  try {
    await navigator.clipboard.writeText(byId('command').textContent);
    toast(copy.copied);
  } catch {
    const range = document.createRange();
    range.selectNodeContents(byId('command'));
    const selection = window.getSelection();
    selection.removeAllRanges();
    selection.addRange(range);
    toast(copy.selected);
  }
});
