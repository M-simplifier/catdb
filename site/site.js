'use strict';

// Recorded Relay Desk v0.2.0-poc results, not a browser database engine.
// See site/README.md for the scenario and the implementation sources.
const demo = {
  initial: {people: 5, pairs: 7, names: ['Jon Bell', 'Jon B.']},
  linked: {people: 4, pairs: 6, names: ['Jon Bell']}
};
const japanese = document.documentElement.lang === 'ja';
const copy = japanese ? {
  link: 'この2件を、同じ人に。　↓',
  unlink: '同一人物リンクを外して、戻す。　↶',
  before: '同じ人と確認できたら、リンクを追加。\n下の担当候補も一緒に変わります。',
  after: '原本は2件とも残ったまま。\n担当候補のJonも1人になります。',
  choices: n => `候補 ${n}人`,
  rejected: '登録を拒否。保存された担当はMayaのまま。',
  accepted: '登録できる。二つの道が一致する。',
  tryWrong: '別チームのJonを登録すると？',
  restore: '同じチームのMayaに戻す',
  copied: '起動コマンドをコピーしました。',
  selected: 'コマンドを選択しました。Ctrl+C（Macは⌘C）でコピーできます。'
} : {
  link: 'Link these records as one person ↓',
  unlink: 'Remove the link. Bring both identities back ↶',
  before: 'Confirm the identity, then add a link.\nWatch the assignee choices change with it.',
  after: 'Both source records are still here.\nNow Jon appears once in the assignee choices.',
  choices: n => `${n} ${n === 1 ? 'choice' : 'choices'}`,
  rejected: 'Rejected. The saved assignee is still Maya.',
  accepted: 'Accepted. Both paths reach Design.',
  tryWrong: 'Try Jon from another team',
  restore: 'Go back to Maya in Design',
  copied: 'Start command copied.',
  selected: 'Command selected. Press Ctrl+C (⌘C on Mac) to copy.'
};
const byId = id => document.getElementById(id);
let linked = false;
byId('merge-button').addEventListener('click', () => {
  linked = !linked;
  const state = linked ? demo.linked : demo.initial;
  byId('merge-demo').classList.toggle('linked', linked);
  byId('merge-button').setAttribute('aria-pressed', String(linked));
  byId('merge-button').textContent = linked ? copy.unlink : copy.link;
  byId('merge-explain').textContent = linked ? copy.after : copy.before;
  byId('hero-people').textContent = state.people;
  byId('hero-pairs').textContent = state.pairs;
  byId('candidate-count').textContent = copy.choices(state.names.length);
  const people = state.names.map(name => {
    const pill = document.createElement('span');
    pill.className = 'person-pill';
    const avatar = document.createElement('i');
    avatar.className = 'avatar';
    avatar.textContent = 'JB';
    avatar.setAttribute('aria-hidden', 'true');
    pill.append(avatar, document.createTextNode(name));
    return pill;
  });
  byId('hero-candidates').replaceChildren(...people);
});

let wrongTeam = false;
byId('rule-button').addEventListener('click', () => {
  wrongTeam = !wrongTeam;
  byId('rule-box').classList.toggle('bad', wrongTeam);
  byId('rule-button').setAttribute('aria-pressed', String(wrongTeam));
  byId('rule-person').textContent = wrongTeam ? 'Jon' : 'Maya';
  byId('rule-team').textContent = wrongTeam ? 'Engineering' : 'Design';
  byId('rule-equals').textContent = wrongTeam ? '≠' : '=';
  byId('rule-status').textContent = wrongTeam ? copy.rejected : copy.accepted;
  byId('rule-button').firstElementChild.textContent = wrongTeam ? copy.restore : copy.tryWrong;
  byId('rule-button').lastElementChild.textContent = wrongTeam ? '↶' : '↗';
});

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
