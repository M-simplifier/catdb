(() => {
  const en = document.documentElement.lang === "en";
  const words = en ? {
    open: "Open navigation", close: "Close navigation", valid: "● Consistent", invalid: "● Draft is inconsistent",
    messages: ["Both paths reach the same book, b1.", "Directly: b2. Through the quotation: b1. The draft violates the path equation and cannot be committed.", "Both paths now reach b2. The two changes can be checked together."],
    copy: "Copy ↗", copied: "Copied ✓", select: "Select the text to copy",
  } : {
    open: "ナビゲーションを開く", close: "ナビゲーションを閉じる", valid: "● 整合しています", invalid: "● 下書きに不一致があります",
    messages: ["どちらの道も同じ本 b1 に着きます。", "直接は b2、引用を経由すると b1。経路等式に反するため、このままでは保存できません。", "両方が b2 に一致。二つの変更をまとめて検査できます。"],
    copy: "コピー ↗", copied: "コピーしました ✓", select: "選択してコピーしてください",
  };
  const menu = document.querySelector("#menu-button");
  const mobile = document.querySelector("#mobile-nav");
  function toggleMenu(open) {
    if (!menu || !mobile) return;
    menu.setAttribute("aria-expanded", String(open));
    menu.setAttribute("aria-label", open ? words.close : words.open);
    mobile.hidden = !open;
  }
  menu?.addEventListener("click", () => toggleMenu(menu.getAttribute("aria-expanded") !== "true"));
  mobile?.querySelectorAll("a").forEach(link => link.addEventListener("click", () => toggleMenu(false)));
  document.addEventListener("keydown", event => { if (event.key === "Escape") toggleMenu(false); });
  const tabs = [...document.querySelectorAll(".operation-tab")];
  function selectTab(tab, focus = false) {
    for (const candidate of tabs) {
      const active = candidate === tab;
      candidate.classList.toggle("active", active);
      candidate.setAttribute("aria-selected", String(active));
      candidate.tabIndex = active ? 0 : -1;
    }
    document.querySelectorAll(".operation-panel").forEach(panel => {
      const active = panel.id === `panel-${tab.dataset.panel}`;
      panel.hidden = !active;
      panel.classList.toggle("active", active);
    });
    if (focus) tab.focus();
  }
  tabs.forEach((tab, index) => {
    tab.addEventListener("click", () => selectTab(tab));
    tab.addEventListener("keydown", event => {
      let next;
      if (["ArrowRight", "ArrowDown"].includes(event.key)) next = (index + 1) % tabs.length;
      else if (["ArrowLeft", "ArrowUp"].includes(event.key)) next = (index + tabs.length - 1) % tabs.length;
      else if (event.key === "Home") next = 0;
      else if (event.key === "End") next = tabs.length - 1;
      else return;
      event.preventDefault();
      selectTab(tabs[next], true);
    });
  });
  const change = document.querySelector("#change-path");
  const fix = document.querySelector("#fix-path");
  const reset = document.querySelector("#reset-path");
  const direct = document.querySelector("#direct-target");
  const quoted = document.querySelector("#quote-target");
  const quotation = document.querySelector("#via-quote");
  const status = document.querySelector("#path-status");
  const message = document.querySelector("#path-message");
  const equals = document.querySelector("#path-equals");
  let step = 0;
  function renderPath() {
    if (![change, fix, direct, quoted, quotation, status, message, equals].every(Boolean)) return;
    const invalid = step === 1;
    direct.textContent = step === 0 ? "b1" : "b2";
    quoted.textContent = step === 2 ? "b2" : "b1";
    quotation.textContent = step === 2 ? "quote → q2 → quoteBook →" : "quote → q1 → quoteBook →";
    status.textContent = invalid ? words.invalid : words.valid;
    status.classList.toggle("invalid", invalid);
    status.classList.toggle("valid", !invalid);
    message.classList.toggle("invalid", invalid);
    equals.classList.toggle("invalid", invalid);
    equals.textContent = invalid ? "≠" : "=";
    message.textContent = words.messages[step];
    change.disabled = step !== 0;
    fix.disabled = step !== 1;
  }
  change?.addEventListener("click", () => { step = 1; renderPath(); });
  fix?.addEventListener("click", () => { step = 2; renderPath(); });
  reset?.addEventListener("click", () => { step = 0; renderPath(); });
  renderPath();
  const copy = document.querySelector("#copy-code");
  copy?.addEventListener("click", async () => {
    try {
      await navigator.clipboard.writeText(document.querySelector("#code-snippet").textContent);
      copy.textContent = words.copied;
    } catch { copy.textContent = words.select; }
    setTimeout(() => { copy.textContent = words.copy; }, 2400);
  });
})();
