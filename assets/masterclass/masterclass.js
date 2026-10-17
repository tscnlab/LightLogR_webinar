(() => {
  "use strict";
  const steps = [...document.querySelectorAll(".mc-step")];
  const jump = document.getElementById("mc-jump");
  const previous = document.getElementById("mc-prev");
  const next = document.getElementById("mc-next");
  const present = document.getElementById("mc-present");
  const progress = document.getElementById("mc-progress-fill");
  const announcement = document.getElementById("mc-announcement");
  if (!steps.length || !jump) return;

  let current = Math.max(0, steps.findIndex(step => `#${step.id}` === location.hash));
  let presenting = new URL(location.href).searchParams.get("present") === "1";
  let trackUserScroll = true;
  const toolbar = document.querySelector(".mc-toolbar");
  const courseHeader = document.getElementById("quarto-header");

  function updateChromeOffsets() {
    const headerBottom = courseHeader?.getBoundingClientRect().bottom || 0;
    const offset = presenting ? 0 : Math.max(0, headerBottom);
    document.documentElement.style.setProperty("--mc-nav-offset", `${offset}px`);
    document.documentElement.style.setProperty("--mc-toolbar-height", `${toolbar.offsetHeight}px`);
  }
  steps.forEach((step, index) => {
    const option = document.createElement("option");
    option.value = String(index);
    option.textContent = `${index + 1} / ${steps.length} · ${step.dataset.short}`;
    jump.append(option);
  });

  function controls() {
    jump.value = String(current);
    previous.disabled = current === 0;
    next.disabled = current === steps.length - 1;
    progress.style.width = `${100 * (current + 1) / steps.length}%`;
  }

  function saveLocation(replace = false) {
    const url = new URL(location.href);
    url.hash = steps[current].id;
    if (presenting) url.searchParams.set("present", "1");
    else url.searchParams.delete("present");
    history[replace ? "replaceState" : "pushState"]({}, "", url);
  }

  function renderMode() {
    document.body.classList.toggle("mc-presenting", presenting);
    present.textContent = presenting ? "Course view" : "Present";
    present.setAttribute("aria-pressed", String(presenting));
    present.title = presenting ? "Show the whole course. Escape also exits presentation view." : "Show one chapter at a time. Use left and right arrows outside the editor.";
    steps.forEach((step, index) => { step.hidden = presenting && index !== current; });
    updateChromeOffsets();
    controls();
    // CodeMirror measures itself again after a formerly hidden chapter is shown.
    window.dispatchEvent(new Event("resize"));
  }

  function go(index, save = true) {
    current = Math.max(0, Math.min(steps.length - 1, index));
    renderMode();
    if (save) saveLocation();
    scrollToCurrent();
    announcement.textContent = `Chapter ${current + 1} of ${steps.length}: ${steps[current].dataset.short}`;
  }

  function scrollToCurrent() {
    // Layout changes while switching views must not override a chosen chapter.
    trackUserScroll = false;
    steps[current].scrollIntoView({ block: "start", behavior: "instant" });
  }

  previous.addEventListener("click", () => go(current - 1));
  next.addEventListener("click", () => go(current + 1));
  jump.addEventListener("change", () => go(Number(jump.value)));
  present.addEventListener("click", () => {
    presenting = !presenting;
    renderMode();
    saveLocation();
    scrollToCurrent();
    announcement.textContent = presenting ? "Presentation view. Use the chapter controls or left and right arrow keys outside code editors." : "Course view. All chapters are visible.";
  });

  document.addEventListener("click", event => {
    const link = event.target.closest("a[href^='#']");
    if (!link || !link.hash) return;
    const index = steps.findIndex(step => `#${step.id}` === link.hash);
    if (index < 0) return;
    event.preventDefault();
    go(index);
  });

  document.addEventListener("keydown", event => {
    if (!presenting || event.ctrlKey || event.metaKey || event.altKey || event.shiftKey) return;
    if (event.target.closest("input, textarea, select, button, [role='tab'], [contenteditable='true'], .cm-editor, .mc-concept-scroll")) return;
    if (event.key === "ArrowRight") { event.preventDefault(); go(current + 1); }
    if (event.key === "ArrowLeft") { event.preventDefault(); go(current - 1); }
    if (event.key === "Escape") { event.preventDefault(); present.click(); }
  });

  function restoreLocation() {
    const index = steps.findIndex(step => `#${step.id}` === location.hash);
    presenting = new URL(location.href).searchParams.get("present") === "1";
    go(index < 0 ? 0 : index, false);
  }
  window.addEventListener("popstate", restoreLocation);
  window.addEventListener("hashchange", restoreLocation);

  // Resume automatic chapter tracking when the reader scrolls or moves focus.
  ["wheel", "touchstart", "pointerdown", "keydown"].forEach(type => {
    window.addEventListener(type, () => { trackUserScroll = true; },
      { capture: true, passive: true });
  });

  let scrollPending = false;
  window.addEventListener("scroll", () => {
    updateChromeOffsets();
    if (presenting || scrollPending || !trackUserScroll) return;
    scrollPending = true;
    requestAnimationFrame(() => {
      if (presenting || !trackUserScroll) {
        scrollPending = false;
        return;
      }
      const target = Math.min(innerHeight * .3, 230);
      let closest = 0;
      steps.forEach((step, index) => {
        if (step.getBoundingClientRect().top <= target) closest = index;
      });
      current = closest;
      controls();
      scrollPending = false;
    });
  }, { passive: true });
  window.addEventListener("resize", updateChromeOffsets);
  courseHeader?.addEventListener("transitionend", updateChromeOffsets);
  const chromeObserver = new ResizeObserver(updateChromeOffsets);
  chromeObserver.observe(toolbar);
  if (courseHeader) chromeObserver.observe(courseHeader);
  renderMode();
})();
