function getPath(obj, path){
  return path.split('.').reduce((o, k) => (o && o[k] !== undefined ? o[k] : undefined), obj);
}

document.addEventListener('DOMContentLoaded', () => {

  /* ---------- preloader ---------- */
  window.addEventListener('load', () => document.body.classList.add('loaded'));
  setTimeout(() => document.body.classList.add('loaded'), 1200);

  /* ---------- scroll top button (declared early — referenced by onScroll below) ---------- */
  const scrollTopBtn = document.querySelector('.scroll-top');
  function toggleScrollTop(){
    if (!scrollTopBtn) return;
    scrollTopBtn.classList.toggle('show', window.scrollY > 500);
  }
  scrollTopBtn?.addEventListener('click', (e) => {
    e.preventDefault();
    window.scrollTo({ top: 0, behavior: 'smooth' });
  });

  /* ---------- nav scroll state ---------- */
  const nav = document.querySelector('.nav');
  const onScroll = () => {
    nav.classList.toggle('scrolled', window.scrollY > 12);
    toggleScrollTop();
  };
  document.addEventListener('scroll', onScroll, { passive: true });
  onScroll();

  /* ---------- mobile nav toggle ---------- */
  const navToggle = document.querySelector('.nav-toggle');
  const navLinks = document.querySelector('.nav-links');
  navToggle?.addEventListener('click', () => navLinks.classList.toggle('open'));
  navLinks?.querySelectorAll('a').forEach(a => a.addEventListener('click', () => navLinks.classList.remove('open')));

  /* ---------- active link on scroll ---------- */
  const sections = document.querySelectorAll('main section[id]');
  const links = document.querySelectorAll('.nav-links a');
  const spy = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
      const link = document.querySelector(`.nav-links a[href="#${entry.target.id}"]`);
      if (!link) return;
      if (entry.isIntersecting) {
        links.forEach(l => l.classList.remove('active'));
        link.classList.add('active');
      }
    });
  }, { rootMargin: '-40% 0px -50% 0px', threshold: 0 });
  sections.forEach(s => spy.observe(s));

  /* ---------- scroll reveal ---------- */
  const revealables = document.querySelectorAll('.reveal');
  const revealObserver = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        entry.target.classList.add('in');
        revealObserver.unobserve(entry.target);
      }
    });
  }, { threshold: 0.12 });
  revealables.forEach(el => revealObserver.observe(el));

  /* ---------- typed hero role ---------- */
  const typedEl = document.getElementById('typed-role');
  const roles = (typeof SITE_CONTENT !== 'undefined' && SITE_CONTENT.hero?.roles) || ['Data Analyst', 'Data Scientist'];
  if (typedEl) {
    let ri = 0, ci = 0, deleting = false;
    const tick = () => {
      const word = roles[ri];
      if (!deleting) {
        ci++;
        typedEl.textContent = word.slice(0, ci);
        if (ci === word.length) { deleting = true; setTimeout(tick, 1400); return; }
      } else {
        ci--;
        typedEl.textContent = word.slice(0, ci);
        if (ci === 0) { deleting = false; ri = (ri + 1) % roles.length; }
      }
      setTimeout(tick, deleting ? 35 : 65);
    };
    tick();
  }

  /* ---------- current year ---------- */
  const yearEl = document.getElementById('year');
  if (yearEl) yearEl.textContent = new Date().getFullYear();

  /* ---------- site content (hero / about / section intros / resume / contact) ---------- */
  if (typeof SITE_CONTENT !== 'undefined') {
    const SC = SITE_CONTENT;

    document.querySelectorAll('[data-field]').forEach(el => {
      const val = getPath(SC, el.dataset.field);
      if (val !== undefined && val !== null) el.textContent = val;
    });
    document.querySelectorAll('[data-field-src]').forEach(el => {
      const val = getPath(SC, el.dataset.fieldSrc);
      if (val) el.setAttribute('src', val);
    });

    const socialMap = {
      email: SC.contact?.email ? `mailto:${SC.contact.email}` : null,
      linkedin: SC.contact?.linkedin?.url,
      github: SC.contact?.github?.url,
      kaggle: SC.contact?.kaggle?.url,
      instagram: SC.contact?.instagram?.url
    };
    document.querySelectorAll('[data-social]').forEach(a => {
      const url = socialMap[a.dataset.social];
      if (url) a.setAttribute('href', url);
    });

    Object.keys(SC.sectionIntros || {}).forEach(key => {
      const head = document.getElementById(`${key}-head`);
      const info = SC.sectionIntros[key];
      if (head && info) {
        head.innerHTML = `<div class="cell-tag"><b>In [${info.cellNum}]:</b> ${info.cellText}</div><h2>${info.title}</h2><p>${info.desc}</p>`;
      }
    });

    const heroStats = document.getElementById('hero-stats');
    if (heroStats && SC.hero?.stats) {
      heroStats.innerHTML = SC.hero.stats.map(s =>
        `<div><b data-count="${s.value}" data-suffix="${s.suffix || ''}">0</b><span>${s.label}</span></div>`
      ).join('');
    }

    const aboutText = document.getElementById('about-text');
    if (aboutText && SC.about?.paragraphs) {
      aboutText.innerHTML = SC.about.paragraphs.map(p => `<p>${p}</p>`).join('');
    }
    const aboutFacts = document.getElementById('about-facts');
    if (aboutFacts && SC.about?.facts) {
      aboutFacts.innerHTML = SC.about.facts.map(f => `<li><b>${f.label}</b><span class="k">${f.value}</span></li>`).join('');
    }

    const timelineHTML = (items) => (items || []).map(it => `
      <div class="timeline-item">
        <div class="when">${it.when}</div>
        <h4>${it.title}</h4>
        <div class="org">${it.org}</div>
        <ul>${(it.bullets || []).map(b => `<li>${b}</li>`).join('')}</ul>
      </div>`).join('');
    const eduT = document.getElementById('edu-timeline');
    if (eduT) eduT.innerHTML = timelineHTML(SC.resume?.education);
    const expT = document.getElementById('exp-timeline');
    if (expT) expT.innerHTML = timelineHTML(SC.resume?.experience);
    const certCard = document.getElementById('cert-card');
    if (certCard && SC.resume?.certCard) {
      certCard.innerHTML = `<i class="bi bi-patch-check-fill"></i><div><h5>${SC.resume.certCard.title}</h5><span>${SC.resume.certCard.subtitle}</span></div>`;
    }

    const contactList = document.getElementById('contact-list');
    if (contactList && SC.contact) {
      const c = SC.contact;
      let html = '';
      if (c.email) html += `<a class="contact-item" href="mailto:${c.email}" data-copy="${c.email}"><span class="ic"><i class="bi bi-envelope"></i></span><div><b>Email</b><span>${c.email}</span></div><span class="copy-hint mono">click to copy</span></a>`;
      if (c.linkedin?.url) html += `<a class="contact-item" href="${c.linkedin.url}" target="_blank" rel="noopener noreferrer"><span class="ic"><i class="bi bi-linkedin"></i></span><div><b>LinkedIn</b><span>${c.linkedin.label || c.linkedin.url}</span></div></a>`;
      if (c.github?.url) html += `<a class="contact-item" href="${c.github.url}" target="_blank" rel="noopener noreferrer"><span class="ic"><i class="bi bi-github"></i></span><div><b>GitHub</b><span>${c.github.label || c.github.url}</span></div></a>`;
      if (c.kaggle?.url) html += `<a class="contact-item" href="${c.kaggle.url}" target="_blank" rel="noopener noreferrer"><span class="ic"><i class="fa-brands fa-kaggle"></i></span><div><b>Kaggle</b><span>${c.kaggle.label || c.kaggle.url}</span></div></a>`;
      if (c.instagram?.url) html += `<a class="contact-item" href="${c.instagram.url}" target="_blank" rel="noopener noreferrer"><span class="ic"><i class="bi bi-instagram"></i></span><div><b>Instagram</b><span>${c.instagram.label || c.instagram.url}</span></div></a>`;
      contactList.innerHTML = html;
    }

    const csForm = document.getElementById('contact-form');
    if (csForm && csForm.hasAttribute('data-form-endpoint') && SC.contact?.formEndpoint) {
      csForm.setAttribute('action', `https://formsubmit.co/${SC.contact.formEndpoint}`);
    }
  }

  /* ---------- animated stat counters ---------- */
  const counters = document.querySelectorAll('[data-count]');
  const counterObserver = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
      if (!entry.isIntersecting) return;
      const el = entry.target;
      const target = parseFloat(el.dataset.count);
      const suffix = el.dataset.suffix || '';
      const decimals = el.dataset.count.includes('.') ? el.dataset.count.split('.')[1].length : 0;
      const duration = 1100;
      const start = performance.now();
      const step = (now) => {
        const p = Math.min(1, (now - start) / duration);
        const eased = 1 - Math.pow(1 - p, 3);
        el.textContent = (target * eased).toFixed(decimals) + suffix;
        if (p < 1) requestAnimationFrame(step);
      };
      requestAnimationFrame(step);
      counterObserver.unobserve(el);
    });
  }, { threshold: 0.6 });
  counters.forEach(el => counterObserver.observe(el));

  /* ---------- skill progress bars (shared: homepage + case-study pages) ---------- */
  const barFillObserver = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
      if (!entry.isIntersecting) return;
      const fill = entry.target;
      requestAnimationFrame(() => { fill.style.width = (fill.dataset.percent || 0) + '%'; });
      barFillObserver.unobserve(fill);
    });
  }, { threshold: 0.4 });

  function skillBarRowHTML(skill, i){
    return `
      <div class="skill-bar-row reveal" style="--i:${i}">
        <div class="sb-head"><span>${skill.name}</span><b>${skill.percent}%</b></div>
        <div class="skill-bar-track"><div class="skill-bar-fill" data-percent="${skill.percent}"></div></div>
      </div>`;
  }

  function watchBars(root){
    root.querySelectorAll('.skill-bar-fill').forEach(f => barFillObserver.observe(f));
    root.querySelectorAll('.reveal').forEach(el => revealObserver.observe(el));
  }

  const skillsGrid = document.getElementById('skills-grid');
  if (skillsGrid && typeof SKILL_CATEGORIES !== 'undefined') {
    skillsGrid.innerHTML = SKILL_CATEGORIES.map((cat, ci) => {
      const body = cat.badge
        ? `<div class="skill-tags">${cat.skills.map(s => `<span>${s.name}</span>`).join('')}</div>`
        : `<div class="skill-bars">${cat.skills.map((s, i) => skillBarRowHTML(s, i)).join('')}</div>`;
      return `
        <div class="skill-card reveal" style="--i:${ci}">
          <h4><i class="bi ${cat.icon}"></i> ${cat.title}</h4>
          <p>${cat.desc}</p>
          ${body}
        </div>`;
    }).join('');
    watchBars(skillsGrid);
  }

  /* ---------- cursor-reactive glow on cards / hero panel ---------- */
  document.querySelectorAll('.hero-panel, .project-card').forEach(card => {
    card.addEventListener('mousemove', (e) => {
      const r = card.getBoundingClientRect();
      card.style.setProperty('--mx', `${e.clientX - r.left}px`);
      card.style.setProperty('--my', `${e.clientY - r.top}px`);
    });
  });
  function bindCardGlow(root){
    root.querySelectorAll('.project-card').forEach(card => {
      card.addEventListener('mousemove', (e) => {
        const r = card.getBoundingClientRect();
        card.style.setProperty('--mx', `${e.clientX - r.left}px`);
        card.style.setProperty('--my', `${e.clientY - r.top}px`);
      });
    });
  }

  /* ---------- copy-to-clipboard email ---------- */
  document.querySelectorAll('[data-copy]').forEach(el => {
    el.addEventListener('click', (e) => {
      const text = el.dataset.copy;
      if (!text) return;
      e.preventDefault();
      navigator.clipboard?.writeText(text).then(() => {
        const hint = el.querySelector('.copy-hint');
        el.classList.add('copied');
        if (hint) { const prev = hint.textContent; hint.textContent = 'copied!'; setTimeout(() => { hint.textContent = prev; el.classList.remove('copied'); }, 1400); }
      });
    });
  });

  /* ---------- lightbox (shared: gallery triggers + slideshow zoom) ---------- */
  let lightboxOverlay = null;
  function getLightboxOverlay(){
    if (lightboxOverlay) return lightboxOverlay;
    lightboxOverlay = document.createElement('div');
    lightboxOverlay.className = 'lightbox-overlay';
    lightboxOverlay.innerHTML = `<button class="lightbox-close" aria-label="Close"><i class="bi bi-x-lg"></i></button><img src="" alt="">`;
    document.body.appendChild(lightboxOverlay);
    lightboxOverlay.addEventListener('click', (e) => {
      if (e.target === lightboxOverlay || e.target.closest('.lightbox-close')) closeLightbox();
    });
    return lightboxOverlay;
  }
  function openLightbox(src, alt){
    const overlay = getLightboxOverlay();
    const img = overlay.querySelector('img');
    img.src = src; img.alt = alt || '';
    overlay.classList.add('open');
    document.body.style.overflow = 'hidden';
  }
  function closeLightbox(){
    if (!lightboxOverlay) return;
    lightboxOverlay.classList.remove('open');
    document.body.style.overflow = '';
  }
  document.addEventListener('keydown', (e) => { if (e.key === 'Escape') closeLightbox(); });
  document.querySelectorAll('[data-lightbox]').forEach(a => {
    a.addEventListener('click', (e) => {
      e.preventDefault();
      openLightbox(a.getAttribute('href') || a.dataset.lightbox, a.querySelector('img')?.alt);
    });
  });

  /* ---------- doc viewer (presentations — PDF preview inline, no download) ---------- */
  let docViewerOverlay = null;
  function getDocViewerOverlay(){
    if (docViewerOverlay) return docViewerOverlay;
    docViewerOverlay = document.createElement('div');
    docViewerOverlay.className = 'viewer-overlay';
    docViewerOverlay.innerHTML = `
      <div class="viewer-panel">
        <div class="viewer-bar">
          <span class="dot" style="background:#ff5f56"></span>
          <span class="dot" style="background:#ffbd2e"></span>
          <span class="dot" style="background:#27c93f"></span>
          <span class="viewer-title mono"></span>
          <a class="viewer-open" href="#" target="_blank" rel="noopener noreferrer" title="Open in new tab"><i class="bi bi-box-arrow-up-right"></i></a>
          <button class="viewer-close" aria-label="Close"><i class="bi bi-x-lg"></i></button>
        </div>
        <div class="viewer-body"><iframe title="Document preview" loading="lazy"></iframe></div>
      </div>`;
    document.body.appendChild(docViewerOverlay);
    docViewerOverlay.addEventListener('click', (e) => {
      if (e.target === docViewerOverlay || e.target.closest('.viewer-close')) closeDocViewer();
    });
    return docViewerOverlay;
  }
  function openDocViewer(relPath, title){
    const overlay = getDocViewerOverlay();
    const absoluteUrl = new URL(relPath, window.location.href).href;
    overlay.querySelector('.viewer-title').textContent = title || 'Presentation';
    overlay.querySelector('.viewer-open').setAttribute('href', absoluteUrl);
    // PDFs render natively in an iframe — no third-party embed service needed.
    overlay.querySelector('iframe').setAttribute('src', absoluteUrl + '#toolbar=1');
    overlay.classList.add('open');
    document.body.style.overflow = 'hidden';
  }
  function closeDocViewer(){
    if (!docViewerOverlay) return;
    docViewerOverlay.classList.remove('open');
    docViewerOverlay.querySelector('iframe').setAttribute('src', '');
    document.body.style.overflow = '';
  }
  document.addEventListener('keydown', (e) => { if (e.key === 'Escape') closeDocViewer(); });
  document.addEventListener('click', (e) => {
    const trigger = e.target.closest('[data-doc-viewer]');
    if (!trigger) return;
    e.preventDefault();
    openDocViewer(trigger.dataset.docViewer, trigger.dataset.docTitle || trigger.textContent.trim());
  });

  /* ---------- code viewer (R scripts — fetched + syntax highlighted inline, no download) ---------- */
  let codeViewerOverlay = null;
  let hljsReady = null;
  function loadHighlightJS(){
    if (hljsReady) return hljsReady;
    hljsReady = new Promise((resolve) => {
      const css = document.createElement('link');
      css.rel = 'stylesheet';
      css.href = 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github-dark.min.css';
      document.head.appendChild(css);
      const core = document.createElement('script');
      core.src = 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js';
      core.onload = () => {
        const rLang = document.createElement('script');
        rLang.src = 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/r.min.js';
        rLang.onload = resolve;
        rLang.onerror = resolve;
        document.head.appendChild(rLang);
      };
      core.onerror = resolve;
      document.head.appendChild(core);
    });
    return hljsReady;
  }
  function getCodeViewerOverlay(){
    if (codeViewerOverlay) return codeViewerOverlay;
    codeViewerOverlay = document.createElement('div');
    codeViewerOverlay.className = 'viewer-overlay code-viewer-overlay';
    codeViewerOverlay.innerHTML = `
      <div class="viewer-panel">
        <div class="viewer-bar">
          <span class="dot" style="background:#ff5f56"></span>
          <span class="dot" style="background:#ffbd2e"></span>
          <span class="dot" style="background:#27c93f"></span>
          <span class="viewer-title mono"></span>
          <a class="viewer-open" href="#" target="_blank" rel="noopener noreferrer" title="Open raw file"><i class="bi bi-box-arrow-up-right"></i></a>
          <button class="viewer-close" aria-label="Close"><i class="bi bi-x-lg"></i></button>
        </div>
        <div class="viewer-body code-viewer-body"><pre><code class="language-r"></code></pre></div>
      </div>`;
    document.body.appendChild(codeViewerOverlay);
    codeViewerOverlay.addEventListener('click', (e) => {
      if (e.target === codeViewerOverlay || e.target.closest('.viewer-close')) closeCodeViewer();
    });
    return codeViewerOverlay;
  }
  async function openCodeViewer(relPath, title){
    const overlay = getCodeViewerOverlay();
    const codeEl = overlay.querySelector('code');
    const absoluteUrl = new URL(relPath, window.location.href).href;
    overlay.querySelector('.viewer-title').textContent = title || 'Script';
    overlay.querySelector('.viewer-open').setAttribute('href', absoluteUrl);
    codeEl.textContent = 'Loading…';
    overlay.classList.add('open');
    document.body.style.overflow = 'hidden';
    try {
      const [res] = await Promise.all([fetch(relPath), loadHighlightJS()]);
      if (!res.ok) throw new Error('fetch failed');
      codeEl.textContent = await res.text();
      if (window.hljs) window.hljs.highlightElement(codeEl);
    } catch (err) {
      codeEl.textContent = "Couldn't load the file inline — use the open-in-new-tab icon above instead.";
    }
  }
  function closeCodeViewer(){
    if (!codeViewerOverlay) return;
    codeViewerOverlay.classList.remove('open');
    document.body.style.overflow = '';
  }
  document.addEventListener('keydown', (e) => { if (e.key === 'Escape') closeCodeViewer(); });
  document.addEventListener('click', (e) => {
    const trigger = e.target.closest('[data-code-viewer]');
    if (!trigger) return;
    e.preventDefault();
    openCodeViewer(trigger.dataset.codeViewer, trigger.dataset.codeTitle || trigger.textContent.trim());
  });

  /* ---------- project rendering ---------- */
  const grid = document.getElementById('project-grid');
  const tabs = document.querySelectorAll('.tab-btn');

  const linkMeta = {
    dashboard:     { label: 'Live Dashboard', icon: 'bi-bar-chart-line' },
    presentation:  { label: 'Presentation', icon: 'bi-easel2', viewer: 'doc' },
    rScript:       { label: 'R Script', icon: 'bi-filetype-raw', viewer: 'code' },
    dataset:       { label: 'Dataset',      icon: 'bi-database' },
    kaggleDataset: { label: 'Kaggle Data',  icon: 'bi-database-fill' },
    kaggleNotebook:{ label: 'Kaggle Notebook', icon: 'bi-code-slash' },
    notebookDownload: { label: 'Download Notebook', icon: 'bi-download' },
    github:        { label: 'GitHub',       icon: 'bi-github' },
  };

  // Builds the right markup for a link entry: a viewer-trigger button for
  // presentations/R scripts (keeps the visit on-site), a plain external
  // link for everything else (dashboards, GitHub, raw datasets).
  function linkActionHTML(key, url, label, icon){
    const meta = linkMeta[key] || {};
    if (meta.viewer === 'doc') {
      return `<a class="pc-link" href="${url}" data-doc-viewer="${url}" data-doc-title="${label}"><i class="bi ${icon}"></i>${label}</a>`;
    }
    if (meta.viewer === 'code') {
      return `<a class="pc-link" href="${url}" data-code-viewer="${url}" data-code-title="${label}"><i class="bi ${icon}"></i>${label}</a>`;
    }
    return `<a class="pc-link" href="${url}" target="_blank" rel="noopener noreferrer"><i class="bi ${icon}"></i>${label}</a>`;
  }

 function cardHTML(p, idx) {
  const links = p.links || {};
  let actions = `<a class="pc-link" href="case-study.html?id=${p.id}"><i class="bi bi-file-earmark-text"></i>Case Study</a>`;
  Object.keys(linkMeta).forEach(key => {
    if (links[key]) {
      const meta = linkMeta[key];
      actions += linkActionHTML(key, links[key], meta.label, meta.icon);
    }
  });
  if (p.notebook) {
    actions += `<a class="pc-link" href="${nbviewerLink(p.notebook)}" target="_blank" rel="noopener noreferrer"><i class="bi bi-journal-code"></i>Notebook</a>`;
  }

  const tags = (p.stack || []).map(t => `<span>${t}</span>`).join('');
  const catLabel = p.category === 'data-science' ? 'Data Science' : 'Analytics';

  return `
    <div class="project-card reveal" style="--i:${idx}">
      <a class="pc-media" href="case-study.html?id=${p.id}">
        <span class="pc-cat">${catLabel}</span>
        <img src="${p.image}" alt="${p.title}" loading="lazy">
      </a>
      <div class="pc-body">
        <h4><a href="case-study.html?id=${p.id}">${p.title}</a></h4>
        <div class="pc-tags">${tags}</div>
      </div>
      <div class="pc-actions">${actions}</div>
    </div>`;
}

  const templateCardHTML = `
    <div class="project-card template reveal">
      <i class="bi bi-plus-circle"></i>
      <h4>More notebooks on the way</h4>
      <p>New Kaggle / Jupyter work gets added here as it ships.</p>
      <p>Adding one yourself? Drop the .ipynb in the repo and list it in <code>projects-data.js</code>.</p>
    </div>`;

  function renderProjects(filter){
    if (!grid || typeof PROJECTS === 'undefined') return;
    const items = PROJECTS.filter(p => filter === 'all' ? true : p.category === filter);
    grid.innerHTML = items.map((p, i) => cardHTML(p, i)).join('') + (filter === 'data-science' ? templateCardHTML : '');
    grid.querySelectorAll('.reveal').forEach((el, i) => {
      requestAnimationFrame(() => el.classList.add('in'));
    });
    bindCardGlow(grid);
  }

  tabs.forEach(btn => {
    btn.addEventListener('click', () => {
      tabs.forEach(b => b.classList.remove('active'));
      btn.classList.add('active');
      renderProjects(btn.dataset.filter);
    });
  });

  renderProjects('all');

  /* ---------- case-study page rendering (single shared template) ---------- */
  const csRoot = document.getElementById('case-study-root');
  if (csRoot && typeof PROJECTS !== 'undefined') {
    const params = new URLSearchParams(window.location.search);
    const reqId = params.get('id');
    const idx = PROJECTS.findIndex(p => p.id === reqId);
    const project = idx >= 0 ? PROJECTS[idx] : null;

    if (!project) {
      csRoot.innerHTML = `
        <section class="detail-hero">
          <div class="container">
            <h1>Project not found</h1>
            <p class="detail-sub">That project doesn't exist, or the link is out of date.</p>
            <a href="index.html#projects" class="btn btn-primary" style="margin-top:24px;"><i class="bi bi-arrow-left"></i> Back to Projects</a>
          </div>
        </section>`;
    } else {
      document.title = `${project.title} — M. Abdullah`;
      const prevP = PROJECTS[(idx - 1 + PROJECTS.length) % PROJECTS.length];
      const nextP = PROJECTS[(idx + 1) % PROJECTS.length];

      const csLinkMeta = {
        rScript:        { label: 'R Script', icon: 'bi-filetype-raw', viewer: 'code' },
        kaggleNotebook: { label: 'Kaggle Notebook', icon: 'bi-journal-code' },
        notebookDownload: { label: 'Download Notebook (.ipynb)', icon: 'bi-download' },
        models: { label: 'Model Files', icon: 'bi-diagram-3' },
        model: { label: 'Model File', icon: 'bi-diagram-3' },
        dashboard:      { label: 'Live Dashboard', icon: 'bi-bar-chart-line' },
        presentation:   { label: 'Final Presentation', icon: 'bi-easel2', viewer: 'doc' },
        dataset:        { label: 'Dataset', icon: 'bi-database' },
        kaggleDataset:  { label: 'Kaggle Data', icon: 'bi-database-fill' },
        github:         { label: 'GitHub Folder', icon: 'bi-github' },
      };
      let infoLinks = '';
      Object.keys(csLinkMeta).forEach(key => {
        const val = (project.links || {})[key];
        if (val) {
          const meta = csLinkMeta[key];
          if (meta.viewer === 'doc') {
            infoLinks += `<a class="pc-link" href="${val}" data-doc-viewer="${val}" data-doc-title="${meta.label}"><i class="bi ${meta.icon}"></i> ${meta.label}</a>`;
          } else if (meta.viewer === 'code') {
            infoLinks += `<a class="pc-link" href="${val}" data-code-viewer="${val}" data-code-title="${meta.label}"><i class="bi ${meta.icon}"></i> ${meta.label}</a>`;
          } else {
            infoLinks += `<a class="pc-link" href="${val}" target="_blank" rel="noopener noreferrer"><i class="bi ${meta.icon}"></i> ${meta.label}</a>`;
          }
        }
      });
      if (project.notebook) {
        infoLinks += `<a class="pc-link" href="${nbviewerLink(project.notebook)}" target="_blank" rel="noopener noreferrer"><i class="bi bi-journal-code"></i> Notebook</a>`;
      }

      const chips = (project.stack || []).map(t => `<span class="chip">${t}</span>`).join('');
      const toolBars = (project.stack || [])
        .map((t, i) => skillBarRowHTML({ name: t, percent: skillPercentFor(t) }, i))
        .join('');
      const gallery = (project.gallery && project.gallery.length) ? project.gallery : [project.image];
      const slides = gallery.map((src, i) => `<img src="${src}" alt="${project.title} — image ${i + 1}" class="${i === 0 ? 'active' : ''}" data-index="${i}">`).join('');
      const dots = gallery.map((_, i) => `<button class="slide-dot ${i === 0 ? 'active' : ''}" data-index="${i}" aria-label="Go to image ${i + 1}"></button>`).join('');
      const steps = (project.steps || []).map(s => `<li><i class="bi bi-check-circle-fill"></i><span><b>${s.t}:</b> ${s.d}</span></li>`).join('');

      csRoot.innerHTML = `
        <section class="detail-hero">
          <div class="container">
            <div class="breadcrumb">
              <a href="index.html">Home</a><span class="sep">/</span>
              <a href="index.html#projects">Projects</a><span class="sep">/</span>
              <span class="current">${project.title}</span>
            </div>
            <div class="cell-tag"><b>In [${idx + 1}]:</b> load_project("${project.id}")</div>
            <h1>${project.title}</h1>
            <p class="detail-sub">${project.subtitle || project.blurb}</p>
            <div class="detail-chips">${chips}</div>
          </div>
        </section>

        <section class="detail-body">
          <div class="container detail-grid">
            <div class="reveal">
              <div class="slideshow-card">
                <div class="slideshow-bar">
                  <span class="dot" style="background:#ff5f56"></span>
                  <span class="dot" style="background:#ffbd2e"></span>
                  <span class="dot" style="background:#27c93f"></span>
                  <span>output.png</span>
                  <span class="counter mono"><span id="slide-current">1</span> / ${gallery.length}</span>
                </div>
                <div class="slideshow-viewport">
                  <div class="slideshow-track" id="slideshow-track">${slides}</div>
                  <button class="slide-btn slide-prev" aria-label="Previous image"><i class="bi bi-chevron-left"></i></button>
                  <button class="slide-btn slide-next" aria-label="Next image"><i class="bi bi-chevron-right"></i></button>
                </div>
                ${gallery.length > 1 ? `<div class="slideshow-foot">${dots}</div>` : ''}
              </div>

              <div class="detail-section">
                <h2>Objective</h2>
                <p class="detail-lede">${project.objective || ''}</p>

                <h3>Analysis &amp; Key Steps</h3>
                <ul class="check-list">${steps}</ul>

                <h3>Key Finding &amp; Recommendation</h3>
                <div class="callout-grid">
                  <div class="callout finding">
                    <b><i class="bi bi-search"></i> Finding</b>
                    <p>${project.finding || ''}</p>
                  </div>
                  <div class="callout recommend">
                    <b><i class="bi bi-lightbulb"></i> Recommendation</b>
                    <p>${project.recommendation || ''}</p>
                  </div>
                </div>
              </div>
            </div>

            <aside class="info-panel reveal">
              <div class="cell-tag"><b>Out[${idx + 1}]:</b> project_info</div>
              <h3>Project Information</h3>
              <div class="info-row"><b>Data Source</b><span>${project.dataSource || '—'}</span></div>
              <div class="info-row">
                <b>Tools &amp; Proficiency</b>
                <div class="skill-bars" style="margin-top:10px;">${toolBars}</div>
              </div>
              <div class="info-links">${infoLinks}</div>
            </aside>
          </div>

          <div class="container">
            <div class="project-nav reveal">
              <a href="case-study.html?id=${prevP.id}" class="prev">
                <span class="lbl"><i class="bi bi-arrow-left"></i> Previous</span>
                <span class="title">${prevP.title}</span>
              </a>
              <a href="case-study.html?id=${nextP.id}" class="next">
                <span class="lbl">Next <i class="bi bi-arrow-right"></i></span>
                <span class="title">${nextP.title}</span>
              </a>
            </div>
          </div>
        </section>`;

      watchBars(csRoot);

      const track = document.getElementById('slideshow-track');
      const slideImgs = track.querySelectorAll('img');
      const slideDots = csRoot.querySelectorAll('.slide-dot');
      const counterEl = document.getElementById('slide-current');
      let current = 0;
      const goTo = (i) => {
        current = (i + slideImgs.length) % slideImgs.length;
        slideImgs.forEach((img, n) => img.classList.toggle('active', n === current));
        slideDots.forEach((d, n) => d.classList.toggle('active', n === current));
        if (counterEl) counterEl.textContent = current + 1;
      };
      csRoot.querySelector('.slide-prev')?.addEventListener('click', () => goTo(current - 1));
      csRoot.querySelector('.slide-next')?.addEventListener('click', () => goTo(current + 1));
      slideDots.forEach(d => d.addEventListener('click', () => goTo(parseInt(d.dataset.index, 10))));
      document.addEventListener('keydown', (e) => {
        if (e.key === 'ArrowLeft') goTo(current - 1);
        if (e.key === 'ArrowRight') goTo(current + 1);
      });
      slideImgs.forEach(img => img.addEventListener('click', () => {
  const activeImg = slideImgs[current];
  openLightbox(activeImg.src, activeImg.alt);
}));

      let autoplay = setInterval(() => goTo(current + 1), 4500);
      const slideCard = csRoot.querySelector('.slideshow-card');
      slideCard?.addEventListener('mouseenter', () => clearInterval(autoplay));
      slideCard?.addEventListener('mouseleave', () => { autoplay = setInterval(() => goTo(current + 1), 4500); });
    }
  }

  /* ---------- contact form (FormSubmit — no backend required) ---------- */
  const form = document.getElementById('contact-form');
  const status = document.getElementById('form-status');
  form?.addEventListener('submit', async (e) => {
    e.preventDefault();
    status.textContent = 'Sending…';
    status.className = 'form-status';
    try {
      const res = await fetch(form.action, {
        method: 'POST',
        body: new FormData(form),
        headers: { 'Accept': 'application/json' }
      });
      if (res.ok) {
        status.textContent = 'Message sent — thanks! I\'ll reply by email soon.';
        status.className = 'form-status ok';
        form.reset();
      } else {
        throw new Error('send failed');
      }
    } catch (err) {
      status.textContent = 'Something went wrong — please email me directly instead.';
      status.className = 'form-status err';
    }
  });

});
