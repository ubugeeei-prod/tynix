import assert from "node:assert/strict";
import { test } from "node:test";
import { processNavigation } from "./navigation.ts";

const article = (content: string) => `<main class="main"><article class="content">${content}</article></main>`;
const sections = '<h2 id="install">Install <code>tynix</code></h2><h2 id="verify">Check &amp; verify</h2>';

test("matches the current navigation item across directory and index routes", () => {
  const input = '<link rel="canonical" href="https://tynix.dev/tutorial/install/">' +
    '<a href="/tutorial/index.html" class="nav-link">Tutorial</a>' +
    '<a href="/tutorial/install/index.html" class="nav-link">Install</a>';
  const output = processNavigation(input);
  assert.equal((output.match(/aria-current="page"/gu) ?? []).length, 1);
  assert.ok(output.includes('class="nav-link active" aria-current="page">Install'));
});

test("builds section links from article headings while preserving escaped text", () => {
  const output = processNavigation(article(sections));
  assert.ok(output.includes('<main class="main main--with-toc">'));
  assert.ok(output.includes('<a href="#install">Install tynix</a>'));
  assert.ok(output.includes('<a href="#verify">Check &amp; verify</a>'));
  assert.ok(output.includes('<details class="tx-page-index">'));
  assert.ok(output.includes('<aside class="toc tx-toc" aria-label="On this page">'));
});

test("leaves short articles and entry pages free of a section index", () => {
  const short = article('<h2 id="one">One section</h2>');
  assert.equal(processNavigation(short), short);
  const entry = '<body class="entry-page">' + article(sections) + '</body>';
  assert.equal(processNavigation(entry), entry);
});

test("preserves a renderer-supplied table of contents", () => {
  const input = article(sections) + '<aside class="toc">Contents</aside>';
  assert.equal(processNavigation(input), input);
});

test("can process an already enhanced page without duplicating navigation", () => {
  const once = processNavigation(article(sections));
  assert.equal(processNavigation(once), once);
});
