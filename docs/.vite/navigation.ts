// Complete the static reader navigation from the generated document itself.
// Ox Content's renderer receives source paths for pages and URL paths for nav
// items, so its current-page comparison does not match this site's routes.
function route(path: string): string {
  return path.replace(/\/index\.html$/, "/").replace(/\/$/, "") || "/";
}

/** Mark the current page and expose section links without requiring JavaScript. */
export function processNavigation(html: string): string {
  const canonical = html.match(/<link rel="canonical" href="([^"]+)"/u)?.[1];
  if (canonical) {
    const current = route(new URL(canonical).pathname);
    html = html.replace(
      /<a href="([^"]+)" class="nav-link">/gu,
      (anchor, href: string) =>
        route(href) === current
          ? anchor.replace(
              'class="nav-link"',
              'class="nav-link active" aria-current="page"',
            )
          : anchor,
    );
  }

  // A source link to ./index.md means the parent overview. SSG moves each
  // chapter into its own directory but leaves this particular link at ./index.html.
  html = html.replace(/<div class="tx-pager">([\s\S]*?)<\/div>/gu, (pager) =>
    pager.replaceAll('href="./index.html"', 'href="../index.html"'),
  );

  // Entry pages already supply their own overview. Preserve a native TOC if
  // a future renderer supplies one; the fallback reads only article headings.
  if (
    /<aside[^>]+class="[^"]*\btoc\b/u.test(html) ||
    /<body[^>]+class="[^"]*\bentry-page\b/u.test(html)
  )
    return html;
  let hasSectionIndex = false;
  const indexed = html.replace(
    /<article class="content">([\s\S]*?)<\/article>/u,
    (article, content: string) => {
      const headings = [
        ...content.matchAll(/<h2 id="([^"]+)">([\s\S]*?)<\/h2>/gu),
      ];
      if (headings.length < 2) return article;
      hasSectionIndex = true;
      const links = headings
        .map(
          ([, id, title]) =>
            `<li><a href="#${id}">${title.replace(/<[^>]+>/gu, "")}</a></li>`,
        )
        .join("");
      const mobile = `<details class="tx-page-index"><summary>On this page</summary><ul>${links}</ul></details>`;
      const desktop = `<aside class="toc tx-toc" aria-label="On this page"><p class="toc-title">On this page</p><ul>${links}</ul></aside>`;
      return `<article class="content">${content.replace(/<h2 /u, `${mobile}<h2 `)}</article>${desktop}`;
    },
  );
  return hasSectionIndex
    ? indexed.replace(
        '<main class="main">',
        '<main class="main main--with-toc">',
      )
    : indexed;
}
