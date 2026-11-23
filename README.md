# org-markup

An Org-mode subset parser for [cl-stack](https://github.com/egao1980/cl-stack). It returns the same plain-list AST as [`markdown-protocol`](https://github.com/egao1980/markdown-protocol), plus `:image`, so one renderer shows both `.md` and `.org` files, and the `#+KEY: value` lines as a keyword alist. It has no dependencies.

Written for a static blog generator with posts in Org: the keywords carry the post metadata, and the blocks go to the same HTML renderer as markdown posts. The terminal renderer [`tui-markdown`](https://github.com/egao1980/tui-markdown) takes the same AST.

```lisp
(asdf:load-system "org-markup")

(stack-org:parse-org "#+TITLE: Hi
* Hello
Some /emphasis/ and [[https://example.com][a link]].")
;; → ((:heading 1 ("Hello"))
;;    (:paragraph ("Some " (:emph ("emphasis")) " and "
;;                 (:link "https://example.com" ("a link")) ".")))
;;   (("title" . "Hi"))
```

## API

| Function | Result |
|----------|--------|
| `(parse-org string)` | `(values blocks keywords)`: KEYWORDS is `(("title" . "My post") …)` in source order, keys downcased, values trimmed; affiliated keywords (`caption`, `name`, `results`, `header`, `plot`, `tblfm`, `attr_*`) are dropped |
| `(org-options keywords)` | the `#+OPTIONS` words of KEYWORDS merged in order: `(("toc" . t) ("H" . 3) ("email" . t))`; `t` / `nil` / digits read, other values stay strings. Org's defaults are not added |

A non-string signals `type-error`. Any string parses; text the subset does not cover stays text.

## AST

```text
Blocks:  (:paragraph inlines)  (:heading level inlines)  (:thematic-break)
         (:code-block language lines)                ; language: string or NIL
         (:block-quote blocks)
         (:bullet-list tight items)                  ; items: list of block lists
         (:ordered-list start delimiter tight items) ; delimiter #\. or #\)
         (:table alignments header rows)             ; :left :right :center NIL;
                                                     ; header: cells or NIL;
                                                     ; a cell is a list of inlines
Inlines: string  (:emph inlines)  (:strong inlines)  (:strike inlines)
         (:code string)  (:link url inlines)  (:image url inlines)
         (:soft-break)  (:hard-break)
```

Adjacent strings are merged and empty strings are dropped, so results compare with `EQUAL`. Org's `_underline_` maps to `:emph`, `=verbatim=` and `~code~` to `:code`, description items `- term :: text` to a paragraph starting with `(:strong term)` and `" — "`.

## Subset

Blocks: headings (TODO/DONE keywords, priorities and tags stripped; `noexport` and `COMMENT` subtrees dropped; planning lines dropped), drawers and comments (dropped), `#+BEGIN_SRC` / `EXAMPLE` (with comma unescaping; source blocks and their `#+RESULTS:` follow babel's `:exports`, its default from `#+PROPERTY: header-args[:lang]` lines, else `code`), `QUOTE`, `VERSE`, `CENTER` and special blocks, fixed-width `: lines`, horizontal rules, tables (header from the first rule, `<l>` `<c>` `<r>` alignment cookies), plain lists (bullet, ordered with `[@N]` counters, description, tight/loose, nesting), paragraphs. Inlines: emphasis with Org's pre/post rules (`snake_case` and `a/b/c` stay text), code, bracket links and images (`file:`, `file+sys:` and `file+emacs:` prefixes stripped), angle and plain links, line breaks. Dropped: `#+BEGIN_EXPORT`, `#+BEGIN_COMMENT`, dynamic block markers. Not supported, so left as text: footnotes, LaTeX, entities, macros, export snippets, timestamps, targets, sub/superscripts, inline tasks, babel calls. The exact rules are in the header of [`src/org.lisp`](src/org.lisp).

## Tests

```sh
cl-repo test
```

## License

MIT. See [LICENSE](LICENSE).
