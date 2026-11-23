(in-package #:org-markup)

;;; ------------------------------------------------------------------
;;; Org: a subset of Org-mode syntax (org-element 9.x) for documents such
;;; as blog posts, parsed into markdown-protocol's AST (plus :image) so one
;;; renderer shows .md and .org. Where this header and Org differ, this
;;; header wins.
;;;
;;; BLOCKS (line based; a container takes its lines, strips its markers or
;;; indentation and parses the rest recursively):
;;;  - Lines split at LF; one CR before it is dropped. Tabs in a line's
;;;    leading whitespace become spaces (8-column stops), also inside
;;;    blocks. Blank = only spaces and tabs.
;;;  - Headings: 1+ * at column 0, then a space or the end of the line,
;;;    outside list items. Level = star count, capped at 6. Stripped, in
;;;    this order: a TODO or DONE keyword, a priority cookie [#A], a
;;;    COMMENT keyword, trailing tags :a:b: (after a blank, or the whole
;;;    title). A heading tagged noexport, or marked COMMENT, is dropped
;;;    with its subtree: up to the next heading with as many stars or
;;;    fewer (raw count, not capped). A planning line (SCHEDULED:,
;;;    DEADLINE:, CLOSED: first) right after a heading is dropped.
;;;  - Drawers: a line :NAME: (letters, digits, - and _) through the next
;;;    :END: line (any case) are dropped, :PROPERTIES: included. Without
;;;    an :END: in the container the line is not a drawer (text).
;;;  - Comments: a line # followed by a blank or the end, after optional
;;;    indent, is dropped.
;;;  - #+BEGIN_NAME args … #+END_NAME, any case, optional indent; the first
;;;    matching end line closes it; unclosed runs to the end of its
;;;    container.
;;;      SRC     → (:code-block lang lines); lang = first word of ARGS, NIL
;;;                if none or it starts with - or : (switches, headers).
;;;                Babel's :exports decides what shows (EXPORTS below).
;;;      EXAMPLE → (:code-block nil lines).
;;;                Content lines verbatim, except: their common smallest
;;;                indentation is removed, and the comma escaping a leading
;;;                * or #+ (after indent) is removed: ",* a" → "* a",
;;;                ",,* a" → ",* a".
;;;      QUOTE   → (:block-quote blocks), contents parsed as blocks.
;;;      VERSE   → one (:paragraph …): leading / trailing blank lines
;;;                dropped, common indentation removed, each line parsed
;;;                inline (relative indentation stays as text), lines joined
;;;                by (:hard-break). Empty → nothing.
;;;      EXPORT, COMMENT → dropped.
;;;      CENTER and any other name → contents parsed as blocks, spliced in.
;;;    #+BEGIN: name … #+END: (dynamic blocks) → the two marker lines are
;;;    dropped, the contents spliced in like CENTER. Keyword lines in a
;;;    block parsed as blocks are dropped and not collected (in SRC,
;;;    EXAMPLE and EXPORT they are content). A #+END_…, #+END: or :END:
;;;    line that closes nothing is dropped.
;;;  - Keyword lines #+KEY: value (optional indent; KEY = non-blank chars
;;;    up to the first ":") are dropped from the blocks and collected as
;;;    KEYWORDS (below).
;;;  - Fixed width: consecutive lines ": text" or ":" alone (optional
;;;    indent) → (:code-block nil lines), the ": " / ":" prefix stripped,
;;;    the rest verbatim.
;;;  - Horizontal rule: 5+ - alone on a line (blanks around) →
;;;    (:thematic-break).
;;;  - Tables: consecutive lines starting with | (after indent). Lines |-…
;;;    or |+… are rules. A row whose cells are all alignment cookies (<l>
;;;    <r> <c>, optionally with a width: <l10>, or a width alone: <10>) or
;;;    empty, with at least one cookie, is dropped; the first such row sets
;;;    ALIGNMENTS (:left :right :center NIL), else all NIL. Then, when a
;;;    rule directly follows the first data row, that row is the HEADER,
;;;    else HEADER is NIL. Header and rows are padded with empty cells to
;;;    the widest row. Cells: outer pipes dropped, split at every |, trimmed,
;;;    parsed inline. A table with no data rows → nothing.
;;;  - Lists: an item line is indent + a bullet (- or +, or * when the item
;;;    is indented or inside another item) or an ordered marker (1–9
;;;    digits then . or )), then a space or the end of the line. The
;;;    content column is the first non-blank after the marker. Lines
;;;    indented past the bullet belong to the item, and so does a whole
;;;    #+BEGIN … #+END block that starts in it (whatever the indent of its
;;;    contents); one blank line belongs when the next line is indented
;;;    past the bullet; two blank lines end the list. The item body is the
;;;    text after the marker plus those lines dedented to the content
;;;    column (a block's contents all by the same amount, at most that
;;;    column, so they keep their relative indent), parsed as blocks; no
;;;    headings inside items. One list = consecutive items at the same
;;;    indent and of the same kind (bullet or ordered; - and + mix, . and
;;;    ) mix). Ordered: start = the first
;;;    number, delimiter from the first item; a counter cookie [@N] at the
;;;    start of an ordered item's text is removed, and on the first item
;;;    sets the start. Tight unless a blank line separates items or two
;;;    blocks of an item. Checkboxes [ ] [X] [-] stay text. Description
;;;    items (bullet lists only): "- term :: desc" → the item's first
;;;    paragraph is ((:strong term-inlines) " — " desc-inlines…); when the
;;;    item starts with no paragraph, ((:strong term-inlines)) is put
;;;    before its blocks.
;;;  - Paragraphs: other lines, up to a blank line or a line that starts an
;;;    element above (heading, #+BEGIN, stray end line, keyword, comment,
;;;    drawer, fixed width, rule, table, list item). Each line is trimmed;
;;;    the lines are joined by newlines and parsed inline. A #+word line
;;;    with no colon is paragraph text.
;;;  - Not supported: footnotes ([fn:…] stays text), LaTeX fragments and
;;;    environments (text), inline tasks, babel calls, #+INCLUDE (a
;;;    keyword), internal links ([[*Heading]] is a link with that target),
;;;    the ARCHIVE tag, export options (#+OPTIONS is a keyword; the
;;;    caller applies ORG-OPTIONS).
;;;  - EXPORTS (org-babel's, as at export with no evaluation). A SRC
;;;    block's :exports is the word after its last :exports header
;;;    argument; else the default the document's #+PROPERTY: lines give:
;;;    header-args:LANG for the block's language, then header-args; else
;;;    "code". Those lines count anywhere outside #+BEGIN blocks, in
;;;    order: header-args[:LANG] replaces the value (none when it holds no
;;;    :exports), header-args+[:LANG] only overrides it when it holds one.
;;;    Property drawers' header-args are not read.
;;;      none     the block and its results are dropped
;;;      code     the block stays, its results are dropped
;;;      results  the block is dropped, its results stay
;;;      both, anything else   both stay (Org leaves the block as is)
;;;    The results: when the first non-blank line after the block is
;;;    #+RESULTS: (or #+RESULTS[hash]:), the element on the line right
;;;    after it (not a heading or a blank line, else there are none). The
;;;    #+RESULTS: line is dropped either way.
;;;  - Content inside more than 64 nested containers becomes one paragraph
;;;    of its text (lines trimmed, blank lines dropped), and emphasis
;;;    inside more than 64 nested emphases or links stays text, so no
;;;    input exhausts the stack.
;;;
;;; KEYWORDS: (("title" . "My post") …) in source order, from the #+KEY:
;;; value lines outside #+BEGIN blocks, drawers and dropped subtrees (list
;;; items count). Key downcased, value trimmed; a repeated key appears
;;; again. Dropped and not collected: caption, name, results, header, plot,
;;; tblfm and attr_* (an optional [..] after the key is ignored).
;;; ORG-OPTIONS reads the #+OPTIONS words (toc:t H:3 email:t) of KEYWORDS.
;;;
;;; INLINES (one left-to-right scan; at each position the first object that
;;; parses wins, in this order):
;;;  - Line break: \\ at the end of a line (blanks may follow, the char
;;;    before is not a backslash) → (:hard-break); at the end of the text
;;;    it is dropped. Any other newline → (:soft-break). Blanks around a
;;;    newline are dropped.
;;;  - Bracket links [[target][description]] → (:link target desc-inlines)
;;;    and [[target]] → (:image target ()) when the target ends in .png
;;;    .jpg .jpeg .gif .svg .webp (any case), else (:link target
;;;    (target)), the text being the target as stripped below. The target
;;;    ends at the first unescaped ]; an unescaped [
;;;    or an empty target is no link. The description ends at the first ]]
;;;    outside balanced brackets (else the first ]]); an empty one counts
;;;    as none. In target and description \[ \] \\ → [ ] \, and a newline
;;;    with its blanks → one space (in the target). A file:, file+sys: or
;;;    file+emacs: prefix is stripped from targets. The description is
;;;    parsed inline without links: only image links [[x.png]] parse there
;;;    (→ :image); a whole description that is a file link (any of those
;;;    prefixes) or http(s) image link ([[page][file:t.png]]) →
;;;    ((:image "t.png" ())).
;;;  - Angle links <https://…>, <http://…>, <mailto:…> → (:link url (url)),
;;;    brackets dropped; up to the first > with no < or newline before it.
;;;  - Plain links https://… http://… mailto:… (lowercase, not after a
;;;    letter or digit) → (:link url (url)). The URL ends at a blank or one
;;;    of <>[]()" and drops trailing .,;:!?' characters; it needs a char
;;;    after the scheme.
;;;  - Emphasis (org-emphasis-regexp-components defaults): an opening marker
;;;    at the start of the text or after a blank or one of -({'" , followed
;;;    by a non-blank; the closing marker is the first same marker after a
;;;    non-blank and followed by the end, a blank or one of -.,;:!?')}["\ ;
;;;    the contents hold at most one newline. *x* → (:strong …), /x/ →
;;;    (:emph …), _x_ → (:emph …) (the AST has no underline), +x+ →
;;;    (:strike …), contents parsed inline. =x= and ~x~ → (:code "x"),
;;;    contents literal, newlines → spaces. So snake_case, a/b/c and 2*3*4
;;;    stay text.
;;;  - Everything else is text: entities (\alpha), $math$, \(…\), macros
;;;    {{{x}}}, export snippets @@html:…@@, timestamps, targets <<x>>,
;;;    statistics cookies [2/3], sub/superscripts (a_b), footnote
;;;    references.

(defun parse-org (string)
  "Parse STRING (the Org subset above) → (values blocks keywords). A
non-string → TYPE-ERROR. Never signals on any string.

Blocks:  (:paragraph inlines) (:heading level inlines) (:thematic-break)
         (:code-block language lines)       ; language: string or NIL
         (:block-quote blocks)
         (:bullet-list tight items)          ; items: list of block lists
         (:ordered-list start delimiter tight items)   ; delimiter #\\. or #\\)
         (:table alignments header rows)     ; alignment :left :right :center NIL;
                                             ; header: cells or NIL; rows: lists
                                             ; of cells; a cell is a list of inlines
Inlines: string  (:emph inlines) (:strong inlines) (:strike inlines)
         (:code string) (:link url inlines) (:image url inlines)
         (:soft-break) (:hard-break)
Keywords: ((key . value) …) in source order, key downcased, value trimmed.
Adjacent strings are merged and empty strings dropped, so results compare
with EQUAL."
  (check-type string string)
  (let* ((lines (%source-lines string))
         (*keywords* '())
         (*collect-keywords* t)
         (*in-item* nil)
         (*depth* 0)
         (*inline-depth* 0)
         (*line-searches* (make-hash-table :test 'eq))
         (*exports-defaults* (%exports-defaults lines)))
    (let ((blocks (values (%parse-blocks lines))))
      (values blocks (reverse *keywords*)))))

(defun %option-value (raw)
  (cond ((string-equal raw "t") t)
        ((string-equal raw "nil") nil)
        ((and (plusp (length raw)) (every #'%digit-p raw)) (parse-integer raw))
        (t raw)))

(defun org-options (keywords)
  "The export options in KEYWORDS' #+OPTIONS lines (KEYWORDS as PARSE-ORG
returns them), merged in order so a later word wins → an alist (name .
value), names as written. A word NAME:VALUE splits at its last colon;
VALUE t → T, nil → NIL, digits → an integer, else the string. Words with
no colon or an empty name are skipped. (Org's own defaults are not added.)"
  (check-type keywords list)
  (let ((options '()))
    (loop for (key . value) in keywords
          when (equal key "options")
            do (dolist (word (%words value))
                 (let ((colon (position #\: word :from-end t)))
                   (when (and colon (plusp colon))
                     (let ((name (subseq word 0 colon)))
                       (setf options (remove name options :key #'car :test #'string=))
                       (push (cons name (%option-value (subseq word (1+ colon)))) options))))))
    (nreverse options)))
