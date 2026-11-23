(in-package #:org-markup)

;;; ------------------------------------------------------------------
;;; Block parser (the BLOCKS rules in org.lisp's header). Lines are a
;;; simple-vector of strings; a container (list item, #+BEGIN block)
;;; collects its lines, dedented where the rules say so, into a fresh
;;; vector and parses that. Each element parser takes (lines i end) and
;;; returns (values blocks next): zero blocks for dropped elements, several
;;; for a spliced special block.

(defvar *keywords* '()
  "The (key . value) pairs collected so far, newest first.")

(defvar *collect-keywords* t
  "True where keyword lines are collected: NIL inside #+BEGIN blocks.")

(defvar *in-item* nil
  "True inside a list item: no headings, and * is a bullet at any indent.")

(defvar *depth* 0
  "Container nesting depth of the current %PARSE-BLOCKS call.")

(defun %expand-leading-tabs (line)
  "LINE with the tabs of its leading whitespace expanded to 8-column stops."
  (let ((end (or (position-if-not #'%blank-char-p line) (length line))))
    (if (not (find #\Tab line :end end))
        line
        (with-output-to-string (out)
          (let ((col 0))
            (loop for i from 0 below end
                  for ch = (char line i)
                  do (if (char= ch #\Tab)
                         (let ((k (- 8 (mod col 8))))
                           (loop repeat k do (write-char #\Space out))
                           (incf col k))
                         (progn (write-char ch out) (incf col)))))
          (write-string line out :start end)))))

(defun %source-lines (string)
  "STRING → a simple-vector of lines: split at LF, one CR before it dropped,
no empty tail after a final LF, leading tabs expanded."
  (let ((lines '())
        (n (length string))
        (start 0))
    (loop while (< start n)
          do (let* ((p (position #\Newline string :start start))
                    (end (or p n))
                    (e (if (and p (> end start) (char= (char string (1- end)) #\Return))
                           (1- end)
                           end)))
               (push (%expand-leading-tabs (subseq string start e)) lines)
               (setf start (if p (1+ p) n))))
    (coerce (nreverse lines) 'simple-vector)))

;;; Line predicates

(defun %blank-p (line)
  (every #'%blank-char-p line))

(defun %indent (line)
  (or (position-if-not (lambda (ch) (char= ch #\Space)) line) (length line)))

(defun %trim (s) (string-trim '(#\Space #\Tab) s))

(defun %digit-p (ch) (char<= #\0 ch #\9))

(defun %run-length (s i n ch)
  (let ((j i))
    (loop while (and (< j n) (char= (char s j) ch)) do (incf j))
    (- j i)))

(defun %dedent (line col)
  (subseq line (min (%indent line) col)))

(defun %min-indent (lines)
  "The smallest indentation of the non-blank LINES (0 when all are blank)."
  (let ((m nil))
    (dolist (l lines (or m 0))
      (unless (%blank-p l)
        (setf m (min (or m most-positive-fixnum) (%indent l)))))))

(defun %after-indent-prefix-p (prefix line)
  "Does LINE start with PREFIX (ignoring case) after its indentation?"
  (let ((ind (%indent line))
        (k (length prefix)))
    (and (<= (+ ind k) (length line))
         (string-equal prefix line :start2 ind :end2 (+ ind k)))))

;;; Headings

(defun %heading (line)
  "→ the star count of a heading line, else NIL. No headings inside items."
  (unless *in-item*
    (let* ((n (length line))
           (k (%run-length line 0 n #\*)))
      (when (and (plusp k) (or (= k n) (char= (char line k) #\Space)))
        k))))

(defun %strip-word (s word)
  "S without a leading WORD that a space or the end follows. → (values rest
found-p)."
  (let ((k (length word))
        (n (length s)))
    (if (and (>= n k) (string= word s :end2 k) (or (= n k) (char= (char s k) #\Space)))
        (values (string-left-trim " " (subseq s k)) t)
        (values s nil))))

(defun %strip-priority (s)
  (let ((n (length s)))
    (if (and (>= n 4) (char= (char s 0) #\[) (char= (char s 1) #\#)
             (not (%ws-p (char s 2))) (char= (char s 3) #\])
             (or (= n 4) (char= (char s 4) #\Space)))
        (string-left-trim " " (subseq s 4))
        s)))

(defun %tag-token-p (tok)
  (let ((n (length tok)))
    (and (>= n 3)
         (char= (char tok 0) #\:)
         (char= (char tok (1- n)) #\:)
         (every (lambda (ch) (or (alphanumericp ch) (find ch "_@#%:"))) tok)
         (find-if-not (lambda (ch) (char= ch #\:)) tok))))

(defun %split-tags (tok)
  (loop with start = 0
        for p = (position #\: tok :start start)
        while p
        when (> p start) collect (subseq tok start p)
        do (setf start (1+ p))))

(defun %heading-parts (line stars)
  "→ (values title tags commented-p) of a heading line with STARS stars."
  (let ((s (%trim (subseq line stars)))
        (tags '())
        (commented nil))
    (multiple-value-bind (rest found) (%strip-word s "TODO")
      (setf s (if found rest (values (%strip-word s "DONE")))))
    (setf s (%strip-priority s))
    (multiple-value-setq (s commented) (%strip-word s "COMMENT"))
    (let* ((sp (position-if #'%blank-char-p s :from-end t))
           (tok (subseq s (if sp (1+ sp) 0))))
      (when (%tag-token-p tok)
        (setf tags (%split-tags tok)
              s (subseq s 0 (or sp 0)))))
    (values (%trim s) tags commented)))

(defun %planning-line-p (line)
  (let ((s (string-left-trim " " line)))
    (some (lambda (w) (%prefix-at-p w s 0 (length s)))
          '("SCHEDULED:" "DEADLINE:" "CLOSED:"))))

;;; #+ lines, comments, drawers

(defun %block-begin (line)
  "→ (values name args) for a #+BEGIN_NAME line (NAME downcased), (values
:dynamic args) for #+BEGIN:, else NIL."
  (let ((ind (%indent line))
        (n (length line)))
    (cond ((%after-indent-prefix-p "#+BEGIN_" line)
           (let* ((ns (+ ind 8))
                  (ne (or (position-if #'%blank-char-p line :start ns) n)))
             (when (> ne ns)
               (values (string-downcase (subseq line ns ne)) (%trim (subseq line ne))))))
          ((%after-indent-prefix-p "#+BEGIN:" line)
           (values :dynamic (%trim (subseq line (+ ind 8))))))))

(defun %block-end-p (line name)
  (let ((marker (if (eq name :dynamic) "#+END:" (concatenate 'string "#+END_" name))))
    (and (%after-indent-prefix-p marker line)
         (%blank-p (subseq line (+ (%indent line) (length marker)))))))

(defvar *line-searches* nil
  "EQ hash table: lines vector → list of (key end from at), the last forward
search for KEY in that vector; bound by PARSE-ORG.")

(defun %find-line (lines start end key test)
  "Index of the first line of LINES[START, END) satisfying TEST, or NIL.
The last result per LINES and KEY (an EQUAL key) answers later searches
that start between its start and its result, so unclosed blocks and
drawers stay linear."
  (let ((memo (and *line-searches*
                   (assoc key (gethash lines *line-searches*) :test #'equal))))
    (if (and memo
             (destructuring-bind (m-end m-from m-at) (rest memo)
               (and (eql m-end end) (>= start m-from) (or (null m-at) (<= start m-at)))))
        (fourth memo)
        (let ((at (loop for j from start below end
                        when (funcall test (svref lines j)) return j)))
          (when *line-searches*
            (if memo
                (setf (rest memo) (list end start at))
                (push (list key end start at) (gethash lines *line-searches*))))
          at))))

(defun %find-block-end (lines name start end)
  (%find-line lines start end name (lambda (l) (%block-end-p l name))))

(defun %drawer-end-p (line)
  (string-equal (%trim line) ":END:"))

(defun %stray-end-p (line)
  "A #+END_…, #+END: or :END: line that closes nothing."
  (or (%after-indent-prefix-p "#+END_" line)
      (%after-indent-prefix-p "#+END:" line)
      (%drawer-end-p line)))

(defun %keyword-line (line)
  "→ (values key value) for a #+KEY: value line (KEY downcased, VALUE
trimmed), else NIL."
  (when (%after-indent-prefix-p "#+" line)
    (let* ((ks (+ (%indent line) 2))
           (j (position-if (lambda (ch) (or (char= ch #\:) (%ws-p ch))) line :start ks)))
      (when (and j (> j ks) (char= (char line j) #\:))
        (values (string-downcase (subseq line ks j)) (%trim (subseq line (1+ j))))))))

(defparameter *affiliated-keywords* '("caption" "name" "results" "header" "plot" "tblfm"))

(defun %affiliated-p (key)
  "Is KEY (downcased) an affiliated or structural keyword: dropped, not
collected? An optional [..] suffix (#+CAPTION[short]:) is ignored."
  (let ((base (subseq key 0 (or (position #\[ key) (length key)))))
    (or (member base *affiliated-keywords* :test #'string=)
        (%prefix-at-p "attr_" base 0 (length base)))))

(defun %note-keyword (line)
  (multiple-value-bind (key value) (%keyword-line line)
    (when (and *collect-keywords* (not (%affiliated-p key)))
      (push (cons key value) *keywords*))))

(defun %comment-line-p (line)
  (let ((ind (%indent line))
        (n (length line)))
    (and (< ind n)
         (char= (char line ind) #\#)
         (or (= (1+ ind) n) (%blank-char-p (char line (1+ ind)))))))

(defun %drawer-start-p (line)
  (let* ((s (%trim line))
         (n (length s)))
    (and (>= n 3)
         (char= (char s 0) #\:)
         (char= (char s (1- n)) #\:)
         (every (lambda (ch) (or (alphanumericp ch) (find ch "-_"))) (subseq s 1 (1- n)))
         (not (string-equal s ":END:")))))

(defun %drawer-end (lines start end)
  "Index of the :END: line closing the drawer opened at START, or NIL."
  (%find-line lines (1+ start) end :drawer #'%drawer-end-p))

;;; Other line kinds

(defun %fixed-width-content (line)
  "→ the text of a fixed-width line (\": text\" or \":\"), else NIL."
  (let ((ind (%indent line))
        (n (length line)))
    (when (and (< ind n) (char= (char line ind) #\:))
      (cond ((= (1+ ind) n) "")
            ((char= (char line (1+ ind)) #\Space) (subseq line (+ ind 2)))))))

(defun %rule-p (line)
  (let ((s (%trim line)))
    (and (>= (length s) 5) (every (lambda (ch) (char= ch #\-)) s))))

(defun %table-line-p (line)
  (let ((ind (%indent line)))
    (and (< ind (length line)) (char= (char line ind) #\|))))

(defun %table-rule-p (line)
  (let ((ind (%indent line)))
    (and (< (1+ ind) (length line))
         (char= (char line ind) #\|)
         (find (char line (1+ ind)) "-+"))))

(defun %list-item-start (line)
  "→ (values kind indent content-column number delimiter) for a list item
line, else NIL. KIND :bullet or :ordered; the content column is the first
non-blank after the bullet, or the bullet end + 1 when the rest is blank."
  (let ((ind (%indent line))
        (n (length line)))
    (when (< ind n)
      (let ((ch (char line ind))
            kind number delim mend)
        (cond ((or (char= ch #\-) (char= ch #\+)
                   (and (char= ch #\*) (or *in-item* (> ind 0))))
               (setf kind :bullet mend (1+ ind)))
              ((%digit-p ch)
               (let ((j ind))
                 (loop while (and (< j n) (%digit-p (char line j))) do (incf j))
                 (when (and (<= (- j ind) 9) (< j n) (find (char line j) ".)"))
                   (setf kind :ordered
                         number (parse-integer line :start ind :end j)
                         delim (char line j)
                         mend (1+ j))))))
        (when (and kind (or (= mend n) (char= (char line mend) #\Space)))
          (let ((col (%skip-blanks line mend n)))
            (values kind ind (if (= col n) (1+ mend) col) number delim)))))))

(defun %interrupts-p (lines j end)
  "True when LINES[J] starts an element, so it ends a paragraph."
  (let ((line (svref lines j)))
    (or (%heading line)
        (%block-begin line)
        (%stray-end-p line)
        (%keyword-line line)
        (%comment-line-p line)
        (and (%drawer-start-p line) (%drawer-end lines j end))
        (%fixed-width-content line)
        (%rule-p line)
        (%table-line-p line)
        (%list-item-start line))))

;;; Blocks

(defun %parse-blocks (lines &optional (start 0) (end (length lines)))
  "Parse LINES[START, END) → (values blocks gap-p); GAP-P is true when a
blank line separates two of these blocks."
  (let ((blocks '())
        (gap nil)
        (blank-pending nil)
        (i start))
    (loop while (< i end)
          do (if (%blank-p (svref lines i))
                 (progn (setf blank-pending t) (incf i))
                 (multiple-value-bind (new next) (%parse-element lines i end)
                   (dolist (b new)
                     (when (and blank-pending blocks) (setf gap t))
                     (setf blank-pending nil)
                     (push b blocks))
                   (setf i next))))
    (values (nreverse blocks) gap)))

(defun %flat-paragraph (lines)
  "LINES as one paragraph of text: the fallback past +MAX-DEPTH+."
  (let ((parts (loop for l across lines unless (%blank-p l) collect (%trim l))))
    (when parts
      (list (list :paragraph (%parse-inlines (format nil "~{~A~^~%~}" parts)))))))

(defun %parse-container (lines &key (in-item *in-item*) (collect *collect-keywords*))
  "Parse the simple-vector LINES as a container's contents → (values blocks
gap-p)."
  (let ((*in-item* in-item)
        (*collect-keywords* collect))
    (if (>= *depth* +max-depth+)
        (values (%flat-paragraph lines) nil)
        (let ((*depth* (1+ *depth*)))
          (%parse-blocks lines)))))

(defun %parse-element (lines i end)
  "The element at the non-blank line LINES[I] → (values blocks next)."
  (let ((line (svref lines i)))
    (cond
      ((%heading line) (%parse-heading lines i end))
      ((%block-begin line) (%parse-block lines i end))
      ((%stray-end-p line) (values '() (1+ i)))
      ((%keyword-line line) (%note-keyword line) (values '() (1+ i)))
      ((%comment-line-p line) (values '() (1+ i)))
      ((and (%drawer-start-p line) (%drawer-end lines i end))
       (values '() (1+ (%drawer-end lines i end))))
      ((%fixed-width-content line) (%parse-fixed-width lines i end))
      ((%rule-p line) (values (list (list :thematic-break)) (1+ i)))
      ((%table-line-p line) (%parse-table lines i end))
      ((%list-item-start line) (%parse-list lines i end))
      (t (%parse-paragraph lines i end)))))

(defun %parse-heading (lines i end)
  (let* ((line (svref lines i))
         (stars (%heading line)))
    (multiple-value-bind (title tags commented) (%heading-parts line stars)
      (if (or commented (member "noexport" tags :test #'string=))
          ;; drop the subtree: up to the next heading of as many stars or fewer
          (values '()
                  (or (position-if (lambda (l) (let ((k (%heading l))) (and k (<= k stars))))
                                   lines :start (1+ i) :end end)
                      end))
          (let ((next (1+ i)))
            (when (and (< next end) (%planning-line-p (svref lines next)))
              (incf next))
            (values (list (list :heading (min stars 6) (%parse-inlines title))) next))))))

(defun %parse-paragraph (lines i end)
  (let ((parts (list (%trim (svref lines i))))
        (j (1+ i)))
    (loop while (and (< j end)
                     (not (or (%blank-p (svref lines j)) (%interrupts-p lines j end))))
          do (push (%trim (svref lines j)) parts)
             (incf j))
    (values (list (list :paragraph (%parse-inlines (format nil "~{~A~^~%~}" (nreverse parts)))))
            j)))

(defun %parse-fixed-width (lines i end)
  (let ((j i)
        (content '()))
    (loop (let ((c (and (< j end) (%fixed-width-content (svref lines j)))))
            (unless c (return))
            (push c content)
            (incf j)))
    (values (list (list :code-block nil (nreverse content))) j)))

;;; #+BEGIN blocks

(defun %src-language (args)
  "The first word of a #+BEGIN_SRC line's ARGS, NIL when there is none or it
is a switch (-n) or header argument (:results)."
  (let ((w (subseq args 0 (position-if #'%blank-char-p args))))
    (if (or (zerop (length w)) (find (char w 0) "-:"))
        nil
        w)))

(defun %unescape-comma (line)
  "LINE without the comma that escapes a leading * or #+ (also ,,* → ,*)."
  (let ((ind (%indent line))
        (n (length line)))
    (if (and (< ind n)
             (char= (char line ind) #\,)
             (let ((j (position-if-not (lambda (ch) (char= ch #\,)) line :start ind)))
               (and j (or (char= (char line j) #\*)
                          (and (char= (char line j) #\#) (< (1+ j) n)
                               (char= (char line (1+ j)) #\+))))))
        (concatenate 'string (subseq line 0 ind) (subseq line (1+ ind)))
        line)))

(defun %verbatim-lines (lines)
  "SRC / EXAMPLE contents: comma escapes removed, common indentation removed."
  (let* ((ls (map 'list #'%unescape-comma lines))
         (m (%min-indent ls)))
    (mapcar (lambda (l) (subseq l (min m (length l)))) ls)))

(defun %verse (lines)
  "VERSE contents → one paragraph, lines joined by hard breaks; NIL when
empty."
  (let* ((ls (coerce lines 'list))
         (first (position-if-not #'%blank-p ls))
         (last (position-if-not #'%blank-p ls :from-end t)))
    (when first
      (let* ((ls (subseq ls first (1+ last)))
             (m (%min-indent ls)))
        (list (list :paragraph
                    (%merge-inlines
                     (loop for (l . more) on ls
                           append (%parse-inlines
                                   (string-right-trim '(#\Space #\Tab)
                                                      (subseq l (min m (length l)))))
                           when more collect (list :hard-break)))))))))

(defun %parse-block (lines i end)
  (multiple-value-bind (name args) (%block-begin (svref lines i))
    (let* ((close (%find-block-end lines name (1+ i) end))
           (body (subseq lines (1+ i) (or close end))))
      (values
       (cond ((equal name "src")
              (list (list :code-block (%src-language args) (%verbatim-lines body))))
             ((equal name "example")
              (list (list :code-block nil (%verbatim-lines body))))
             ((equal name "quote")
              (list (list :block-quote (values (%parse-container body :collect nil)))))
             ((equal name "verse") (%verse body))
             ((member name '("export" "comment") :test #'equal) '())
             ;; center, dynamic and other special blocks: contents spliced in
             (t (values (%parse-container body :collect nil))))
       (if close (1+ close) end)))))

;;; Tables

(defun %split-row (line)
  "A table row → its cell strings: outer pipes dropped, split at |, trimmed."
  (let* ((s (%trim line))
         (start 1)
         (end (length s)))
    (when (and (> end start) (char= (char s (1- end)) #\|))
      (decf end))
    (loop with p = start
          for bar = (position #\| s :start p :end end)
          collect (%trim (subseq s p (or bar end)))
          while bar
          do (setf p (1+ bar)))))

(defun %cookie-alignment (cell)
  "→ (values alignment t) when CELL is an alignment cookie (<l> <r> <c>,
optionally with a width: <l10>, or a width alone: <10>), else NIL."
  (let ((n (length cell)))
    (when (and (>= n 3) (char= (char cell 0) #\<) (char= (char cell (1- n)) #\>))
      (let* ((a (char cell 1))
             (digits (subseq cell (if (find a "lrc") 2 1) (1- n))))
        (when (every #'%digit-p digits)
          (values (case a (#\l :left) (#\r :right) (#\c :center)) t))))))

(defun %cookie-row (cells)
  "→ the alignments when CELLS are alignment cookies or empty, with at least
one cookie; else NIL."
  (flet ((cookie-p (c) (nth-value 1 (%cookie-alignment c))))
    (when (and (some #'cookie-p cells)
               (every (lambda (c) (or (zerop (length c)) (cookie-p c))) cells))
      (mapcar #'%cookie-alignment cells))))

(defun %parse-table (lines i end)
  (let ((j i)
        (items '())
        (aligns nil))
    (loop while (and (< j end) (%table-line-p (svref lines j)))
          do (let ((line (svref lines j)))
               (if (%table-rule-p line)
                   (push :rule items)
                   (let* ((cells (%split-row line))
                          (cookie (%cookie-row cells)))
                     (cond ((not cookie) (push cells items))
                           ((null aligns) (setf aligns cookie)))))
               (incf j)))
    (setf items (nreverse items))
    (let ((first-row (position-if #'consp items)))
      (if (null first-row)
          (values '() j)
          (let* ((header (and (eq (nth (1+ first-row) items) :rule) (nth first-row items)))
                 (rows (remove :rule (if header (nthcdr (1+ first-row) items) items)))
                 (n (reduce #'max (mapcar #'length (if header (cons header rows) rows)))))
            (flet ((row (cells)
                     (loop for k below n collect (%parse-inlines (or (nth k cells) "")))))
              (values (list (list :table
                                  (loop for k below n collect (nth k aligns))
                                  (and header (row header))
                                  (mapcar #'row rows)))
                      j)))))))

;;; Lists

(defun %same-kind-item-p (line kind indent)
  (and (not (%rule-p line))
       (multiple-value-bind (k ind) (%list-item-start line)
         (and (eq k kind) (= ind indent)))))

(defun %item-lines (lines j end indent col)
  "The lines after the item line LINES[J] that belong to it, dedented to
COL → (values lines next). A #+BEGIN block that starts in the item belongs
to it whole, through its #+END line."
  (let ((out '())
        (p (1+ j)))
    (loop while (< p end)
          do (let ((l (svref lines p)))
               (cond ((%blank-p l)
                      (let ((q (position-if-not #'%blank-p lines :start p :end end)))
                        (if (and q (= q (1+ p)) (> (%indent (svref lines q)) indent))
                            (progn (push "" out) (setf p q))
                            (return))))
                     ((> (%indent l) indent)
                      (let* ((name (%block-begin l))
                             (close (and name (%find-block-end lines name (1+ p) end))))
                        (push (%dedent l col) out)
                        (if (null close)
                            (incf p)
                            ;; the block's contents move left together, by at
                            ;; most COL, keeping their relative indentation
                            (let ((d col))
                              (loop for r from (1+ p) below close
                                    for x = (svref lines r)
                                    unless (%blank-p x) do (setf d (min d (%indent x))))
                              (loop for r from (1+ p) below close
                                    do (push (%dedent (svref lines r) d) out))
                              (push (%dedent (svref lines close) col) out)
                              (setf p (1+ close))))))
                     (t (return)))))
    (values (nreverse out) p)))

(defun %counter-cookie (text)
  "→ (values number rest) when TEXT starts with a [@N] counter cookie."
  (let ((n (length text)))
    (when (and (> n 3) (char= (char text 0) #\[) (char= (char text 1) #\@))
      (let ((j (position-if-not #'%digit-p text :start 2)))
        (when (and j (> j 2) (char= (char text j) #\]))
          (values (parse-integer text :start 2 :end j)
                  (string-left-trim " " (subseq text (1+ j)))))))))

(defun %description-split (text)
  "→ (values term rest) when TEXT is \"term :: rest\", else NIL."
  (let ((n (length text)))
    (loop for p = (search "::" text) then (search "::" text :start2 (1+ p))
          while p
          when (and (> p 0)
                    (%blank-char-p (char text (1- p)))
                    (or (= (+ p 2) n) (%blank-char-p (char text (+ p 2)))))
            do (let ((term (%trim (subseq text 0 p))))
                 (return (when (plusp (length term))
                           (values term (string-left-trim '(#\Space #\Tab)
                                                          (subseq text (+ p 2))))))))))

(defparameter *term-separator* (format nil " ~C " (code-char #x2014))
  "Between a description item's term and its description: \" — \".")

(defun %prepend-term (term blocks)
  "BLOCKS of a description item with (:strong TERM) at the front of its
first paragraph (a new paragraph when it has none)."
  (let ((strong (list :strong (%parse-inlines term))))
    (if (eq (first (first blocks)) :paragraph)
        (cons (list :paragraph (%merge-inlines (list* strong *term-separator*
                                                      (second (first blocks)))))
              (rest blocks))
        (cons (list :paragraph (list strong)) blocks))))

(defun %parse-list (lines i end)
  (multiple-value-bind (kind indent col0 start delimiter) (%list-item-start (svref lines i))
    (declare (ignore col0))
    (let ((items '())
          (loose nil)
          (j i))
      (loop
        (let* ((line (svref lines j))
               (col (nth-value 2 (%list-item-start line)))
               (text (if (< col (length line)) (subseq line col) ""))
               (term nil))
          (if (eq kind :ordered)
              (multiple-value-bind (n rest) (%counter-cookie text)
                (when n
                  (when (= j i) (setf start n))
                  (setf text rest)))
              (multiple-value-bind (tm rest) (%description-split text)
                (when tm (setf term tm text rest))))
          (multiple-value-bind (more p) (%item-lines lines j end indent col)
            (multiple-value-bind (blocks gap)
                (%parse-container (coerce (cons text more) 'simple-vector) :in-item t)
              (when gap (setf loose t))
              (push (if term (%prepend-term term blocks) blocks) items))
            ;; the next item?
            (let ((q (or (position-if-not #'%blank-p lines :start p :end end) end)))
              (if (and (< q end) (< (- q p) 2) (%same-kind-item-p (svref lines q) kind indent))
                  (progn (when (> q p) (setf loose t))
                         (setf j q))
                  (return (values
                           (list (if (eq kind :bullet)
                                     (list :bullet-list (not loose) (nreverse items))
                                     (list :ordered-list start delimiter (not loose)
                                           (nreverse items))))
                           p))))))))))
