(in-package #:org-markup)

;;; ------------------------------------------------------------------
;;; Inline parser (the INLINES rules in org.lisp's header).
;;;
;;; One left-to-right scan. At each position the first object that parses
;;; wins: a line break, a bracket link, an angle link, a plain link, then
;;; an emphasis / code marker. An emphasis marker takes the first valid
;;; closing marker after it (org's non-greedy regexp), and its contents
;;; are parsed again with the same rules. Anything else is text.

(defconstant +max-depth+ 64
  "Deepest container or emphasis nesting that is parsed; deeper content
stays text, so no input exhausts the stack.")

(defvar *inline-depth* 0
  "Emphasis / link-description nesting depth of the current %PARSE-INLINES.")

(declaim (inline %ws-p %blank-char-p))
(defun %ws-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun %blank-char-p (ch)
  (member ch '(#\Space #\Tab)))

(defun %pre-p (ch)
  "May CH come right before an opening emphasis marker?"
  (or (%ws-p ch) (find ch "-({'\"")))

(defun %post-p (ch)
  "May CH come right after a closing emphasis marker?"
  (or (%ws-p ch) (find ch "-.,;:!?')}[\"\\")))

(defun %skip-blanks (s i n)
  (loop while (and (< i n) (%blank-char-p (char s i))) do (incf i))
  i)

(defun %prefix-at-p (prefix s i n)
  (let ((e (+ i (length prefix))))
    (and (<= e n) (string= prefix s :start2 i :end2 e))))

(defun %merge-inlines (items)
  "ITEMS (strings and nodes) with adjacent strings merged and empty strings
dropped."
  (let ((out '()))
    (dolist (x items (nreverse out))
      (cond ((and (stringp x) (zerop (length x))))
            ((and (stringp x) (stringp (first out)))
             (setf (first out) (concatenate 'string (first out) x)))
            (t (push x out))))))

;;; Per-scan lookup tables, so that searching ahead from many positions of
;;; one string stays linear.

(defstruct (scan-cache (:constructor make-scan-cache ()))
  (newlines nil)          ; %NEWLINE-COUNTS, made on demand
  (closers '())           ; alist marker → %CLOSER-TABLE
  (brackets-from -1)      ; the last "]]" search started here …
  (brackets-at nil))      ; … and found this index (or NIL: none after it)

(defun %next-close-brackets (s j n cache)
  "Index of the first \"]]\" at or after J, or NIL."
  (let ((from (scan-cache-brackets-from cache))
        (at (scan-cache-brackets-at cache)))
    (if (and (>= from 0) (>= j from) (or (null at) (<= j at)))
        at
        (let ((p (search "]]" s :start2 j :end2 n)))
          (setf (scan-cache-brackets-from cache) j
                (scan-cache-brackets-at cache) p)
          p))))

;;; Emphasis

(defun %closer-table (s n m)
  "→ V, N+1 fixnums: V[K] is the first index at or after K of a marker M that
may close (after a non-blank; before the end, a blank or a post char), else
N."
  (let ((v (make-array (1+ n) :element-type 'fixnum :initial-element n)))
    (loop for k from (1- n) downto 0
          do (setf (aref v k)
                   (if (and (char= (char s k) m)
                            (> k 0)
                            (not (%ws-p (char s (1- k))))
                            (or (= (1+ k) n) (%post-p (char s (1+ k)))))
                       k
                       (aref v (1+ k)))))
    v))

(defun %newline-counts (s n)
  "→ V, N+1 fixnums: V[K] is the number of newlines in S[0, K)."
  (let ((v (make-array (1+ n) :element-type 'fixnum :initial-element 0)))
    (loop for k from 0 below n
          do (setf (aref v (1+ k))
                   (+ (aref v k) (if (char= (char s k) #\Newline) 1 0))))
    v))

(defun %emphasis-close (s i n cache)
  "S[I] is an emphasis marker. → the index of its closing marker when it
opens one, else NIL: the first marker that may close at I+2 or later, with
at most one newline in between. CACHE is the SCAN-CACHE of S."
  (let ((m (char s i)))
    (when (and (or (= i 0) (%pre-p (char s (1- i))))
               (<= (+ i 2) n)
               (not (%ws-p (char s (1+ i)))))
      (let* ((table (or (cdr (assoc m (scan-cache-closers cache)))
                        (let ((v (%closer-table s n m)))
                          (push (cons m v) (scan-cache-closers cache))
                          v)))
             (k (aref table (+ i 2))))
        (when (< k n)
          (let ((lines (or (scan-cache-newlines cache)
                           (setf (scan-cache-newlines cache) (%newline-counts s n)))))
            (when (<= (- (aref lines k) (aref lines (1+ i))) 1)
              k)))))))

;;; Links

(defparameter *plain-link-schemes* '("https://" "http://" "mailto:"))

(defparameter *image-extensions* '(".png" ".jpg" ".jpeg" ".gif" ".svg" ".webp"))

(defun %strip-file-prefix (target)
  (if (%prefix-at-p "file:" target 0 (length target))
      (subseq target 5)
      target))

(defun %image-target-p (target)
  "Does TARGET (a file: prefix already stripped) name an image?"
  (let ((n (length target)))
    (some (lambda (ext)
            (let ((k (length ext)))
              (and (> n k) (string-equal ext target :start2 (- n k)))))
          *image-extensions*)))

(defun %image-description-p (desc)
  "A description that is itself a file: or http(s) link to an image
(org's [[page][file:thumb.png]] idiom)."
  (and (plusp (length desc))
       (notany #'%ws-p desc)
       (or (%prefix-at-p "file:" desc 0 (length desc))
           (%prefix-at-p "https://" desc 0 (length desc))
           (%prefix-at-p "http://" desc 0 (length desc)))
       (%image-target-p (%strip-file-prefix desc))))

(defun %unescape-brackets (s &optional join-lines)
  "S with \\[ \\] \\\\ → [ ] \\; with JOIN-LINES also each newline and the
blanks after it → one space."
  (with-output-to-string (out)
    (let ((n (length s))
          (i 0))
      (loop while (< i n)
            do (let ((ch (char s i)))
                 (cond ((and (char= ch #\\) (< (1+ i) n) (find (char s (1+ i)) "[]\\"))
                        (write-char (char s (1+ i)) out)
                        (incf i 2))
                       ((and join-lines (char= ch #\Newline))
                        (write-char #\Space out)
                        (setf i (%skip-blanks s (1+ i) n)))
                       (t (write-char ch out) (incf i))))))))

(defun %bracket-target-end (s j n)
  "Index of the \"]\" ending a link target that starts at J, or NIL."
  (loop while (< j n)
        do (let ((ch (char s j)))
             (cond ((and (char= ch #\\) (< (1+ j) n) (find (char s (1+ j)) "[]\\"))
                    (incf j 2))
                   ((char= ch #\[) (return nil))
                   ((char= ch #\]) (return j))
                   (t (incf j))))))

(defun %bracket-description-end (s j n cache)
  "Index of the \"]]\" ending a link description that starts at J: the first
one outside balanced brackets, else the first one; NIL when there is none."
  (let ((first (%next-close-brackets s j n cache))
        (depth 0)
        (p j))
    (unless first
      (return-from %bracket-description-end nil))
    (loop while (< p n)
          do (let ((ch (char s p)))
               (cond ((and (char= ch #\\) (< (1+ p) n) (find (char s (1+ p)) "[]\\"))
                      (incf p 2))
                     ((char= ch #\[) (incf depth) (incf p))
                     ((char= ch #\])
                      (cond ((plusp depth) (decf depth) (incf p))
                            ((and (< (1+ p) n) (char= (char s (1+ p)) #\]))
                             (return-from %bracket-description-end p))
                            (t (incf p))))
                     (t (incf p)))))
    first))

(defun %parse-bracket-link (s i n links cache)
  "S[I] is \"[\". → (values node end) for [[target]] or [[target][desc]], END
after the closing \"]]\"; NIL when there is none. LINKS NIL (inside a link
description) accepts image links only. CACHE is the SCAN-CACHE of S."
  (unless (and (< (1+ i) n) (char= (char s (1+ i)) #\[))
    (return-from %parse-bracket-link nil))
  (let* ((tstart (+ i 2))
         (tend (%bracket-target-end s tstart n)))
    (unless (and tend (< (1+ tend) n) (notevery #'%ws-p (subseq s tstart tend)))
      (return-from %parse-bracket-link nil))
    (let* ((target (%strip-file-prefix (string-trim '(#\Space #\Tab)
                                                    (%unescape-brackets
                                                     (subseq s tstart tend) t))))
           (after (char s (1+ tend)))
           desc end)
      (cond ((char= after #\]) (setf end (+ tend 2)))
            ((char= after #\[)
             (let ((dend (%bracket-description-end s (+ tend 2) n cache)))
               (unless dend (return-from %parse-bracket-link nil))
               (setf desc (%unescape-brackets (subseq s (+ tend 2) dend))
                     end (+ dend 2))))
            (t (return-from %parse-bracket-link nil)))
      (let ((node
              (cond ((and desc (notevery #'%ws-p desc))
                     (let ((d (string-trim '(#\Space #\Tab) desc)))
                       (list :link target
                             (if (%image-description-p d)
                                 (list (list :image (%strip-file-prefix d) '()))
                                 (%nested-inlines desc nil)))))
                    ((%image-target-p target) (list :image target '()))
                    (t (list :link target (if (plusp (length target)) (list target) '()))))))
        (when (or links (eq (first node) :image))
          (values node end))))))

(defun %parse-angle-link (s i n)
  "S[I] is \"<\". → (values node end) for <https://…>, <http://…> or
<mailto:…>, else NIL."
  (let ((scheme (find-if (lambda (p) (%prefix-at-p p s (1+ i) n)) *plain-link-schemes*)))
    (when scheme
      (let* ((start (1+ i))
             (close (position-if (lambda (ch) (member ch '(#\< #\> #\Newline)))
                                 s :start (+ start (length scheme)))))
        (when (and close (char= (char s close) #\>)
                   (> close (+ start (length scheme))))
          (let ((url (subseq s start close)))
            (values (list :link url (list url)) (1+ close))))))))

(defun %parse-plain-link (s i n)
  "→ (values node end) when a plain https://, http:// or mailto: link
starts at S[I] (not after a letter or digit), else NIL."
  (when (or (= i 0) (not (alphanumericp (char s (1- i)))))
    (let ((scheme (find-if (lambda (p) (%prefix-at-p p s i n)) *plain-link-schemes*)))
      (when scheme
        (let* ((body (+ i (length scheme)))
               (j (or (position-if (lambda (ch) (or (%ws-p ch) (find ch "<>[]()\"")))
                                   s :start body)
                      n)))
          (loop while (and (> j body) (find (char s (1- j)) ".,;:!?'")) do (decf j))
          (when (> j body)
            (let ((url (subseq s i j)))
              (values (list :link url (list url)) j))))))))

;;; The scan

(defun %nested-inlines (s links)
  "Inlines of S one nesting level down; at +MAX-DEPTH+ S stays text."
  (if (>= *inline-depth* +max-depth+)
      (list (substitute #\Space #\Newline s))
      (let ((*inline-depth* (1+ *inline-depth*)))
        (%parse-inlines s links))))

(defun %line-break-end (s i n)
  "S[I] is a backslash. → the index after the blanks following \"\\\\\" when
the two backslashes end a line (and the one before I is not a backslash),
else NIL."
  (when (and (< (1+ i) n)
             (char= (char s (1+ i)) #\\)
             (not (and (> i 0) (char= (char s (1- i)) #\\))))
    (let ((k (%skip-blanks s (+ i 2) n)))
      (when (or (= k n) (char= (char s k) #\Newline))
        k))))

(defun %parse-inlines (s &optional (links t))
  "STRING → a list of inlines (see PARSE-ORG). LINKS NIL (inside a link
description): no links, only image links."
  (let* ((n (length s))
         (out '())
         (text (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
         (i 0)
         (cache (make-scan-cache)))
    (labels ((flush ()
               (when (plusp (fill-pointer text))
                 (push (coerce text 'simple-string) out)
                 (setf (fill-pointer text) 0)))
             (emit (x) (flush) (push x out))
             (add-char (ch) (vector-push-extend ch text))
             (trim-trailing ()
               (loop while (and (plusp (fill-pointer text))
                                (%blank-char-p (char text (1- (fill-pointer text)))))
                     do (decf (fill-pointer text)))))
      (loop while (< i n)
            do (let ((ch (char s i))
                     node end)
                 (cond
                   ;; \\ at the end of a line
                   ((and (char= ch #\\) (setf end (%line-break-end s i n)))
                    (trim-trailing)
                    (if (= end n)
                        (setf i n)
                        (progn (emit (list :hard-break))
                               (setf i (%skip-blanks s (1+ end) n)))))
                   ((char= ch #\Newline)
                    (trim-trailing)
                    (emit (list :soft-break))
                    (setf i (%skip-blanks s (1+ i) n)))
                   ;; links
                   ((and (char= ch #\[)
                         (setf (values node end) (%parse-bracket-link s i n links cache)))
                    (emit node) (setf i end))
                   ((and links (char= ch #\<)
                         (setf (values node end) (%parse-angle-link s i n)))
                    (emit node) (setf i end))
                   ((and links (or (char= ch #\h) (char= ch #\m))
                         (setf (values node end) (%parse-plain-link s i n)))
                    (emit node) (setf i end))
                   ;; emphasis and code
                   ((and (find ch "*/_+=~") (setf end (%emphasis-close s i n cache)))
                    (let ((contents (subseq s (1+ i) end)))
                      (emit (if (find ch "=~")
                                (list :code (substitute #\Space #\Newline contents))
                                (list (ecase ch
                                        (#\* :strong)
                                        ((#\/ #\_) :emph)
                                        (#\+ :strike))
                                      (%nested-inlines contents links)))))
                    (setf i (1+ end)))
                   (t (add-char ch) (incf i)))))
      (flush)
      (%merge-inlines (nreverse out)))))
