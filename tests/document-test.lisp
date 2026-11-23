(in-package #:org-markup/tests)

(defun crlf (s)
  "S with every LF as CRLF."
  (with-output-to-string (out)
    (loop for ch across s
          do (when (char= ch #\Newline) (write-char #\Return out))
             (write-char ch out))))

(defparameter *blog-post*
  (nl "#+TITLE: Parsing Org in Common Lisp"
      "#+DATE: <2026-11-23 Mon>"
      "#+FILETAGS: :lisp:org:"
      "#+OPTIONS: toc:nil"
      ""
      "* Introduction"
      ":PROPERTIES:"
      ":CUSTOM_ID: intro"
      ":END:"
      "This post shows *org-markup*, a parser for /Org/ files."
      "See [[https://orgmode.org][the Org manual]] for the syntax."
      ""
      "#+CAPTION: The parser at work"
      "#+ATTR_HTML: :width 600"
      "[[file:images/parser.png]]"
      ""
      "* TODO Usage :howto:"
      "** Loading"
      "#+begin_src lisp"
      "(asdf:load-system \"org-markup\")"
      "(stack-org:parse-org \"* Hi\")"
      "#+end_src"
      ""
      "Steps:"
      "1. Install with =cl-repo=."
      "2. Call ~parse-org~:"
      "   - it returns blocks"
      "   - and keywords"
      ""
      "| Element | Node       |"
      "|---------+------------|"
      "| <l>     | <c>        |"
      "| heading | =:heading= |"
      "| table   | =:table=   |"
      ""
      "- Blocks :: headings, lists, tables"
      "- Inlines :: links and _emphasis_"
      ""
      "* Notes :noexport:"
      "Private draft notes."
      "** More notes"
      "# a comment"
      ""
      "* Conclusion"
      "#+begin_quote"
      "Simplicity is prerequisite for reliability."
      "#+end_quote"
      "That's all -- see https://example.com/blog."
      "-----"))

(deftest document-blog-post
  (multiple-value-bind (blocks keywords) (org:parse-org *blog-post*)
    (ok (equal '(("title" . "Parsing Org in Common Lisp")
                 ("date" . "<2026-11-23 Mon>")
                 ("filetags" . ":lisp:org:")
                 ("options" . "toc:nil"))
               keywords)
        "keywords; caption and attr_html dropped")
    (ok (equal '((:heading 1 ("Introduction"))
                 (:paragraph ("This post shows " (:strong ("org-markup")) ", a parser for "
                              (:emph ("Org")) " files." (:soft-break)
                              "See " (:link "https://orgmode.org" ("the Org manual"))
                              " for the syntax."))
                 (:paragraph ((:image "images/parser.png" ())))
                 (:heading 1 ("Usage"))
                 (:heading 2 ("Loading"))
                 (:code-block "lisp" ("(asdf:load-system \"org-markup\")"
                                      "(stack-org:parse-org \"* Hi\")"))
                 (:paragraph ("Steps:"))
                 (:ordered-list 1 #\. t
                  (((:paragraph ("Install with " (:code "cl-repo") ".")))
                   ((:paragraph ("Call " (:code "parse-org") ":"))
                    (:bullet-list t (((:paragraph ("it returns blocks")))
                                     ((:paragraph ("and keywords"))))))))
                 (:table (:left :center)
                  (("Element") ("Node"))
                  ((("heading") ((:code ":heading")))
                   (("table") ((:code ":table")))))
                 (:bullet-list t
                  (((:paragraph ((:strong ("Blocks")) " — headings, lists, tables")))
                   ((:paragraph ((:strong ("Inlines")) " — links and " (:emph ("emphasis")))))))
                 (:heading 1 ("Conclusion"))
                 (:block-quote ((:paragraph ("Simplicity is prerequisite for reliability."))))
                 (:paragraph ("That's all -- see "
                              (:link "https://example.com/blog" ("https://example.com/blog"))
                              "."))
                 (:thematic-break))
               blocks)
        "the full AST")
    (ok (equal blocks (org:parse-org (crlf *blog-post*)))
        "CRLF line ends give the same blocks")))

(deftest document-values
  (ok (= 2 (length (multiple-value-list (org:parse-org "x")))) "two values")
  (let ((s (make-array 3 :element-type 'character :adjustable t :fill-pointer 3
                         :initial-contents "*a*")))
    (ok (equal '((:paragraph ((:strong ("a"))))) (org:parse-org s)) "a non-simple string")))

(deftest document-never-signals
  (dolist (s (list "[[" "[[a" "[[a]" "[[a][" "[[a][b" "[[a][b]" "*" "**" "* " "=" "==" "~~~"
                   "#+" "#+BEGIN_" "#+BEGIN_SRC" "#+begin_quote" "#+END_" "#+BEGIN:" ":" "::" ":a:"
                   ":END:" "|" "|-" "|+" "- " "1." "1)" "- a ::" "- :: x" "1. [@" "1. [@x]" "1. [@1"
                   "<" "<https://" "https://" "mailto:" "\\" "\\\\" "a\\\\" "-----" "* TODO [#"
                   "* :" "* a :" "#+begin_verse" (format nil "~C" #\Tab) (format nil "~C" #\Return)
                   (format nil "- a~%~%~%") (format nil "#+begin_src~%,") (string (code-char 0))
                   (make-string 200 :initial-element #\[)
                   (make-string 200 :initial-element #\*)
                   (format nil "~{~A~}" (loop repeat 100 collect "/*_+=~"))
                   (format nil "~{~A~%~}" (loop repeat 200 collect "#+begin_quote"))
                   (format nil "~{~A~%~}" (loop for k below 200
                                                collect (concatenate 'string (make-string k :initial-element #\Space)
                                                                     "- x")))))
    (ok (listp (org:parse-org s)) (format nil "parses ~S" (subseq s 0 (min 20 (length s)))))))

(deftest document-nesting-limit
  (let ((blocks (org:parse-org (format nil "~{~A~%~}x~%" (loop repeat 70 collect "#+begin_quote"))))
        (depth 0))
    (loop while (and (= 1 (length blocks)) (eq :block-quote (first (first blocks))))
          do (incf depth)
             (setf blocks (second (first blocks))))
    (ok (= 65 depth) "65 quote nodes: the 65th holds content 65 containers deep")
    (ok (equal (list (list :paragraph
                           (append (loop repeat 5 append (list "#+begin_quote" '(:soft-break)))
                                   (list "x"))))
               blocks)
        "which becomes one paragraph of its text")))
