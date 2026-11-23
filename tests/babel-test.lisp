(in-package #:org-markup/tests)

(deftest babel-exports
  (ok (equal '((:code-block "lisp" ("(+ 1 2)")))
             (po "#+begin_src lisp" "(+ 1 2)" "#+end_src" "" "#+RESULTS:" ": 3"))
      "Org's default is code: the block stays, its results go")
  (ok (equal '((:code-block "lisp" ("(+ 1 2)")) (:code-block nil ("3")))
             (po "#+begin_src lisp :exports both" "(+ 1 2)" "#+end_src" "" "#+RESULTS:" ": 3"))
      "both")
  (ok (equal '((:code-block nil ("3")))
             (po "#+begin_src lisp :exports results" "(+ 1 2)" "#+end_src" "#+RESULTS:" ": 3"))
      "results: the block goes, its results stay")
  (ok (equal '((:paragraph ("after")))
             (po "#+name: shorten"
                 "#+BEGIN_SRC emacs-lisp :var result=\"\" :exports none :results silent"
                 "(s-truncate 120 result)"
                 "#+END_SRC"
                 ""
                 "after"))
      "none: a helper block leaves nothing")
  (ok (equal '((:code-block "lisp" ("x")) (:code-block nil ("{" "}")))
             (po "#+begin_src lisp :exports source" "x" "#+end_src" ""
                 "#+RESULTS:" "#+begin_example" "{" "}" "#+end_example"))
      "an unknown value leaves block and results as they are")
  (ok (equal '((:code-block "sh" ("ls")) (:paragraph ("text")))
             (po "#+begin_src sh :exports both" "ls" "#+end_src" "" "#+RESULTS:" "" "text"))
      "a blank line after #+RESULTS: means no results")
  (ok (equal '((:code-block "sh" ("ls")) (:heading 1 ("h")))
             (po "#+begin_src sh" "ls" "#+end_src" "#+RESULTS:" "* h"))
      "a heading is never results")
  (ok (equal '((:code-block "sh" ("ls")) (:paragraph ("p")) (:code-block nil ("1")))
             (po "#+begin_src sh" "ls" "#+end_src" "p" "#+RESULTS:" ": 1"))
      "results belong to the block only when they come right after it")
  (ok (equal '((:code-block "lisp" ("x")) (:table (nil nil) nil ((("1") ("2")))))
             (po "#+begin_src lisp :exports both :results table" "x" "#+end_src"
                 "#+RESULTS[3f2a]:" "| 1 | 2 |"))
      "a table result, a hashed #+RESULTS"))

(deftest babel-exports-properties
  (let ((doc (list "#+PROPERTY: header-args:lisp :session *lisp* :exports both :async"
                   "#+begin_src lisp" "(+ 1 2)" "#+end_src" "#+RESULTS:" ": 3"
                   "#+begin_src python" "1 + 2" "#+end_src" "#+RESULTS:" ": 3"
                   "#+begin_src lisp :exports code" "(+ 1 2)" "#+end_src" "#+RESULTS:" ": 3")))
    (ok (equal '((:code-block "lisp" ("(+ 1 2)")) (:code-block nil ("3"))
                 (:code-block "python" ("1 + 2"))
                 (:code-block "lisp" ("(+ 1 2)")))
               (apply #'po doc))
        "header-args:lisp is the lisp default; python keeps Org's; the block's own wins"))
  (ok (equal '((:code-block "sh" ("ls")) (:code-block nil ("a")))
             (po "#+begin_src sh" "ls" "#+end_src" "#+RESULTS:" ": a"
                 "#+PROPERTY: header-args :exports both"))
      "header-args for every language, wherever the line is")
  (ok (equal '((:code-block "sh" ("ls")))
             (po "#+PROPERTY: header-args :exports both"
                 "#+PROPERTY: header-args :results silent"
                 "#+begin_src sh" "ls" "#+end_src" "#+RESULTS:" ": a"))
      "a later header-args line replaces the value")
  (ok (equal '((:code-block "sh" ("ls")) (:code-block nil ("a")))
             (po "#+PROPERTY: header-args :exports both"
                 "#+PROPERTY: header-args+ :results silent"
                 "#+begin_src sh" "ls" "#+end_src" "#+RESULTS:" ": a"))
      "header-args+ without :exports keeps it")
  (ok (equal '((:code-block nil ("a")))
             (po "#+PROPERTY: header-args :exports both"
                 "#+PROPERTY: header-args+:sh :exports results"
                 "#+begin_src sh" "ls" "#+end_src" "#+RESULTS:" ": a"))
      "header-args+:LANG with :exports overrides for that language")
  (ok (equal '((:code-block "org" ("#+PROPERTY: header-args :exports both"))
               (:code-block "sh" ("ls")))
             (po "#+begin_src org" "#+PROPERTY: header-args :exports both" "#+end_src"
                 "#+begin_src sh" "ls" "#+end_src" "#+RESULTS:" ": a"))
      "a #+PROPERTY line inside a block is content")
  (ok (equal '(("property" . "header-args:lisp :exports both"))
             (kw "#+PROPERTY: header-args:lisp :exports both"))
      "#+PROPERTY is still collected"))

(deftest org-options-parse
  (ok (equal '(("author" . t) ("email" . t) ("toc" . t))
             (org:org-options (kw "#+OPTIONS: author:t email:t toc:t"))))
  (ok (equal '(("toc" . 2) ("num" . nil) ("^" . "{}") (":" . t) ("H" . 3))
             (org:org-options (kw "#+OPTIONS: toc:2 num:nil ^:{} ::t" "#+OPTIONS: H:3")))
      "integers, nil, strings, a colon name, several lines")
  (ok (equal '(("email" . t) ("toc" . nil))
             (org:org-options (kw "#+OPTIONS: toc:t" "#+OPTIONS: email:t toc:nil")))
      "a later word wins")
  (ok (null (org:org-options (kw "#+TITLE: x" "#+OPTIONS: plain :t")))
      "no options; words without a name are skipped")
  (ok (signals (org:org-options "toc:t") 'type-error)))
