(defsystem "org-markup"
  :version "0.1.1"
  :description "Org-mode subset parser producing the markdown-protocol plain-list block / inline AST, plus #+KEYWORD metadata"
  :author "egao1980"
  :license "MIT"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "inline")
               (:file "block")
               (:file "org"))
  :in-order-to ((test-op (test-op "org-markup/tests"))))

(defsystem "org-markup/tests"
  :depends-on ("org-markup" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "block-test")
               (:file "inline-test")
               (:file "keyword-test")
               (:file "babel-test")
               (:file "document-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
