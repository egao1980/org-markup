(defpackage #:org-markup/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:org #:org-markup)))
(in-package #:org-markup/tests)

(defun nl (&rest lines)
  "LINES joined with #\\Newline, each followed by one."
  (format nil "~{~A~%~}" lines))

(defun po (&rest lines)
  "The blocks of LINES."
  (values (org:parse-org (apply #'nl lines))))

(defun kw (&rest lines)
  "The keywords of LINES."
  (nth-value 1 (org:parse-org (apply #'nl lines))))
