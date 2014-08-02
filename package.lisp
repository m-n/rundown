;;;; package.lisp

(defpackage #:rundown
  (:use #:cl)
  (:export
   #:run
   ))

(in-package #:rundown)

(ls:setup-package-rt (rundown)
  (#\# #\Space) #'dishes:repl-run-program-reader
  (#\# #\;) #'dishes:comment-line-suppress-forms
  :digit-separators '(#\_))
