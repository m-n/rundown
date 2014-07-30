;;;; package.lisp

(defpackage #:tutorial-builder
  (:use #:cl)
  (:export
   #:run
   ))

(in-package #:tutorial-builder)

(ls:setup-package-rt (tutorial-builder)
  (#\# #\Space) #'dishes:repl-run-program-reader
  (#\# #\;) #'dishes:comment-line-suppress-forms
  :digit-separators '(#\_))
