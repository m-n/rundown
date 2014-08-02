;;;; rundown.asd

(asdf:defsystem #:rundown
  :serial t
  :description "A library for running markdown files as tutorials."
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on (#:lazy-susan
               #:dishes
               #:uiop
               #:alexandria
               #:3bmd
               )
  :components ((:file "package")
               (:file "utils")
               (:file "rundown")))

