;;;; tutorial-builder.asd

(asdf:defsystem #:tutorial-builder
  :serial t
  :description "A library for presenting md files as tutorials."
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on (#:lazy-susan
               #:dishes
               #:uiop/package
               #:alexandria
               #:3bmd
               )
  :components ((:file "package")
               (:file "tutorial-builder")))

