;;;; utils.lisp

(in-package #:tutorial-builder)
(ls:in-package/rt #:tutorial-builder)

(defmacro case* ((obj &key (test #'eql)) &body clauses)
  "Like case but accepts :test parameter."
  ;; not alexandria:switch for 3 reasons
  ;;   switch evaluates keyforms
  ;;   switch treats caseforms as a single element not list of alternatives
  ;;   switch only takes a named function for a test
  (alexandria:with-gensyms (gtest gobj)
    (flet ((generate-cond-clause (clause ctest cobj)
             (destructuring-bind (cases &body body) clause
               (if (member cases '(t otherwise))
                   `(t ,@body)
                   `((member ,cobj ',(alexandria:ensure-list cases) :test ,ctest)
                     ,@body)))))
      `(let ((,gobj ,obj)
             (,gtest ,test))
         (cond ,@(mapcar (alexandria:rcurry #'generate-cond-clause gtest gobj)
                         clauses))))))

(defun symbol-name-equal (s1 s2)
  (and (symbolp s1) (symbolp s2) (string-equal s1 s2)))
