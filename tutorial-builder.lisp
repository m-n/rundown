;;;; tutorial-builder.lisp

(in-package #:tutorial-builder)

(ls:in-package/rt #:tutorial-builder)

(defun print-welcome (name)
  (format t "Welcome to the ~A tutorial.

The controls are simple:

    up/u           -- Takes you to the previous section
    quit/q         -- Exits the tutorial.
    <a blank line> -- Advances you to the next section
    repeat/r       -- Repeat this text
    #\\# #\\Space    -- Treats the line as a shell command
    <anything else>-- Runs as a lisp command

The shell syntax runs its command from the directory of the *d-p-d*. To
change directories for the shell syntax, instead of `# cd new/path`
do `(setf *default-pathname-defaults* #P\"new/path\")`.

So, press enter to coninue!~%" name))

;; addresses https://github.com/3b/3bmd/issues/14
(when (fboundp '3bmd::ensure-block)
  (setf (symbol-function '3bmd::ensure-paragraph) #'3bmd::ensure-block
        (symbol-function '3bmd::end-paragraph) #'3bmd::end-block))

(defun parse-markdown (pathname)
  (3bmd-grammar:parse-doc
   (alexandria:read-file-into-string pathname)))

(defmacro with-shard-markdown-context (&body body)
  `(let ((3bmd::*references* (3bmd::extract-refs l))
         (3bmd::*md-indent* 0)
         (3bmd::*md-in-block* nil)
         (3bmd::*md-block-seen-p* nil))
     ,@body))

(defmacro with-fresh-bindings ((&rest vars) &body body)
  `(let ,(mapcar (lambda (v) (list v v)) vars)
     ,@body))

(defmacro with-protected-markdown-context (&body body)
  `(with-fresh-bindings (3bmd::*references* 
                         3bmd::*md-indent*
                         3bmd::*md-in-block*
                         3bmd::*md-block-seen-p*)
     ,@body))

(defun lines (string)
  (loop for i across string
        with lines = 1
        do (case i
             ((#\Newline #\Return) (incf lines))
             (t ()))
        finally (return lines)))

(defun print-next-tutorial-group (v index stream)
  (flet ((type (i) 
           (car (aref v i)))
         (output-line-count (i)
           (if (< i (length v))
               (let ((string
                      (with-output-to-string (s)
                        (with-protected-markdown-context
                          (3bmd::print-md-element (aref v i) s)))))
                 (lines string))
               0)))
    (labels ((start (n)
               (if (< n (length v))
                   (case (type n)
                     (:heading
                      #'start)
                     ((:verbatim . #.(when (find-package "3BMD-CODE-BLOCKS")
                                       `(,(find-symbol
                                           "CODE-BLOCK" "3BMD-CODE-BLOCKS"))))
                      #'verbatim) 
                     (t
                                        ;(:paragraph :counted-list :bullet-list)
                      #'paragraph))
                   #'end))
             (verbatim (n) (declare (ignore n)) #'end)
             (paragraph (n)
               (if (< n (length v))
                   (case (type n)
                     (:heading #'end)
                     ((:verbatim . #.(when (find-package "3BMD-CODE-BLOCKS")
                                       `(,(find-symbol
                                           "CODE-BLOCK" "3BMD-CODE-BLOCKS"))))
                      #'verbatim)
                     (t #'paragraph))
                   #'end))
             (end (n)
               (do* ((j index (+ 1 j))
                     (lc (output-line-count j)
                         (+ lc (output-line-count j))))
                    ((and (> j index)
                          (or (> j  (- n 2))
                              (> lc 50)))
                     (return-from print-next-tutorial-group
                       (1- n)))
                 (3bmd::print-md-element (aref v j) stream))))
      (if (< index (length v))
          (loop for idx from index
                for state = (start idx) then (funcall state idx))
          (progn (format stream "Thank you. I hope you enjoyed the tutorial.~%")
                 index)))))

(defun print-prompt ()
  (format t "~A> " (package-name  *package*)))

(defun tutorial-read ()
  (let ((*readtable* (ls:package-rt 'tutorial-builder)))
    ;; We don't use `read` because it hangs until it creates an
    ;; object, but we want a bare return to advance the tutorial.  We
    ;; don't use read-line because we want multi-line input.
    (read-from-string
     (with-output-to-string (s)
       (peek-char)                      ; hang for any input
       (do ((c (read-char-no-hang)
               (read-char-no-hang)))
           ((not c))                    ; grab all the input
         (write-char c s)))
     ()
     :next)))

(defun get-input ()
  (terpri)
  (print-prompt)
  (handler-case (tutorial-read)
    (serious-condition (c)
      (format t "Error reading form. ~&~A~%" c)
      (get-input))))

(defun eval-print (form)
  (handler-case (prin1 (eval form))
    (serious-condition (c)
      (format t "Evaluation aborted. ~&~A." c)))
  (terpri))

(defun interactive-tutorial (l)
  (with-shard-markdown-context 
    (let ((v (make-array (length l) :initial-contents l)))
      (do ((is (list 0) (or is (list 0)))
           (print t))
          (())
        (when print
          (let ((string
                 (with-output-to-string (s)
                   (push (print-next-tutorial-group v (car is) s)
                         is))))
            (format t "~%==============================~%~%")
            (princ (if (and (> (length string) 0)
                            (member (schar string 0) '(#\Return #\Newline)))
                       (subseq string 1)
                       string))))
        (let ((form (get-input)))
          (case* (form :test #'symbol-name-equal)
            ((next n)
             (setq print t))
            ((up u)
             (pop is) (pop is)
             (setq print t))
            ((quit q)
             (return-from interactive-tutorial))
            ((repeat r)
             (pop is)
             (setq print t))
            (t (eval-print form)
               (setq print ()))))))))

(defun run (file &key name)
  "Start the tutorial using the given pathname as the source file."
  (tagbody
   welcome
     (print-welcome (if name name (pathname-name (pathname file))))
   prompt
     (let ((form (get-input)))
       (case* (form :test #'symbol-name-equal)
         ((up u repeat r)
          (go welcome))
         ((next n)
          ())
         ((quit q)
          (return-from run))
         (t
          (eval-print form)
          (go prompt)))))
  (interactive-tutorial (parse-markdown file)))
