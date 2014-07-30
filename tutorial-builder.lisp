;;;; tutorial-builder.lisp

(in-package #:tutorial-builder)

(ls:in-package/rt #:tutorial-builder)

(defun welcome (name)
  (format t "Welcome to the ~A tutorial.

The format is simple:

    :up/:u/up/u    -- Takes you to the previous section
    :quit/:q/quit/q-- Exits the tutorial.
    <a blank line> -- Advances you to the next section
    #\\# #\\Space    -- Treats the line as a shell command, e.g. `# ls`
    <anything else>-- Runs as a lisp command

So, press enter to coninue!" name))

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
           (let ((string
                  (with-output-to-string (s)
                    (with-protected-markdown-context
                      (3bmd::print-md-element (aref v i) s)))))
             (lines string))))
    (labels ((start (n)
               (case (type n)
                 (:heading
                  #'start)
                 ((:verbatim . #.(when (find-package "3B-MD-CODE-BLOCKS")
                                   `(,(find-symbol
                                       "CODE-BLOCK" "3B-MD-CODE-BLOCKS"))))
                  #'verbatim) 
                 (t
                                        ;(:paragraph :counted-list :bullet-list)
                  #'paragraph)))
             (verbatim (n) (declare (ignore n)) #'end)
             (paragraph (n)
               (case (type n)
                 (:heading #'end)
                 ((:verbatim . #.(when (find-package "3B-MD-CODE-BLOCKS")
                                   `(,(find-symbol
                                       "CODE-BLOCK" "3B-MD-CODE-BLOCKS"))))
                  #'verbatim)
                 (t #'paragraph)))
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
      (loop for idx from index
            for state = (start idx) then (funcall state idx)))))

(defun print-prompt ()
  (format t "~A> " (package-name  *package*)))

(defun interactive-tutorial (l)
  (with-shard-markdown-context 
    (let ((v (make-array (length l) :initial-contents l)))
      (do ((is (list 0) (or is (list 0)))
           (print t))
          () 
        (when print
          (let ((string
                 (with-output-to-string (s)
                   (push (print-next-tutorial-group v (car is) s)
                         is)
                   ;(print (aref v i) s)
                   )))
            (format t "~%==============================~%~%")
            (princ (if (and (> (length string) 0)
                            (member (schar string 0) '(#\Return #\Newline)))
                       (subseq string 1)
                       string))))
        (format t "~%") (print-prompt)
        (let ((line (read-from-string (read-line) () :next)))
          (case line
            ((:next :n next n)
             (setq print t))
            ((:up :u up u)
             (pop is) (pop is)
             (setq print t))
            ((:quit  :q quit q)
             (return-from interactive-tutorial))
            (t (prin1 (eval line)) (terpri) (setq print ()))))))))

(defun run (file &key name)
  (welcome (if name name (pathname-name (pathname file))))
  (read-line)
  (interactive-tutorial (parse-markdown file)))
