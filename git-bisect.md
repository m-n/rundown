# Automating `git bisect` on a lisp project

## Manual `git bisect`

`git bisect` is a tool for determining which commit to a git
repository introduced a change -- typically a bug. In its simplest
form, you run `git bisect start badcommit goodcommit`; git checks out
a revision of the software; you check whether the revision contains
the bug, and you mark the revision as good or bad with `git bisect
good` or `git bisect bad`. Then you have it checkout the next revision
with `git bisect next`. Each step bisect picks a commit roughly
halfway between the last good and bad revisions, so the number of
steps required scales with the log of the number of commits between
the good and bad revision.

## `git bisect run`

Manual `git bisect` is okay, but it is obviously not ideal. It can be
downright painful if determining whether the defect is present is
tedious or requires a slow step, such as a long build. Git provides a
solution to that via `git bisect run` which runs a program at every
bisected commit, and marks the commit good if the program returns 0 or
bad if it returns 1-124, 126, or 127. A return value of 125 indicates
that the program can't determine whether the commit is good or bad.

As an example, we'll find the commit which introduced a regression in
my lazy-susan library. lazy-susan provides some readtable
customizations, and recently I realized that it no longer works with
input such as "`(1 .,1)".

## Getting lazy-susan

We assume that you have SBCL installed, and that you have Quicklisp
installed and that your .sbclrc loads it. Clone lazy-susan into
~/quicklisp/local-projects, if you haven't already done so:

    cd ~/quicklisp/local-projects
    git clone https://github.com/m-n/lazy-susan
    cd lazy-susan/

Now that we have lazy-susan, we can create a bisect.lisp script in its
directory which will return 1 if the current checkout exhibits the
defect, 0 if it doesn't, and 125 if we can't determine that.

    #!/usr/bin/sbcl --script
    ;; inspired by tkych's Travis CI post
    ;; http://qiita.com/tkych/items/08d6549be8795751a887

    (load "~/.sbclrc")

    (let ((*standard-output* (make-broadcast-stream)))
      (handler-case (asdf:load-system 'lazy-susan :force t)
        (serious-condition () (sb-ext:quit :unix-status 125))))

    (let ((*readtable* (ls:rt)))
      (sb-ext:quit :unix-status
                   (handler-case (progn (read-from-string "`(1 .,1)") 0)
                     (serious-condition () 1))))

Now `chmod +x bisect.lisp` so that git can execute it, then we can
finally do the automatic bisect:

    git bisect start d169e99 697d95   # d169e99 could typically be a branch name
    git bisect run ./bisect.lisp

That should have run for several seconds, and then told you that the
first bad commit was 4392d6, in which I introduced my own list reader.

Now you can check the log of the bisect, and then finish the bisect.

    git bisect log
    git bisect reset # finish bisecting, reset HEAD to master
