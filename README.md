# Rundown

A small library for running a markdown file as a repl based
interactive tutorial.

We include a short example tutorial showing the use of
`git bisect run` with a lisp project.

Although the tutorials can be arbitrary markdown files, they are
displayed in a way which assumes:

    1. The file is written in short sections.
    2. Code sections are preceded by a clear call to action.
    3. Headings start a section.
    4. Code blocks end a section. 

Like other interactive tutorials, this doles out bite-size chunks over
an interactive prompt, but unlike most of them there is no attempt to
verify that a step has been successfully completed when the user moves
to the next step. I find that such guards typically cause harm.

`run` is the only exported function: given a path to a markdown file
it starts the tutorial.

## Installation

Assuming you have quicklisp installed, clone rundown and its two
non-quicklispable dependencies into local-projects.

    cd ~/quicklisp/local-projects
    git clone git@github.com/m-n/lazy-susan.git       # non-quicklisp dependency
    git clone git@github.com/m-n/dishes.git           # non-quicklisp dependency
    git clone git@github.com/m-n/rundown.git


## Example

To run the example tutorial:

    ;; at the lisp prompt
    (ql:quickload 'rundown)
    (rundown:run #P"~/quicklisp/local-projects/rundown/git-bisect.md")

Feedback is welcome at `https://github.com/m-n/rundown` or
`matt.niemeir@gmail.com`.
