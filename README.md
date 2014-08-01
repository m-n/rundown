# Tutorial Builder

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

## Example

To run the example tutorial (assuming you have quicklisp installed in
`~/quicklisp`):

    # at the shell prompt
    cd ~/quicklisp/local-projects
    git clone git@github.com/m-n/tutorial-builder.git

    ;; at the lisp prompt
    (ql:quickload 'tutorial-builder)
    (tutorial-builder:run #P"~/quicklisp/local-projects/tutorial-builder/git-bisect.md")

Feedback is welcome at `https://github.com/m-n/tutorial-builder` or
`matt.niemeir@gmail.com`.
