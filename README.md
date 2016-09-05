
# meatmath-lisp

This is the start of a metamath verifier in Common Lisp,

This is very early and does NOT really implement verification.
At this time it's a study in how to efficiently parse the file in Common Lisp.

It uses sweet-expressions so that the code is much easier to read
for most developers compared to traditional Common Lisp notation.

The scripts depend on sbcl, but the code should run on any
Common Lisp implementation.

You need to install a relatively current version of QuickLisp.
You can do this with:

~~~~
make install-quicklisp
~~~~

You can then run the program with "run"; standard input should be
a valid metmath file:

~~~~
(ql:quickload "readable")
~~~~
