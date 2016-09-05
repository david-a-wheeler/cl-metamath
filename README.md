
# meatmath-lisp

This is the start of a metamath verifier in Common Lisp,
It is open source software, released under the MIT license.

This is a very early version and does NOT yet implement verification.
Currently this focuses on how to efficiently parse the file in Common Lisp.
It's currently about 23 seconds (Ubuntu, laptop) to tokenize set.mm;
this is faster than some implementations (e.g., Python) but it's
not as fast as C or Rust yet.

The code uses sweet-expressions so that the code is much easier to read
for most developers compared to traditional Common Lisp notation.

The scripts depend on sbcl, but the code should run on any
Common Lisp implementation.

# Installation

You need to install a relatively current version of QuickLisp.
You can do this with:

~~~~
make install-quicklisp
~~~~

# Running

You can then run the program with "run"; standard input should be
a valid metmath file:

~~~~
./run < set.mm
~~~~
