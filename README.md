# metamath-lisp

This is the start of a metamath package/verifier in Common Lisp.
It is open source software, released under the MIT license.

This is a very early version and does NOT yet implement verification.
The current focus has been on experimentation to determine
how to efficiently handle the basics.

This implementation is intended to be high-speed.
It can currently tokenize set.mm (~29MB) into intern'ed symbols
in about 4 seconds on a laptop, and eventually it should be faster.
Good Common Lisp implementations are historically fast if written well.

The code uses sweet-expressions so that the code is much easier to read
for most developers compared to traditional Common Lisp notation.
Take a look; you might be surprised how clear it is.

The scripts depend on sbcl, but the code should generally run on any
Common Lisp implementation.  Help to make this more portable would be
greatly appreciated.

# Installation

You need a Common Lisp; currently we focus on sbcl.
You can run "sbcl-install" to download and install it.
You may need to specially handle SBCL_HOME and PATH.

You *cannot* use old sbcl versions, like the sbcl on Ubuntu's 2014 edition;
such old versions of sbcl can't support current versions of various libraries.

You need to install a relatively current version of QuickLisp.
You can do this with:

~~~~
make install-quicklisp
~~~~

It doesn't currently have an "install someplace" option.

# Running

You can run the program with "run".  It requires a single command line
parameter, the name of the .mm file to check:

~~~~
./run demo0.mm
./run set.mm
~~~~


It directly loads the file into memory instead of trying to use
Lisp's read-char and peek-char (which work much more slowly).
Programs can be extremely fast in Common Lisp, but it sometimes takes
a few tricks (e.g., declaring types and avoiding system calls).
