# We lamely force the use of SBCL.  Portability improvements welcome.

quickcheck: metamath
	./metamath demo0.mm

install-sbcl:
	install-sbcl

install-quicklisp:
	sbcl --non-interactive --load install-quicklisp.lisp

metamath: metamath.lisp
	echo "(load \"metamath.lisp\")\n(sb-ext:save-lisp-and-die \"metamath\" :toplevel #'metamath:main :executable t)\n(exit)\n" | sbcl

failing-compile:
	sbcl --noinform --eval "(compile-file \"metamath.lisp\")" --eval "(quit)" > /dev/null

#sbcl --eval "(progn (load \"metamath.lisp\") (sb-ext:save-lisp-and-die \"metamath\" :toplevel #'metamath:main :executable t))"

# We hand-provide a copy of the "quicklisp" package manager, so that
# we're sure to have one.  Here's how to update it.
update-quicklisp:
	wget https://beta.quicklisp.org/quicklisp.lisp

