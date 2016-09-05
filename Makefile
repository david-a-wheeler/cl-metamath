

# We lamely force the use of SBCL.  Portability improvements welcome.

install-quicklisp:
	sbcl --non-interactive --load install-quicklisp.lisp

# We hand-provide a copy of the "quicklisp" package manager, so that
# we're sure to have one.  Here's how to update it.
update-quicklisp:
	wget https://beta.quicklisp.org/quicklisp.lisp

