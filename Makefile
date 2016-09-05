

# We lamely force the use of SBCL.  Portability improvements welcome.

install-quicklisp:
	sbcl --non-interactive --load install-quicklisp.lisp

update-quicklisp:
	wget https://beta.quicklisp.org/quicklisp.lisp

