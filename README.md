

* Pick a Common Lisp implementation, e.g., sbcl
* Install the QuickLisp package manager (if it isn't already).

~~~~
wget https://beta.quicklisp.org/quicklisp.lisp # Get Quicklisp installer
sbcl # or whatever Lisp implementation
(load "quicklisp.lisp")
(quicklisp-quickstart:install)  ; Install QuickLisp (also installs ASDF)
(ql:add-to-init-file)           ; Add QuickLisp to your Lisp init file
~~~~

Running the program will automatically download and install its
depedencies.  You can force download ahead-of-time:

~~~~
(ql:quickload "readable")
~~~~
