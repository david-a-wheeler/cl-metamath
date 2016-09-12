;;;; Implement metamath-related operations
;;;; Copyright 2016, David A. Wheeler and the cl-metamath contributors
;;;; SPDX-License-Identifier: MIT

;;; This is designed to be high performance. We want the code to be clear,
;;; but some decisions make a big difference to performance.
;;; This is in many ways inspired by checkmm.cpp (which is CC0 licensed).

(cl:in-package #:cl-metamath)

; Switch to sweet-expressions - from here on the code is more readable.
(readable:enable-sweet)

declaim $ optimize speed

defvar *author* "David A. Wheeler <dwheeler@dwheeler.com>"
defvar *license* "MIT"

; TODO: where's the portable library for this?
;(defun my-command-line ()
;  (or
;   #+SBCL *posix-argv*
;   #+LISPWORKS system:*line-arguments-list*
;   #+CMU extensions:*command-line-words*
;   nil))

defun my-command-line ()
  '("demo0.mm")

; TODO: Find the routine to do this less hackishly; it's not *package*.
defparameter *self-package* find-package('cl-metamath)

; Extra code to *quickly* read files.

defvar mmbuffer
defvar mmbuffer-position 0
defvar mmbuffer-length 0
defun load-mmfile (filename)
  ; format t "DEBUG: Starting load-mmfile~%"
  ; (declare (optimize (speed 3) (debug 0) (safety 0)))
  let* ((buffer-element-type '(unsigned-byte 8)))
    setf mmbuffer make-array(100000000 :element-type buffer-element-type)
    with-open-file (in filename :element-type '(unsigned-byte 8))
      setf mmbuffer-length read-sequence(mmbuffer in)
  nil

; Optimized version of read-char()
defun my-read-char ()
  declare $ optimize speed(3) safety(0)
  declare $ type fixnum mmbuffer-position mmbuffer-length
  declare simple-array(unsigned-byte(8))(mmbuffer)
  if {mmbuffer-position >= mmbuffer-length}
    nil
    let ((result aref(mmbuffer mmbuffer-position)))
      declare $ type (unsigned-byte 8) result
      setf mmbuffer-position $ the fixnum $ 1+ mmbuffer-position
      code-char result

; Optimized version of peek-char(nil nil nil nil)
defun my-peek-char ()
  declare $ optimize speed(3) safety(0)
  declare $ type fixnum mmbuffer-position mmbuffer-length
  declare simple-array(unsigned-byte(8))(mmbuffer)
  if {mmbuffer-position >= mmbuffer-length}
    nil
    let ((result aref(mmbuffer mmbuffer-position)))
      declare $ type (unsigned-byte 8) result
      if {result > 127}
        error "Code point >127: ~S." result
        code-char result




defstruct scope ; The data stored in a single scope
  active-variables ; :type hash-table ; t if active
  active-hypotheses ; :type vector of list(label is-$f)
  disjoint-variables ; :type vector of hash-tables ; each $d
  floating-hypotheses ; :type hash-table ; maps variable->label of typedef

defun create-scope ()
  make-scope
    :active-variables \\ make-hash-table(:test #'eq)
    :active-hypotheses \\ make-array(10 :fill-pointer 0 :adjustable t)
    :disjoint-variables \\ make-array(10 :fill-pointer 0 :adjustable t)
    :floating-hypotheses \\ make-hash-table(:test #'eq)

; list of currently-active scopes; first is deepest-nested scope
defparameter *scopes* list(create-scope())

defparameter *constants* make-hash-table(:test #'eq :size 4000)
defparameter *variables* make-hash-table(:test #'eq :size 1000)
defparameter *hypotheses* make-hash-table(:test #'eq) ; label -> (stmt is-$f)
defparameter *assertions* make-hash-table(:test #'eq)

defun label-used-p (label)
  {gethash(label *hypotheses*) or gethash(label *assertions*)}

defun get-floating-hyp (hyp)
  some (lambda (scope) gethash(hyp (scope-floating-hypotheses scope))) *scopes*

defun active-variable-p (token)
  some (lambda (scope) gethash(token (scope-active-variables scope))) *scopes*

; Return "true" if c is whitespace character.
declaim $ inline whitespace-char-p
defun whitespace-char-p (c)
  ; declare(optimize(speed(3) safety(0)))
  ; declare(type character c)
  {char=(c #\space) or not(graphic-char-p(c))}


declaim $ inline consume-whitespace
defun consume-whitespace ()
  declare $ optimize speed(3) safety(0)
  iter
     for c = my-peek-char()
     while {c and whitespace-char-p(c)}
     my-read-char()

; Skip characters within a "$(" comment.
; "$)" ends, but must be whitespace separated.
; TODO: Handle embedded x$) correctly; perhaps last line consume-non-whitespace
defun read-comment ()
  declare $ optimize speed(3) safety(0)
  iter
    consume-whitespace
    for c = my-peek-char()
    if not(c)
      error "Unterminated comment"
    if eq(c #\$)
      if eq(read-token() '|$)|) finish()
      my-read-char()

; Read a whitespace-terminated token; returns it as a symbol. EOF returns nil.
; This first skips any leading whitespace.
; We intentionally intern all symbols into private symbols within this
; package, so we don't pollute the namespace of potential callers, and so
; that we can easily inline constants like '|$p|.  We could put the symbols
; into *package*, but then we'd need to change the rest of the code so that
; comparisons with inline constants like '|$p| would work.
defun read-token ()
  ; declare $ optimize speed(3) safety(0)
  consume-whitespace()
  let
    $ cur make-array(20 :element-type 'character :fill-pointer 0 :adjustable t)
    if not(my-peek-char())
      nil
      iter
        for c = my-peek-char() ; include whitespace
        while {c and not(whitespace-char-p(c))}
        vector-push-extend my-read-char() cur
        finally $ return
          intern coerce(cur 'simple-string) *self-package*

; Read a token, but skip $(...$)
defun read-token-skip-comment ()
  iter
    for token = read-token()
    when eq(token '|$(|)
      read-comment()
      next-iteration()
    return token

; Read tokens until terminator, return list of tokens
defun read-to-terminator (terminator)
  iter
    for token = read-token-skip-comment()
    if not(token)
      error "Early termination without ~S~%" terminator
    while not(eq(token terminator))
    collect token

; Return true if sym is a "math symbol" (does not contain "$")
; See Matamath book section 4.1.1.
declaim $ inline mathsymbolp
defun mathsymbolp (sym)
  not(find(#\$ symbol-name(sym)))

; Return true if it's a list of length 1, trickier to be efficient.
declaim $ inline length1p
defun length1p (list)
  {list and consp(list) and not(cdr(list))}

; Read rest of $c statement, add to *constants*
defun read-constants ()
  if not(length1p(*scopes*))
    error "$c statement incorrectly occurs in inner block"
  iter
    with listempty = t ; We'll assume empty list until shown otherwise
    for token = read-token-skip-comment()
    if not(token)
      error "Unterminated $c"
    while not(eq(token '|$.|))
    setf listempty nil ; We see a non-"$."
    if not(mathsymbolp(token))
      error "Attempt to declare non-mathsymbol ~S as constant" token
    if gethash(token *variables*)
      error "Attempt to redeclare variable ~S as constant" token
    if label-used-p(token)
      error "Attempt to reuse label ~S as constant" token
    if gethash(token *constants*)
      error "Constant redeclaration attempted for ~S~%" token
      setf gethash(token *constants*) t ; Set constant entry.
    finally
      if listempty
        error "Empty $c list"
        nil

; Read rest of $d statement
defun read-disjoint ()
  ; format t "Reading disjoint~%"
  iter
    with disjoint-variables = make-hash-table()
    for token = read-token-skip-comment()
    if not(token)
      error "Unterminated $d"
    while not(eq(token '|$.|))
    if not(active-variable-p(token))
      error "Token ~S is not an active variable, but in $d statement" token
    if gethash(token disjoint-variables)
      error "$d statement mentions ~S twice" token
      setf gethash(token disjoint-variables) t
    finally
      if {hash-table-count(disjoint-variables) < 2}
        error "Not enough items in $d statement"
        vector-push-extend disjoint-variables
          scope-disjoint-variables(first(*scopes*))

; Read rest of $v statement
defun read-variables ()
  iter
    with listempty = t ; We'll assume empty list until shown otherwise
    for token = read-token-skip-comment()
    if not(token)
      error "Unterminated $v"
    while not(eq(token '|$.|))
    setf listempty nil ; We see a non-"$."
    if not(mathsymbolp(token))
      error "Attempt to declare non-mathsymbol ~S as a variable" token
    if gethash(token *constants*)
      error "Attempt to redeclare constant ~S as a variable" token
    if label-used-p(token)
      error "Attempt to reuse label ~S as a variable" token
    if active-variable-p(token)
      error "Variable redeclaration attempted for ~S~%" token
      progn
        setf gethash(token *variables*) t
        setf gethash(token (scope-active-variables first(*scopes*))) t
    finally
      if listempty
        error "Empty $v list"
        nil

; Read rest of $f
defun read-f (label)
  ; format t "Reading $f in label ~S~%" label
  let ((statement read-to-terminator('|$.|)))
    if {length(statement) /= 2}
      error "Must have exactly two symbols in $f"
    if not(gethash(first(statement) *constants*))
      error "First symbol in $f statement ~S is ~S, which is not a constant"
        label \\ first(statement)
    if not(active-variable-p(second(statement)))
      error "Second symbol in $f statement ~S is ~S, which is not a variable"
        label \\ second(statement)
    ; Create new floating hypothesis
    ; Like hypotheses.insert(std::make_pair(label,std::make_pair(newhyp,true)))
    setf gethash(label *hypotheses*) list(statement t)
    ; Like scopes.back().activehyp.push_back(label);
    vector-push-extend label scope-active-hypotheses(first(*scopes*))
    ; Like scopes.back().floatinghyp.insert(std::make_pair(variable, label));
    setf
      gethash second(statement) scope-floating-hypotheses(first(*scopes*))
      label

defun read-expression (statement-type label terminator)
  let ((expression read-to-terminator(terminator)))
    if null(expression)
      error "Unfinished $~c statement ~S" statement-type label
    if not(gethash(first(expression) *constants*))
      error "First symbol in $~c statement ~S is ~S which is not a constant"
        statement-type \\ label \\ first(expression)
    iter
      for sym in expression
      if {not(gethash(sym *constants*)) and not(get-floating-hyp(sym))}
        error "In $~c statement ~S, token ~S is not constant or variable in $f"
          statement-type \\ label \\ sym
    expression ; return the expression (as a list)

; Read rest of $e
defun read-e (label)
  ; format t "Reading $e in label ~S~%" label
  let ((expression read-expression(#\e label '|$.|)))
    ; Like hypotheses.insert(std::make_pair(label,std::make_pair(newhyp,false)))
    setf gethash(label *hypotheses*) list(expression nil)
    vector-push-extend label scope-active-hypotheses(first(*scopes*))

; Read rest of $a
; TODO: Really handle
defun read-a (label)
  declare $ ignore label
  ; format t "Reading $a in label ~S~%" label
  read-to-terminator (quote |$.|)

; Read rest of $p
; TODO: Really handle
defun read-p (label)
  declare $ ignore label
  ; format t "Reading $p in label ~S~%" label
  read-to-terminator (quote |$.|)

; Read statement labelled "label".
defun read-labelled (label)
  if-let (token read-token())
    cond
      eq(token '|$f|) read-f(label)
      eq(token '|$e|) read-e(label)
      eq(token '|$a|) read-a(label)
      eq(token '|$p|) read-p(label)
      t error("Unknown operation ~S after label ~S~%" token label)
    error "Cannot end on label"

defun do-nothing ()

defun show-status ()
  format t " Status:~%"
  let ((*package* find-package('cl-metamath)))
    format t "  *constants* = ~S~%" hash-table-keys(*constants*)
    format t "  *variables* = ~S~%" hash-table-keys(*variables*)
    format t "  *hypotheses* = ~S~%" hash-table-keys(*hypotheses*)
    format t "  *assertions* = ~S~%" hash-table-keys(*assertions*)

; Read a metamath file from *standard-input*
defun process-metamath-file ()
  ; declare $ optimize speed(3) safety(0)
  format t "process-metamath-file.~%"
  iter
    for token next read-token()
    while token
    ; Note - at this point "token" is not null.
    ; TODO: Handle "begin with $" specially - error if not listed.
    cond
      eq(token '|$(|) read-comment()
      eq(token '|$c|) read-constants()
      eq(token '|$d|) read-disjoint()
      eq(token '|$v|) read-variables()
      eq(token '|${|) do-nothing() setq(*scopes* cons(create-scope() *scopes*))
      eq(token '|$}|) do-nothing()
        setq *scopes* rest(*scopes*)
        if null(*scopes*)
          error "$} without corresponding ${"
      t read-labelled(token)
  format t " DEBUG: Processing file complete.~%"
  show-status()

; main entry for command line.
defun main ()
  format t "Starting metamath.~%"
  ; Profile code.
  ; Load .mm file.  For speed we'll load the whole thing straight to memory.
  ; We force people to provide a filename (as parameter #1), so later if we
  ; use mmap there will be no interface change.
  format t "command line ~S~%" my-command-line()
  load-mmfile (car my-command-line())
  ;
  format t "File loaded.  Now processing.~%"
  ;
  ; require :sb-sprof
  ; sb-sprof:with-profiling
  ;   :report :flat :show-progress t
  ;   process-metamath-file()
  process-metamath-file()
  ;
  ;
  format t "Ending.~%"

; End of file, restore readtable.
(readable:disable-readable)
