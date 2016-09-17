;;;; Implement metamath-related operations
;;;; Copyright 2016, David A. Wheeler and the cl-metamath contributors
;;;; SPDX-License-Identifier: MIT

;;; This is designed to be high performance. We want the code to be clear,
;;; but some decisions make a big difference to performance.
;;; This is in many ways inspired by checkmm.cpp (which is CC0 licensed).

(cl:in-package #:cl-metamath)

; Switch to sweet-expressions - from here on the code is more readable.
(readable:enable-sweet)

; declaim $ optimize speed ; Uncomment to see optimization hints

defvar *author* "David A. Wheeler <dwheeler@dwheeler.com>"
defvar *license* "MIT"

defmacro defun-inline (name parameters &body body)
  "Define an inline function"
  ` declaim $ inline ,name
  ` defun ,name ,parameters
      ,@body

; TODO: Find the routine to do this less hackishly; it's not *package*.
defparameter *self-package* find-package('cl-metamath)

; Extra code to *quickly* read files.

defvar mmbuffer
declaim $ type simple-array(unsigned-byte(8)) mmbuffer
defvar mmbuffer-position 0
defvar mmbuffer-length 0
declaim $ type fixnum mmbuffer-position mmbuffer-length
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
  declare $ ftype function(() {character or null}) my-read-char
  if {mmbuffer-position >= mmbuffer-length}
    nil
    let ((result aref(mmbuffer mmbuffer-position)))
      declare $ type (unsigned-byte 8) result
      setf mmbuffer-position $ the fixnum $ 1+ mmbuffer-position
      code-char result

; Optimized version of peek-char(nil nil nil nil)
defun my-peek-char ()
  declare $ optimize speed(3) safety(0)
  declare $ ftype function(() {character or null}) my-peek-char
  if {mmbuffer-position >= mmbuffer-length}
    nil
    let ((result aref(mmbuffer mmbuffer-position)))
      declare $ type (unsigned-byte 8) result
      if {result > 127}
        error "Code point >127: ~S." result
        code-char result

; TODO: specify :type of members.
defstruct assertion ; The data stored about one assertion (axiom or theorem)
  hypotheses nil :type vector(list)
  disjoint-variables ; set of pairs
  expression nil :type vector(symbol)
  proof-info nil :type list ; list (incomplete? sequence).  Sequence is array,nil if assertion.
  ; variables ; variables used (as hash).  Stored so we can parallelize?
  ;  - won't work, lookups of hypotheses, etc., will also go through
  ;  mutating hash tables.  Just read, then parallel check.

defstruct scope ; The data stored in a single scope
  active-variables nil :type hash-table ; symbol -> boolean
  active-hypotheses nil :type vector(symbol) ; symbols are labels
  disjoint-variables nil :type vector(hash-table) ; each $d
  floating-hypotheses nil :type hash-table ; variable->typedef label

defun create-scope ()
  make-scope
    :active-variables \\ make-hash-table(:test #'eq)
    :active-hypotheses \\ make-array(10 :fill-pointer 0 :adjustable t)
    :disjoint-variables \\ make-array(10 :fill-pointer 0 :adjustable t)
    :floating-hypotheses \\ make-hash-table(:test #'eq)

; list of currently-active scopes; first is deepest-nested scope
defparameter *scopes* list(create-scope())
declaim $ type list *scopes*

defparameter *constants* make-hash-table(:test #'eq :size 4000)
defparameter *variables* make-hash-table(:test #'eq :size 1000)
defparameter *hypotheses* make-hash-table(:test #'eq) ; label -> (stmt is-$f)
defparameter *assertions* make-hash-table(:test #'eq) ; label -> assertion
declaim $ type hash-table *constants* *variables* *hypotheses* *assertions*

defun label-used-p (label)
  {gethash(label *hypotheses*) or gethash(label *assertions*)}

defun get-floating-hyp (hyp)
  some (lambda (scope) gethash(hyp (scope-floating-hypotheses scope))) *scopes*

defun active-variable-p (token)
  some (lambda (scope) gethash(token (scope-active-variables scope))) *scopes*

; This does a linear search, but active-hypotheses is expected to be short.
defun active-hyp-p (token)
  some (lambda (scope)
          position(token (scope-active-hypotheses scope) :test #'eq)) *scopes*

; Return "true" if c is Metamath whitespace character. c must be a character.
defun-inline whitespace-char-p (c)
  declare $ optimize(speed(3) safety(0))
  declare $ type character c
  member c '(#\space #\linefeed #\tab #\page #\return)

defun-inline consume-whitespace ()
  declare $ optimize speed(3) safety(0)
  iter
    declare $ type {character or nil} c
    for c = my-peek-char()
    while {c and whitespace-char-p(c)}
    my-read-char()

; Skip characters within a "$(" comment.
; "$)" ends, but must be whitespace separated.
; TODO: Handle embedded x$) correctly; perhaps last line consume-non-whitespace
defun read-comment ()
  declare $ optimize speed(3) safety(0)
  iter
    ; declare $ type {character or null} c ; null, not nil; nil means "no type"
    consume-whitespace()
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
  declare $ ftype function(() {symbol or null}) read-token
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
  declare $ ftype function(() {symbol or null}) read-token-skip-comment
  iter
    for token = read-token()
    when eq(token '|$(|)
      read-comment()
      next-iteration()
    return token

; ; Read tokens until terminator, return vector of tokens
; defun read-to-terminator (terminator)
;   declare $ ftype function(() vector) read-to-terminator
;   iter
;     with tokens = make-array(254 :fill-pointer 0 :adjustable t)
;     for token = read-token-skip-comment()
;     if not(token)
;       error "Early termination without ~S~%" terminator
;     while not(eq(token terminator))
;     vector-push-extend token tokens
;     finally
;       tokens

; Read tokens until terminator, return list of tokens
; Probably should be removed & have loops moved into their own functions.
defun read-to-terminator (terminator)
  declare $ ftype function(() list) read-to-terminator
  iter
    for token = read-token-skip-comment()
    if not(token)
      error "Early termination without ~S~%" terminator
    while not(eq(token terminator))
    collect token

; Return true if sym is a "math symbol" (does not contain "$")
; See Matamath book section 4.1.1.
defun-inline mathsymbolp (sym)
  declare $ ftype function((symbol) boolean) mathsymbolp
  not(find(#\$ symbol-name(sym)))

; Return true if it's a list of length 1, trickier to be efficient.
defun-inline length1p (list)
  {list and consp(list) and not(cdr(list))}

defun-inline mmvariablep (symbol) ; Return true-value if symbol is mm variable
  gethash(symbol *variables*)

defun-inline variables-in (expression)
  declare $ ftype function((vector) vector) variables-in
  remove-if-not #'mmvariablep expression

; defun hash-table-t-from-list (old-list) ; Create hash table with "t" values
;   declare $ ftype function((list) hash-table) hash-table-t-from-list
;   let ((new-table make-hash-table(:test #'eq)))
;     dolist (x old-list new-table)
;       setf gethash(x new-table) t

defun hash-table-t-from-vector (vec) ; Create hash table with "t" values
  declare $ ftype function((vector) hash-table) hash-table-t-from-vector
  declare $ type vector(symbol) vec
  iter
    with new-table = make-hash-table(:test #'eq)
    for key in-vector vec with-index i
    declare $ type fixnum i
    declare $ type symbol key
    setf gethash(key new-table) t
    finally $ return new-table

; Insert into array.  *Modifies* vector.
defun insert-into-array (vector value position)
  declare $ ftype function((vector t fixnum) vector) insert-into-array
  declare $ type fixnum position ; this shouldn't be needed
  replace(vector vector :start2 position :start1 (the fixnum 1+(position))
           :end2 vector-push-extend(value vector))
  setf (aref vector position) value
  vector

defun verify-assertion-ref (label step stack)
  "Verify step given stack; modifies stack (which must have a fill pointer)"
  declare $ ftype function((symbol symbol vector) boolean) verify-assertion-ref
  format t " DEBUG6: step ~S stack ~S~%" step stack
  let*
    \\
      assertion gethash(step *assertions*)
      this-assertion-hypotheses assertion-hypotheses(assertion)
      num-assertion-hypotheses length(this-assertion-hypotheses)
      base {length(stack) - num-assertion-hypotheses}
      ; TODO: define
      ; substitutions make-hash-table(:test #'eq) ; variable->expression
    if {base < 0}
      error "In proof of theorem ~A step ~A stack too short" label step
    iter (for i from 0 below num-assertion-hypotheses)
      declare $ type fixnum i
      let ; walk through each hypothesis
        $ hypothesis gethash(elt(this-assertion-hypotheses i) *hypotheses*)
        if second(hypothesis) ; is it floating?
          progn ; Floating hypothesis
            format t " DEBUG7a: floating. ~S - ~S~%" first(hypothesis) elt(first(hypothesis) 1)
            if not(eq(first(hypothesis) elt(elt(stack {base + i}) 0)))
              error "In proof of theorem ~A unification failed - type" label
              ; TODO: substitution. 589
              ; $ subst first(hypothesis) expression
          progn ; Essential hypothesis
            ; TODO
            ; format t " DEBUG7b: essential. ~S~%" hypothesis
            do-nothing()
    ; TODO: Remove hypothesis from stack 609 - check for off-by-one
    setf (fill-pointer stack) 1-(base)
    format t " DEBUG12 - after setf fill-pointer: step ~S stack ~S~%" step stack
    ; TODO: Verify disjoint variable conditions
    ; TODO: Done verification of this step; insert new statement onto stack
    vector-push-extend '(term t BOGUS) stack ; Push just the expression
  nil

defun verify-proof (label)
  declare $ ftype function((symbol) boolean) verify-proof
  format t "DEBUG: entering verify-proof ~A~%" label
  let*
    \\
      assertion gethash(label *assertions*)
      proof-info (assertion-proof-info assertion)
      incomplete first(proof-info)
      proof second(proof-info)
      ; TODO: declare :element-type for stack.
      stack make-array(200 :fill-pointer 0 :adjustable t)
    when incomplete
      ; format t "Skipping verification of incomplete proof ~A~%" label
      return-from verify-proof nil
    iter
      for step in-vector proof with-index i
      declare $ type fixnum i
      format t "DEBUG5: checking step ~S~%" step
      ; format t "~A " step
      if-let (hyp gethash(step *hypotheses*))
        vector-push-extend first(hyp) stack ; Push just the expression
        verify-assertion-ref(label step stack)
    ; format t "~%"
    ; TODO - restore these:
    ; if {length(stack) /= 1}
    ;   error "Proof of theorem ~A doesn't end with only 1 item on stack" label
    ; if not(equal(elt(stack 0) expression))
    ;   error "Proof of theorem ~A proves wrong statement ~S" label elt(stack 0)
  t

defun calculate-disjoint-variables (vars-used)
  declare $ ignore vars-used
  nil ; TODO. ~Line 380

defun construct-assertion (label expression)
  ; format t "DEBUG construct-assertion expression=~S~%" expression
  declare $ ftype function((symbol vector) assertion) construct-assertion
  let*
    \\
      new-assertion
        make-assertion
          :hypotheses \\ make-array(10 :fill-pointer 0 :adjustable t)
          :disjoint-variables \\ nil ; \\ make-hash-table(:test #'equal)
          :expression \\ expression
      vars-used ; We could store this in an assertion slot if we wanted to
        hash-table-t-from-vector(variables-in(expression))
    declare $ type hash-table vars-used ; TODO: Why is vars-used unavailable?
    ; Put active hypotheses in right order, and note vars-used if essential.
    iter (for scope in *scopes*)
      iter (for hyp-name in-vector scope-active-hypotheses(scope) downto 0)
        let ((hyp gethash(hyp-name *hypotheses*))) ; hyp=(name,is-floating)
          ; format t " DEBUG hyp-name=~S~%" hyp-name
          ; format t " DEBUG lookup=~S~%" hyp
          cond
            {second(hyp) and gethash(first(hyp) vars-used)} ; Mandatory floating
              insert-into-array assertion-hypotheses(new-assertion) hyp 0
            not(second(hyp)) ; Essential hypothesis
              insert-into-array assertion-hypotheses(new-assertion) hyp 0
              iter (for sym in-vector first(hyp))
                if mmvariablep(sym)
                  setf gethash(sym vars-used) t
    setf assertion-disjoint-variables(new-assertion)
     calculate-disjoint-variables(vars-used)
    setf gethash(label *assertions*) new-assertion ; Add to set of assertions
    new-assertion

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
  ; TODO: Just read them in one-by-one.
  let ((statement read-to-terminator('|$.|)))
    if {length(statement) /= 2}
      error "Must have exactly two symbols in $f"
    if not(gethash(elt(statement 0) *constants*))
      error "First symbol in $f statement ~S is ~S, which is not a constant"
        label \\ elt(statement 0)
    if not(active-variable-p(elt(statement 1)))
      error "Second symbol in $f statement ~S is ~S, which is not a variable"
        label \\ elt(statement 1)
    ; Create new floating hypothesis
    ; Like hypotheses.insert(std::make_pair(label,std::make_pair(newhyp,true)))
    setf gethash(label *hypotheses*) list(statement t)
    ; Like scopes.back().activehyp.push_back(label);
    vector-push-extend label scope-active-hypotheses(elt(*scopes* 0))
    ; Like scopes.back().floatinghyp.insert(std::make_pair(variable, label));
    setf
      gethash second(statement) scope-floating-hypotheses(elt(*scopes* 0))
      label

defun read-expression (statement-type label terminator)
  "Read a mathematical expression (key part of $a, $p, or $e)"
  declare $ ftype function((character symbol symbol) vector) read-expression
  let
    \\
      first read-token-skip-comment()
      expression make-array(254 :fill-pointer 0 :adjustable t)
    if null(first)
      error "Unfinished $~c statement ~S" statement-type label
    if not(gethash(first *constants*))
      error "First symbol in $~c statement ~S is ~S which is not a constant"
        statement-type \\ label \\ first
    vector-push-extend first expression
    iter
      for sym = read-token-skip-comment()
      if null(sym)
        error "Unfinished $~c statement ~S" statement-type label
      while not(eq(sym terminator))
      if {not(gethash(sym *constants*)) and not(get-floating-hyp(sym))}
        error "In $~c statement ~S, token ~S is not constant or variable in $f"
          statement-type \\ label \\ sym
      vector-push-extend sym expression
    expression ; return the expression (as a vector)

defun read-compressed-proof (label)
  ; TODO
  declare $ ignore label
  read-to-terminator('|$.|)

defun read-uncompressed-proof (label first-token)
  let
    \\
      proof make-array(500 :fill-pointer 0 :adjustable t)
      incomplete nil ; If true, we have an incomplete proof (e.g., "?")
    vector-push-extend first-token proof
    iter
      for token next read-token-skip-comment()
      while not(eq(token '|$.|))
      if not(token)
        error "Unfinished $p statement ~S" label
      if eq(token label)
        error "Proof of theorem ~S refers to itself" label
      if {not(gethash(token *assertions*)) and not(active-hyp-p(token))}
        error "Proof of theorem ~S refers to itself" label
      if eq(token '|?|)
        setq incomplete t
      vector-push-extend token proof
    if incomplete
      format t "Warning: Proof of theorem ~A is incomplete.~%" label
    list(incomplete proof) ; return its incompleteness and the proof

; Read rest of $p
; TODO: Really handle
defun read-p (label)
  ; format t "Reading $p in label ~S~%" label
  let*
    \\
       new-theorem read-expression(#\p label '|$=|)
       assertion construct-assertion(label new-theorem)
       next-token read-token-skip-comment()
    if {length(new-theorem) = 0}
      error "Empty theorem statement ~S" label
    if not(next-token)
      error("Unfinished $p statement ~S" label)
    setf (assertion-proof-info assertion)
      if eq(next-token '\()
        read-compressed-proof(label)
        read-uncompressed-proof(label next-token)
  ; TODO: Verify now.  Maybe wait til later so can parallelize
  verify-proof(label)

; Read rest of $e
defun read-e (label)
  ; format t "Reading $e in label ~S~%" label
  let ((expression read-expression(#\e label '|$.|)))
    ; Like hypotheses.insert(std::make_pair(label,std::make_pair(newhyp,false)))
    setf gethash(label *hypotheses*) list(expression nil)
    vector-push-extend label scope-active-hypotheses(first(*scopes*))

; Read rest of $a
defun read-a (label)
  ; format t "Reading $a in label ~S~%" label
  let ((expression read-expression(#\a label '|$.|)))
    construct-assertion label expression

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
    format t "  *constants* = ~{~A~^ ~}~%" hash-table-keys(*constants*)
    format t "  *variables* = ~{~A~^ ~}~%" hash-table-keys(*variables*)
    format t "  *hypotheses* = ~{~A~^ ~}~%" hash-table-keys(*hypotheses*)
    format t "  *assertions* = ~{~A~^ ~}~%" hash-table-keys(*assertions*)

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
  ; show-status()

; TODO: where's the portable library for this?
;(defun my-command-line ()
;  (or
;   #+SBCL *posix-argv*
;   #+LISPWORKS system:*line-arguments-list*
;   #+CMU extensions:*command-line-words*
;   nil))
defun my-command-line ()
  '("demo0.mm")

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
