;;;; Implement metamath-related operations

;;; This is designed to be high performance. We want the code to be clear,
;;; but some decisions make a big difference to performance.

(cl:in-package #:cl-metamath)

; Switch to sweet-expressions - from here on the code is more readable.
(readable:enable-sweet)

declaim $ optimize speed

; import 'alexandria:define-constant

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
defvar *self-package* find-package('cl-metamath)

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

; Skip characters within a "$(" comment.
; "$)" ends, but must be whitespace separated.
; TODO: Detect unterminated comment.
defun read-comment ()
  declare $ optimize speed(3) safety(0)
  iter
     consume-whitespace
     for c = my-peek-char()
     while c
     if eq(c #\$)
       if eq(read-token() '|$)|) finish()
       my-read-char()

; Read tokens until terminator.
; TODO: This is for stubbing out parsing.
defun read-to-terminator (terminator)
  iter
     for tok = read-token()
     while {tok and not(eq(tok terminator))}
     cond
       eq(tok '|$(|)
         read-comment()
         next-iteration()
     collect tok

; Read rest of $c statement
defun read-constant ()
  ; TODO: Error if scopes.size>1
  ;   "$c statement incorrectly occurs in inner block"
  let ((listempty t))
    iter
       for tok = read-token()
       if not(tok)
         error "Unterminated $c"
       while not(eq(tok (quote |$.|)))
       cond ; Are comments allowed *inside* $c?  Presumably yes..
         eq(tok '|$(|)
           read-comment()
           next-iteration()
       setf listempty nil
       ; TODO:
       ; if tok is mathsymbol -> error "Attempt to declare ~S as constant"
       ; if tok variable -> error "Attempt to redeclare variable ~S as constant"
       ; if tok is label -> error "Attempt to reuse label ~S as constant"
       ; if tok is declared -> error "Attempt to redeclare ~S"
       ; Add constant "tok"
       ; format t "DEBUG: Adding constant ~S~%" tok
       finally
         if listempty
           error "Empty $c list"
           nil

; Read rest of $d statement
; TODO: Really handle
defun read-distinct ()
  ; format t "Reading distinct~%"
  read-to-terminator (quote |$.|)

; Read rest of $v statement
; TODO: Really handle
defun read-variables ()
  ; format t "Reading variables~%"
  read-to-terminator (quote |$.|)

; Read rest of $f
; TODO: Really handle
defun read-f (label)
  declare $ ignore label
  ; format t "Reading $f in label ~S~%" label
  read-to-terminator (quote |$.|)

; Read rest of $e
; TODO: Really handle
defun read-e (label)
  declare $ ignore label
  ; format t "Reading $e in label ~S~%" label
  read-to-terminator (quote |$.|)

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
; TODO: Really handle
defun read-labelled (label)
  let ((tok read-token()))
    if not(tok)
      error "Cannot end on label"
    cond
      eq(tok '|$f|) read-f(label)
      eq(tok '|$e|) read-e(label)
      eq(tok '|$a|) read-a(label)
      eq(tok '|$p|) read-p(label)
      t error("Unknown operation ~S after label ~S~%" tok label)

defun do-nothing ()

; Read a metamath file from *standard-input*
defun process-metamath-file ()
  ; declare $ optimize speed(3) safety(0)
  format t "process-metamath-file.~%"
  iter
    for tok next read-token()
    while tok
    ; format t "~S~%" tok
    ; Note - at this point "tok" is not null.
    ; TODO: Handle "begin with $" specially - error if not listed.
    cond
      eq(tok '|$(|) read-comment()
      eq(tok '|$c|) read-constant()
      eq(tok '|$d|) read-distinct()
      eq(tok '|$v|) read-variables()
      eq(tok '|${|) do-nothing() ; TODO
      eq(tok '|$}|) do-nothing() ; TODO
      t read-labelled(tok)

; main entry for command line.
defun main ()
  format t "Starting metamath.~%"
  ; Profile code.
  ; (require :sb-sprof)
  ; (sb-sprof:with-profiling (:report :flat
  ;                           :show-progress t)
  ;  process-metamath-file())
  ;
  ; Load .mm file.  For speed we'll load the whole thing straight to memory.
  ; We force people to provide a filename (as parameter #1), so later if we
  ; use mmap there will be no interface change.
  format t "command line ~S~%" my-command-line()
  load-mmfile (car my-command-line())
  ;
  format t "File loaded.  Now processing.~%"
  ;
  process-metamath-file()
  ;
  format t "Ending.~%"

; End of file, restore readtable.
(readable:disable-readable)
