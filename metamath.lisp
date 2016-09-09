;;;; Implement metamath-related operations

;;; Startup.
;;; We assume we have the following working:
;;; - ASDF (for loading packages on machine) / uiop (for basic I/O)
;;; - Quicklisp (to download/manage external packages)

(cl:in-package #:cl-user)
(declaim (optimize speed))

; Load Libraries
(require "asdf")
(quicklisp:quickload "readable" :silent t) ; Easy-to-read program notation
(quicklisp:quickload "iterate" :silent t)  ; Improved iterator
(quicklisp:quickload "alexandria" :silent t)  ; Improved iterator

(defpackage #:metamath
  (:use #:cl
        #:iterate ; Improved iterator
        #:uiop)   ; Part of ASDF, provides *command-line-arguments*
  (:export #:main
           #:load-mmfile
           #:process-metamath-file))

(cl:in-package #:metamath)

; Switch to sweet-expressions - from here on the code is more readable.
(readable:enable-sweet)

import 'alexandria:define-constant

; TODO: Find the routine to do this less hackishly; it's not *package*.
define-constant self-package find-package('metamath)

; Extra code to *quickly* read files.  This brings tokenization times down
; from ~24 seconds to ~4 seconds.  This deserves an explanation.
; Lisp's read-char and peek-char provide the necessary functions, but per
; <http://www.gigamonkeys.com/book/files-and-file-io.html>,
;  "READ-SEQUENCE is likely to be quite a bit more efficient
;  than filling a sequence by repeatedly calling READ-BYTE or READ-CHAR."
;  (verified in SBCL by profiling).
; I'd like to use mmap(), but I've had a lot of trouble getting that to work.
; The osicat library requires a version of ASDF that's not in Ubuntu 2014, and
; we can't override sbcl's ASDF, so we can't use osicat for common setups.
; SBCL has a nonportable interface to mmap, but it's poorly documented.
; For now, just load the whole file at once.
;
; TODO:  In the future we could use a loop over a big buffer & handle
; multiple files.
; For an example, see:
; http://stackoverflow.com/questions/38667846/how-to-improve-the-speed-of-reading-a-large-file-in-common-lisp-line-by-line

defvar mmbuffer
defvar mmbuffer-position 0
defvar mmbuffer-length 0
defun load-mmfile (filename)
  format t "DEBUG: Starting load-mmfile~%"
  ; (declare (optimize (speed 3) (debug 0) (safety 0)))
  let* ((buffer-element-type '(unsigned-byte 8)))
    setf mmbuffer make-array(100000000 :element-type buffer-element-type)
    with-open-file (in filename :element-type '(unsigned-byte 8))
      format t "DEBUG: Inside with-open-file~%"
      setf mmbuffer-length read-sequence(mmbuffer in)
  nil

; Optimized version of read-char()
defun my-read-char ()
  if {mmbuffer-position >= mmbuffer-length}
    nil
    let ((result aref(mmbuffer mmbuffer-position)))
      incf mmbuffer-position
      code-char result

; Optimized version of peek-char(nil nil nil nil)
defun my-peek-char ()
  if {mmbuffer-position >= mmbuffer-length}
    nil
    let ((result aref(mmbuffer mmbuffer-position)))
      if {result > 127}
        error "Code point >127: ~S." result
        code-char result
;

; Return "true" if c is whitespace character.
defun whitespace-char-p (c)
  declare $ type character c
  {char=(c #\space) or not(graphic-char-p(c))}

declaim $ inline consume-whitespace
defun consume-whitespace ()
  declare $ optimize speed(3) safety(0)
  iter
     for c = my-peek-char()
     while {c and whitespace-char-p(c)}
     my-read-char()

define-constant this-package *package*

; Read a whitespace-terminated token; returns it as a symbol. EOF returns nil.
; This first skips any leading whitespace.
; We intentionally intern all symbols into private symbols within this
; package, so we don't pollute the namespace of potential callers, and so
; that we can easily inline constants like '|$p|.  We could put the symbols
; into *package*, but then we'd need to change the rest of the code so that
; comparisons with inline constants like '|$p| would work.
defun read-token ()
  declare $ optimize speed(3) safety(0)
  consume-whitespace()
  let
    $ cur make-array(20 :element-type 'character :fill-pointer 0 :adjustable t)
    if not(my-peek-char())
      nil
      iter
         for c = my-peek-char() ; include whitespace
         while {c and not(whitespace-char-p(c))}
         ; collect (the character my-read-char()) into letters
         vector-push-extend my-read-char() cur
         finally $ return
           intern coerce(cur 'simple-string) self-package


; Skip characters within a "$(" comment.
; "$)" ends, but must be whitespace separated.
; TODO: Detect unterminated comment.
defun read-comment ()
  declare $ optimize speed(3) safety(0)
  format t "DEBUG read-comment"
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
     format t "DEBUG: In read-to-terminator ~S~%" tok


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
       format t "DEBUG: Adding constant ~S~%" tok
       finally
         if listempty
           error "Empty $c list"
           nil

; Read rest of $d statement
; TODO: Really handle
defun read-distinct ()
  format t "Reading distinct~%"
  read-to-terminator (quote |$.|)

; Read rest of $v statement
; TODO: Really handle
defun read-variables ()
  format t "Reading variables~%"
  read-to-terminator (quote |$.|)

; Read rest of $f
; TODO: Really handle
defun read-f (label)
  format t "Reading $f in label ~S~%" label
  read-to-terminator (quote |$.|)

; Read rest of $e
; TODO: Really handle
defun read-e (label)
  format t "Reading $e in label ~S~%" label
  read-to-terminator (quote |$.|)

; Read rest of $a
; TODO: Really handle
defun read-a (label)
  format t "Reading $a in label ~S~%" label
  read-to-terminator (quote |$.|)

; Read rest of $p
; TODO: Really handle
defun read-p (label)
  format t "Reading $p in label ~S~%" label
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

; Read a metamath file from *standard-input*
defun process-metamath-file ()
  declare $ optimize speed(3) safety(0)
  format t "process-metamath-file.~%"
  iter
    declare $ type atom tok
    for tok next read-token()
    while tok
    print tok
    ; Note - at this point "tok" is not null.
    ; TODO: Handle "begin with $" specially - error if not listed.
    cond
      eq(tok '|$(|) read-comment()
      eq(tok '|$c|) read-constant()
      eq(tok '|$d|) read-distinct()
      eq(tok '|$v|) read-variables()
      eq(tok '|${|) format(t "DEBUG: ${~%") ; TODO
      eq(tok '|$}|) format(t "DEBUG: $}~%") ; TODO
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
  load-mmfile car(cddr(*command-line-arguments*))
  ;
  format t "File loaded.  Now processing.~%"
  ;
  process-metamath-file()
  ;
  format t "Ending.~%"

; End of file, restore readtable.
(readable:disable-readable)
