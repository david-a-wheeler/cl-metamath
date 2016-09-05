; Implement metamath-related operations

; Startup.
; We assume we have a working ASDF.  Force load of local quicklisp.
(load (merge-pathnames "quicklisp/setup.lisp"
      (user-homedir-pathname)))

; Turn on optimization early.
(declaim (optimize speed))

; Load Libraries
(quicklisp:quickload "readable" :silent t) ; Easy-to-read program notation
(quicklisp:quickload "iterate" :silent t)  ; Improved iterator

; Switch to sweet-expressions - from here on it's more readable.
(readable:enable-sweet)

; Import symbols from "iterate" package for ease-of-use.  Annoyingly,
; it exports the language keyword "iterate", so we can't just call use-package.
; Instead, import everything *except* the conflicting symbol.
do-external-symbols (sym find-package('iterate))
  if not(eq(sym 'iterate:iterate))
    import(sym)


; Extra code to *quickly* read files.  This deserves an explanation.
; Lisp's read-char and peek-char provide the necessary functions, but per
; <http://www.gigamonkeys.com/book/files-and-file-io.html>,
;  "READ-SEQUENCE is likely to be quite a bit more efficient
;  than filling a sequence by repeatedly calling READ-BYTE or READ-CHAR."
;  (verified in SBCL by profiling).
; I'd like to use mmap(), but I've had a lot of trouble getting that to work.
; The osicat library requires a version of ASDF that's not in Ubuntu 2014, and
; we can't override sbcl's ASDF, so we can't use osicat for common setups.
; SBCL has a nonportable interface to mmap, but it's poorly documented.

; For now, just load the whole file at once.  It the future we could
; use a loop over a big buffer.  For an example, see:
; http://stackoverflow.com/questions/38667846/how-to-improve-the-speed-of-reading-a-large-file-in-common-lisp-line-by-line

defvar mmbuffer
defvar mmbuffer-position 0
defvar mmbuffer-length 0
defun load-mmfile (filename)
  ; (declare (optimize (speed 3) (debug 0) (safety 0)))
  let* ((buffer-element-type '(unsigned-byte 8)))
    setf mmbuffer make-array(100000000 :element-type buffer-element-type)
    with-open-file (in filename :element-type '(unsigned-byte 8))
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
    code-char aref(mmbuffer mmbuffer-position)

; Return "true" if c is whitespace character.
defun whitespace-char-p (c)
  declare $ type character c
  {char=(c #\space) or not(graphic-char-p(c))}

defun consume-whitespace ()
  declare $ optimize speed(3) safety(0)
  iter
     for c = my-peek-char()
     while {c and whitespace-char-p(c)}
     my-read-char()
declaim $ inline consume-whitespace

; Read a whitespace-terminated token; returns it as a symbol. EOF returns nil.
defun read-token ()
  declare $ optimize speed(3) safety(0)
  let
    $ cur make-array(20 :element-type 'character :fill-pointer 0 :adjustable t)
    consume-whitespace()
    if not(my-peek-char())
      nil
      iter
         for c = my-peek-char() ; include whitespace
         while {c and not(whitespace-char-p(c))}
         ; collect (the character my-read-char()) into letters
         vector-push-extend my-read-char() cur
         finally $ return
           intern coerce(cur 'simple-string)


; Skip characters within a "$(" comment.
; "$)" ends, but must be whitespace separated.
defun read-comment ()
  declare $ optimize speed(3) safety(0)
  iter
     consume-whitespace
     for c = my-peek-char()
     while c
     if eq(c #\$)
       if eq(read-token() '|$)|) finish()
       my-read-char()

; Read a metamath file from *standard-input*
defun process-metamath-file ()
  declare $ optimize speed(3) safety(0)
  format t "process-metamath-file.~%"
  iter
    declare $ type atom tok
    for tok next read-token()
    while tok
    ; print tok
    cond
      eq(tok '|$(|) read-comment()
      ; t format(t "~S~%" tok)

; Profile code.
; (require :sb-sprof)
; (sb-sprof:with-profiling (:report :flat
;                           :show-progress t)
;  process-metamath-file())

format t "Starting.~%"

; This takes less than 1 second - read in *en masse* using read-sequence
load-mmfile "../set.mm/set.mm"
; load-mmfile "hello.mm"

format t "File loaded.  Now processing.~%"
process-metamath-file()

format t "Ending.~%"

; End of file, restore readtable.
(readable:disable-readable)
