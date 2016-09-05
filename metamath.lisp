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

(require :sb-posix)

; osicat requires a version of ASDF that's not in Ubuntu 2014, and
; we can't override sbcl's ASDF, so we're stuck - can't use osicat for
; common setups (like Travis).
; (quicklisp:quickload "osicat" :silent t)  ; Use "mmap" for fast reading

; Switch to sweet-expressions - from here on it's more readable.
(readable:enable-sweet)

; Import symbols from "iterate" package for ease-of-use.  Annoyingly,
; it exports the language keyword "iterate", so we can't just call use-package.
; Instead, import everything *except* the conflicting symbol.
do-external-symbols (sym find-package('iterate))
  if not(eq(sym 'iterate:iterate))
    import(sym)

; Return "true" if c is whitespace character.
defun whitespace-char-p (c)
  declare $ type character c
  {char=(c #\space) or not(graphic-char-p(c))}

defun consume-whitespace ()
  declare $ optimize speed(3) safety(0)
  iter
     for c = peek-char(nil nil nil nil)
     while {c and whitespace-char-p(c)}
     read-char()
declaim $ inline consume-whitespace

; Read a whitespace-terminated token; returns it as a symbol. EOF returns nil.
defun read-token ()
  declare $ optimize speed(3) safety(0)
  let
    $ cur make-array(20 :element-type 'character :fill-pointer 0 :adjustable t)
    consume-whitespace()
    if not(peek-char(nil nil nil nil))
      nil
      iter
         for c = peek-char(nil nil nil nil) ; include whitespace
         while {c and not(whitespace-char-p(c))}
         ; collect (the character read-char()) into letters
         vector-push-extend read-char() cur
         finally $ return
           intern coerce(cur 'simple-string)


; Skip characters within a "$(" comment.
; "$)" ends, but must be whitespace separated.
defun read-comment ()
  declare $ optimize speed(3) safety(0)
  iter
     consume-whitespace
     for c = peek-char(nil nil nil nil)
     while c
     if eq(c #\$)
       if eq(read-token() '|$)|) finish()
       read-char()

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
      t princ(tok)

defun read-char-repeatedly ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  iter
     for c = read-char(nil nil nil nil)
     while c


; Here's a non-portable SBCL-specific approach, but can't get it working:
; See: https://groups.google.com/forum/#!topic/comp.lang.lisp/MlyWkTNwks4
(defun mmap-lisp-object (map-file)
  "Return a lisp object, corresponding to data dumped into a file."
  (with-open-file (file map-file)
    (let* ((sap (sb-posix:mmap nil
                               (file-length file)
                               sb-posix:prot-read
                               sb-posix:map-private
                               (sb-impl::fd-stream-fd file)
                               0))
           (addr (logior (sb-sys:sap-int sap)
                         sb-vm:other-pointer-lowtag)))
      (sb-kernel:make-lisp-obj addr))))
; This tries to use it, and fails.
; defvar mm
; setq mm mmap-lisp-object("hello.mm")

; Profile code.  The naive read-char implementation spends all its time
; in read-char/peek-char functions (including the Lisp/C interface and syscall)
; (require :sb-sprof)
; (sb-sprof:with-profiling (:report :flat
;                           :show-progress t)
;  process-metamath-file())

; http://www.gigamonkeys.com/book/files-and-file-io.html
;  READ-SEQUENCE is likely to be quite a bit more efficient
;  than filling a sequence by repeatedly calling READ-BYTE or READ-CHAR.

; http://stackoverflow.com/questions/38667846/how-to-improve-the-speed-of-reading-a-large-file-in-common-lisp-line-by-line
;  shows an example
(defun count-lines (file &optional (buffer-size 50000000)) ; Orig size 32768
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum buffer-size))
  (let* ((buffer-element-type '(unsigned-byte 8))
        (buffer
         (make-array buffer-size
                     :element-type buffer-element-type))
        (sum 0)
        (end 0))
    (declare (type fixnum sum end))
    (with-open-file (in file :element-type buffer-element-type)
      (loop
         (setf end (read-sequence buffer in))
         (when (= end 0)
           (return sum))
         (dotimes (i end)
           (declare (type fixnum i)
                    (dynamic-extent i))
           (when (= 10
                    (aref buffer i))
             (incf sum)))))))

defvar mmbuffer
defun load-mmfile (filename)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  let*
    (
      (buffer-element-type '(unsigned-byte 8))
      (mmbuffer make-array(50000000 :element-type '(unsigned-byte 8)))
    with-open-file (in filename :element-type '(unsigned-byte 8))
      read-sequence mmbuffer in

format t "Starting.~%"

; This takes less than 1 second - read in *en masse* using read-sequence
load-mmfile "../set.mm/set.mm"

; This takes less than 1 second, but the interior structure
; makes it less convenient:
; count-lines("../set.mm/set.mm")

; This takes 4 seconds - there may be mild specialized optimizations
; that don't scale (not clear yet):
; read-char-repeatedly()

; This takes about 23 seconds (reading and consing):
; process-metamath-file()

format t "Ending.~%"

; End of file, restore readtable.
(readable:disable-readable)
