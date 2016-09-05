; Implement metamath-related operations

; Startup.
; We assume we have a working ASDF.  Force load of local quicklisp.
(load (merge-pathnames "quicklisp/setup.lisp" 
      (user-homedir-pathname)))

; Load Libraries
(quicklisp:quickload "readable" :silent t)
(quicklisp:quickload "iterate" :silent t)

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
      ; t princ(tok)

format t "Starting.~%"
process-metamath-file()
format t "Ending.~%"

; End of file, restore readtable.
(readable:disable-readable)
