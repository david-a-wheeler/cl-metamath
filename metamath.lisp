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
  {char=(c #\space) or not(graphic-char-p(c))}

defun consume-whitespace (&optional (stream *standard-input*))
  iter
     for c = peek-char(nil stream nil nil)
     while {c and whitespace-char-p(c)}
     read-char stream

; Read a whitespace-terminated token; returns it as a symbol. EOF returns nil.
defun read-token (&optional (stream *standard-input*))
  consume-whitespace stream
  iter
     for c = peek-char(nil stream nil nil) ; include whitespace
     while {c and not(whitespace-char-p(c))}
     collect read-char(stream) into letters
     ; finally return(intern(coerce(letters 'string)))
     finally $ return
       if letters
         intern coerce(letters 'string)
         nil

; Skip characters within a "$(" comment.
; "$)" ends, but must be whitespace separated.
defun read-comment (&optional (stream *standard-input*))
  iter
     consume-whitespace stream
     for c = peek-char(nil stream nil nil)
     while c
     if eq(c #\$)
       if eq(read-token(stream) '|$)|) finish()
       read-char stream


defun process-metamath-file (&optional (stream *standard-input*))
  format t "process-metamath-file.~%"
  iter
    for tok next read-token(stream)
    while tok
    print tok
    cond
      eq(tok '|$(|) read-comment(stream)
      t princ(tok)
  ; do
  ;   ((tok read-token(stream) read-token(stream)))
  ;   ((eq tok 'end) nil)
  ;   princ tok


format t "Starting.~%"
process-metamath-file()
format t "Ending.~%"

; End of file, restore readtable.
(readable:disable-readable)
