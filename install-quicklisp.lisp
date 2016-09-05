(load "quicklisp.lisp")
(quicklisp-quickstart:install)  ; Install QuickLisp (also installs ASDF)
;
; This would install it to our init file, but there's no documented way
; to prevent waiting for an "Enter", so we'll force load later.
; (ql:add-to-init-file)           ; Add QuickLisp to your Lisp init file
