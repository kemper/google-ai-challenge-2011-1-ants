#!/usr/bin/sbcl --script

(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
;(declaim (sb-ext:muffle-conditions style-warning))  ; doesn't work
;(declaim (sb-ext:muffle-conditions warning))        ; idem
(setf sb-ext:*muffled-warnings* 'style-warning)

(require :asdf)

(load (merge-pathnames "3rd-party/asdf-init.lisp" *default-pathname-defaults*))
(asdf:oos 'asdf:load-op :ants-bot)
(save-lisp-and-die "MyBot" :toplevel #'ants-bot::main :executable t)
