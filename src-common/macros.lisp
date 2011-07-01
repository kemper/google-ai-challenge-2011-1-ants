;;;; macros.lisp

(in-package :ants-common)


;;; Macros

;; shortcut for (SLOT *STATE*)
;;
;; another possibility by pjb#lisp: (define-symbol-macro @turn (turn *state*))
(set-macro-character #\@
  (lambda (stream char)
    (declare (ignore char))
    (let ((obj (read stream nil (values) t)))
      `(,obj *state*))))
