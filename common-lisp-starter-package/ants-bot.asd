;;;; ants-bot.asd

(in-package :cl-user)

(asdf:defsystem :ants-bot
  :components ((:module src
                :serial t
                :components ((:file "package")
                             (:file "classes")
                             (:file "misc")
                             (:file "game-state")
                             (:file "main"))))
  :depends-on (:ants-common :usocket))
