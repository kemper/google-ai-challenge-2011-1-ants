;;;; package.lisp

(in-package :cl-user)

(defpackage :ants-common
  (:use :cl)
  (:export ;; specials
           :*state* :*verbose* :+land+
           ;; classes and accessors / readers
           :ant :food :land :state :water
           :attack-radius2 :col :cols :conversion-turn :dead :end-turn
           :enemy-ants :error-stream :food :game-map :initial-col :initial-row
           :input :last1 :load-time :log-stream :my-ants :orders :output :pid
           :row :rows :seen-by :spawn-radius2 :start-turn :turn
           :turn-start-time :turn-time :turns :view-radius2
           ;; handlers
           :address-in-use :connection-lost :connection-refused :error-handler
           :socket-error-handler :user-interrupt
           ;; functions
           :current-date-time-string :distance :errmsg :host2str :logmsg :mkstr
           :new-location :par-value :print-game-map :quit :starts-with
           :wall-time :water? :wrapped-row-col))
