;;;; ants-bot.lisp

(in-package :ants-bot)


;;; Functions

;; This is the actual 'AI'.  Very simple currently: loops through each of your
;; ants and issues an order to go either north, east, south or west if the tile
;; in the direction is not a water tile.
(defun do-turn ()
  (logmsg "[do-turn] " (length (my-ants *state*)) " ants~%")
  (loop for ant in (reverse (my-ants *state*))
        for row = (row ant)
        for col = (col ant)
        do (loop for dir in '(:north :east :south :west)
                 for new-location = (new-location row col dir)
                 for nlrow = (elt new-location 0)
                 for nlcol = (elt new-location 1)
                 do (unless (waterp (tile-at nlrow nlcol))
                      (issue-order row col dir)
                      (loop-finish)))))


;;; Main Program

;; This MAIN is used on the competition server.
(defun main (&key (log nil) (state (make-instance 'ants-bot-state))
                  (verbose nil))
  "Main game loop: parses the (initial) game state and calls DO-TURN and
  FINISH-TURN."
  (let ((*state* state)
        (*verbose* verbose))
    (cond ((and log *verbose*)
           (setf (slot-value *state* 'log-stream)
                 (open log :direction :output :if-exists :append
                           :if-does-not-exist :create)))
          (*verbose*
           (setf (slot-value *state* 'log-stream) *debug-io*)))
    (logmsg "~&=== New Match: " (current-date-time-string) " ===~%")
    (handler-bind ((sb-sys:interactive-interrupt #'user-interrupt))
      (loop while (handler-case (peek-char nil (input *state*) nil)
                    (sb-int:simple-stream-error nil))
            for end-of-game-p = (parse-game-state)
            when end-of-game-p do (loop-finish)
            do (logmsg "--- turn: " (turn *state*) " ---~%")
               (logmsg "~&[start] " (current-date-time-string) "~%")
               (do-turn)
               (finish-turn)
               (logmsg "~&[end] move took " (turn-time-used) " seconds ("
                       (turn-time-remaining) " left).~%")))))


;; This MAIN is called when you use the Makefile locally.
(defun main-for-local (&key (log "ants-bot.log") (verbose t))
  (main :log log :verbose verbose))


;; This MAIN is for the Slime REPL with bin/play-proxy-game.sh.
(defun main-for-proxybot (&key (log "ants-bot-proxied.log") (verbose t)
                          (host #-allegro #(127 0 0 1) #+allegro "localhost")
                          (port 41807))
  (let (client socket stream)
    (unwind-protect
         (handler-bind (#+sbcl (sb-bsd-sockets:socket-error
                                #'socket-error-handler)
                        (address-in-use-error #'address-in-use))
           (setf socket (socket-listen host port :reuse-address t))
           (format *debug-io* "Waiting for connection on ~A:~D...~%"
                   (host2str host)  port)
           (force-output)
           (setf client (socket-accept socket)
                 stream (socket-stream client))
           (format *debug-io* "Connected. Playing game...~%")
           (force-output)
           (main :state (make-instance 'ants-bot-state :input stream
                                       :output stream)
                 :log log :verbose verbose))
      (ignore-errors (socket-close client)
                     (socket-close socket))))
  (format *debug-io* "Game finished. Connection closed...~%"))
