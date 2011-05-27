;;;; play-game.lisp
;;;;
;;;; This is really a hack job currently.
;;;;
;;;; Let me see if I can get this right: (bot-id bot) is assigned in the order
;;;; of how they appear on the commandline by looping through (remainder)
;;;; and calling (unique-bot-id) each time a bot class is instatiated.
;;;;
;;;; The initial ants / "ant tiles", however, are assigned IDs in the order
;;;; they appear on the map (by going through it from left-to-right, top-to-
;;;; bottom).  So the alphabetic character used on the map to signify an
;;;; ant tile has NO bearing on the ID it will get.  These are only used to
;;;; differentiate between ant tiles.
;;;;
;;;; This is all in the name of compatibility with playgame.py.

(in-package :play-game)


;;; Constants

(defvar +land+ (make-instance 'land))
(defvar +water+ (make-instance 'water))


;;; Functions

(defun ants-within-attack-range ()
  (loop with all = (loop for row from 0 below (rows *state*)
                     append (loop for col from 0 below (cols *state*)
                                  for tile = (aref (game-map *state*) row col)
                                  when (and (typep tile 'ant)
                                            (not (dead tile)))
                                    collect tile))
        for ant-a in all
        for aid = (pid ant-a)
        for arow = (row ant-a)
        for acol = (col ant-a)
        append (loop for ant-b in (remove ant-a all)
                     for bid = (pid ant-b)
                     for brow = (row ant-b)
                     for bcol = (col ant-b)
                     for dist = (distance arow acol brow bcol)
                     when (and (/= aid bid)
                               (<= dist (sqrt (attack-radius2 *state*))))
                       collect (list :a-row arow :a-col acol
                                     :b-row brow :b-col bcol
                                     :distance dist))
          into result
        finally (return (sort result #'< :key (lambda (plist)
                                                (getf plist :distance))))))


;; TODO implement battle resolution from:
;; http://github.com/aichallenge/aichallenge/wiki/Ants-Game-Specification
(defun battle-resolution ()
  ;(loop for awar = (ants-within-attack-range)
  ;      for battle = (first awar)
  ;      while battle
  ;      for arow = (getf battle :a-row)
  ;      for acol = (getf battle :a-col)
  ;      for aid = (when battle (- (aref (game-map *state*) arow acol) 99))
  ;      for brow = (getf battle :b-row)
  ;      for bcol = (getf battle :b-col)
  ;      for bid = (when battle (- (aref (game-map *state*) brow bcol) 99))
  ;      do (logmsg "Ants " aid ":" arow ":" acol " and " bid ":" brow ":" bcol
  ;                 " fought...~%")
  ;         ;; TODO implement proper battle resolution
  ;         (setf (aref (game-map *state*) arow acol) (+ aid 199)
  ;               (aref (game-map *state*) brow bcol) (+ bid 199)))
  )


(let ((mapping (make-array 0 :fill-pointer 0 :element-type 'character)))
  (defun char2pid (char)
    (unless (find char mapping)
      (vector-push-extend char mapping))
    (position char mapping)))


;; TODO fix inefficient algorithm
(defun check-collisions ()
  (loop for order-a in (copy-seq (orders *state*))
        for bot-a-id = (getf order-a :bot-id)
        for srow-a = (getf order-a :src-row)
        for scol-a = (getf order-a :src-col)
        for row-a = (getf order-a :dst-row)
        for col-a = (getf order-a :dst-col)
        do (loop for order-b in (remove order-a (copy-seq (orders *state*)))
                 for bot-b-id = (getf order-b :bot-id)
                 for srow-b = (getf order-b :src-row)
                 for scol-b = (getf order-b :src-col)
                 for row-b = (getf order-b :dst-row)
                 for col-b = (getf order-b :dst-col)
                 do (when (and (= row-a row-b)
                               (= col-a col-b))
                      (logmsg "Ants " bot-a-id ":" srow-a ":" scol-a " and "
                              bot-b-id ":" srow-b ":" scol-b " collided. "
                              "Killing ants...~%")
                      (setf (aref (game-map *state*) srow-a scol-a) +land+
                            (aref (game-map *state*) srow-b scol-b) +land+
                            (orders *state*) (remove order-b (remove order-a (orders *state*))))))))


(defun check-positions ()
  (loop for order in (copy-seq (orders *state*))
        for bot-id = (getf order :bot-id)
        for row = (getf order :src-row)
        for col = (getf order :src-col)
        do (when (/= bot-id (pid (aref (game-map *state*) row col)))
             (logmsg "Bot " bot-id " issued an order for a position it "
                     "doesn't occupy. Ignoring...~%")
             (logmsg "o: " order "~%")
             (logmsg "p: " (pid (aref (game-map *state*) row col)) "~%")
             (setf (orders *state*) (remove order (orders *state*))))))


(defun check-water ()
  (loop for order in (copy-seq (orders *state*))
        for bot-id = (getf order :bot-id)
        for row = (getf order :src-row)
        for col = (getf order :src-col)
        for dir = (getf order :direction)
        do (when (water? row col dir)
             (logmsg "Bot " bot-id " ordered an ant into water. Ignoring...~%")
             (setf (orders *state*) (remove order (orders *state*))))))


(defun clear-dead-ants ()
  (loop for row from 0 below (rows *state*)
        do (loop for col from 0 below (cols *state*)
                 for tile = (aref (game-map *state*) row col)
                 do (when (and (typep tile 'ant) (dead tile))
                      (setf (aref (game-map *state*) row col) +land+)))))


(defun dir2key (direction)
  (cond ((equal direction "N") :north)
        ((equal direction "E") :east)
        ((equal direction "S") :south)
        ((equal direction "W") :west)))


(defun do-turn (turn)
  (setf (orders *state*) nil)
  (loop for bot across (bots *state*)
        for command-line = (command-line bot)
        for id = (bid bot)
        for proc = (process bot)
        for pin = (sb-ext:process-input proc)
        for pout = (sb-ext:process-output proc)
        do  (if (equal :running (sb-ext:process-status proc))
                (send-game-state id pin turn)
                (logmsg id ":" command-line " has stopped running...~%"))
           ;; TODO don't do this when bot has stopped running
           (let ((turn-start (wall-time)))
             (wait-for-output pout turn-start)
             (loop ;while (wait-for-output pout start)  ; no return values
                   with end-loop = nil
                   until end-loop
                   do (cond ((no-turn-time-left-p turn-start)
                             (logmsg id ":" command-line " timed out.~%")
                             (setf end-loop t))
                            ((listen pout)
                             (let ((line (read-line pout nil)))
                               (when (starts-with line "go")
                                 (loop-finish))
                               (when (starts-with line "o ")
                                 (queue-ant-order id line))))))))
  (move-ants))


(defun init-scores-for-new-turn ()
  (loop for bot across (bots *state*)
        for prev-turn-score = (aref (scores bot) (- (turn *state*) 1))
        do (vector-push-extend prev-turn-score (scores bot))))


(defun key2dir (key)
  (cond ((equal key :north) #\n)
        ((equal key :east)  #\e)
        ((equal key :south) #\s)
        ((equal key :west)  #\w)))


(defun log-new-turn-stats ()
  (when *verbose*
    (format (log-stream *state*) "turn ~4D stats: ant_count: [~A]~%"
            (turn *state*) (players-ant-count-string :sep ", "))
    (force-output (log-stream *state*))))


(defun move-ants ()
  (clear-dead-ants)
  (check-positions)
  (check-water)
  (check-collisions)
  (loop for order in (orders *state*)
        for bot-id = (getf order :bot-id)
        for src-row = (getf order :src-row)
        for src-col = (getf order :src-col)
        for dst-row = (getf order :dst-row)
        for dst-col = (getf order :dst-col)
        for ant = (aref (game-map *state*) src-row src-col)
        do (vector-push-extend (key2dir (getf order :direction)) (orders ant))
           (setf (slot-value ant 'row) dst-row
                 (slot-value ant 'col) dst-col
                 (aref (game-map *state*) dst-row dst-col) ant
                 (aref (game-map *state*) src-row src-col) +land+))
  (battle-resolution)
  ;(spawn-ants)
  )


(defun no-turn-time-left-p (turn-start-time)
  (not (turn-time-left-p turn-start-time)))


(defun parse-map (file-name)
  (with-open-file (f file-name)
    (loop with rows = 0
          for line = (read-line f nil)
          while line
          do (cond ((starts-with line "cols ")
                    (setf (slot-value *state* 'cols) (par-value line)))
                   ((starts-with line "rows ")
                    (setf (slot-value *state* 'rows) (par-value line)))
                   ((starts-with line "players ")
                    (setf (slot-value *state* 'n-players) (par-value line)))
                   ((and (starts-with line "m ") (null (cols *state*)))
                    (logmsg "~&Map missing \"cols n\" line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (null (rows *state*)))
                    (logmsg "~&Map missing \"rows n\" line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (null (n-players *state*)))
                    (logmsg "~&Map missing \"players n\" line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ")
                         (< (length (remainder)) (n-players *state*)))
                    (logmsg "~&Map needs " (n-players *state*) " players but "
                            ;"only " (length (bots *state*)) " were entered on "
                            "only " (length (remainder)) " were entered on "
                            "the command-line. Aborting...~%")
                    (quit 1))
                   ((and (starts-with line "m ") (null (game-map *state*)))
                    (setf (slot-value *state* 'game-map)
                          (make-array (list (rows *state*) (cols *state*))
                                      :initial-element 'abc123))
                    (parse-map-line (game-map *state*) line rows)
                    (incf rows))
                   ((starts-with line "m ")
                    (parse-map-line (game-map *state*) line rows)
                    (incf rows)))
          finally (when (/= rows (rows *state*))
                    (logmsg "~&Actual map rows (" rows ") not equal to "
                            "specified number of rows (" (rows *state*)
                            "). Aborting...~%")
                    (quit 1)))))


(defun parse-map-line (map-array string row)
  (when (/= (- (length string) 2) (cols *state*))
    (logmsg "~&Actual map columns (" (- (length string) 2) ") for this line "
            "not equal to specified number of~%columns (" (cols *state*) ") "
            "for this map. Aborting...~%")
    (quit 1))
  (loop for c across (subseq string 2)
        for col from 0
        do (setf (aref map-array row col)
                 (case c
                   (#\. +land+)
                   (#\% +water+)
                   (otherwise
                    (let* ((pid (char2pid c))
                           (ant (make-instance 'ant :initial-row row
                                  :initial-col col :row row :col col :pid pid
                                  :start-turn (turn *state*))))
                      (push ant (slot-value (aref (bots *state*) pid) 'ants))
                      ant))))))


(defun play-game ()
  (loop for turn from 0 to (turns *state*)
        do (setf (slot-value *state* 'turn) turn)
           (log-new-turn-stats)
           (cond ((= turn 0) (send-initial-game-state))
                 ((> turn 0) (init-scores-for-new-turn)
                             ;; should be in DO-TURN?
                             ;(spawn-food)  ; TODO turn on eventually
                             (do-turn turn)
                             (update-immobile-ant-orders)))))


(defun players-ant-count-string (&key (sep " "))
  (with-output-to-string (s)
    (loop for bot across (bots *state*)
          for i from 1
          do (princ (length (ants bot)) s)
             (when (< i (length (bots *state*)))
               (princ sep s)))))


(defun players-score-string (&key (sep " "))
  (with-output-to-string (s)
    (loop for bot across (bots *state*)
          for i from 1
          do (princ (last1 (scores bot)) s)
             (when (< i (length (bots *state*)))
               (princ sep s)))))


(defun players-status-string (&key (sep " ") (quotes nil))
  (with-output-to-string (s)
    (loop for bot across (bots *state*)
          for i from 1
          do (when quotes (princ "\"" s))
             (princ (status bot) s)
             (when quotes (princ "\"" s))
             (when (< i (length (bots *state*)))
               (princ sep s)))))


(defun print-game-map (game-map &optional (stream *debug-io*))
  (loop with dim = (array-dimensions game-map)
        for col from 0 below (second dim)
        initially (princ #\space stream)
                  (princ #\space stream)
        do (princ (mod col 10) stream)
        finally (terpri stream))
  (loop with dim = (array-dimensions game-map)
        for row from 0 below (first dim)
        do (princ (mod row 10) stream)
           (princ #\space stream)
           (loop for col from 0 below (second dim)
                 for tile = (aref game-map row col)
                 for type = (type-of tile)
                 do (case type
                      (land  (princ #\. stream))
                      (water (princ #\% stream))
                      (food  (princ #\* stream))
                      (ant (if (dead tile)
                               (princ (code-char (+ (pid tile) 65)) stream)
                               (princ (code-char (+ (pid tile) 97)) stream)))
                      (t (princ #\? stream))))
           (terpri stream)))


(defun process-command-line-options ()
  (cond ((or (getopt :short-name "?") (getopt :short-name "h")
             (= 1 (length (cmdline))))
         (help)
         (quit))
        ;; use :SAVE-RUNTIME-OPTIONS to SAVE-LISP-AND-DIE if you want --version
        ((getopt :long-name "release")
         (format t "~&play-game (Ant Wars) version ~A~%" +version+)
         (quit)))
  (do-cmdline-options (option name value source)
    (setf source source)  ; to silence compilation warnings  TODO remove!
    (cond ((or (equal name "m") (equal name "map_file"))
           (setf (slot-value *state* 'map-file) value))
          ((or (equal name "r") (equal name "rounds"))
           (setf (slot-value *state* 'turns) (parse-integer value)))
          ((or (equal name "t") (equal name "turns"))
           (setf (slot-value *state* 'turns) (parse-integer value)))
          ((or (equal name "v") (equal name "verbose"))
           (setf *verbose* t))
          ((equal name "loadtime")
           (setf (slot-value *state* 'load-time) (parse-integer value)))
          ((equal name "turntime")
           (setf (slot-value *state* 'turn-time) (parse-integer value)))))
  (unless (map-file *state*)
    (help)
    (quit)))
  ;; (remainder) is processed in (start-bots)


(defun queue-ant-order (bot-id string)
  (let* ((split (split-sequence #\space (string-upcase string)))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (dir (dir2key (elt split 3)))
         (nl (new-location row col dir)))
    ; TODO check for illegal moves here
    (push (list :bot-id bot-id :src-row row :src-col col :dst-row (elt nl 0)
                :dst-col (elt nl 1) :direction dir)
          (orders *state*))))


(defun random-elt (sequence)
  "Returns a random element from SEQUENCE."
  (let ((length (length sequence)))
    (when (> length 0)
      (elt sequence (random length)))))


(defun run-program (program &optional (args nil))
  ;; If there's a space in PROGRAM assume it needs to be started with an
  ;; interpreter which needs a slightly different call to RUN-PROGRAM.
  ;; Perhaps just checking for "python", "ruby", etc. would be better...
  (if (find #\space program)
      (let ((split (split-sequence #\space program)))
        (sb-ext:run-program (car split) (cdr split) :search t :wait nil
                            :input :stream :output :stream))
      (sb-ext:run-program (merge-pathnames program) args :wait nil
                          :input :stream :output :stream)))


;; See: https://github.com/aichallenge/aichallenge/wiki/Ants-replay-format
(defun save-replay (&optional (round 0))
  (with-open-file (f (mkstr round ".replay") :direction :output
                     :if-exists :supersede)
    (format f (mkstr "{~%"
                     "    \"challenge\": \"ants\",~%"
                     "    \"game_id\": 0,~%"
                     "    \"location\": \"localhost\",~%"
                     "    \"player_info\": ["))
    (loop for i from 0 below (n-players *state*)
          do (princ "{}" f)
             (when (< i (- (n-players *state*) 1))
               (princ ", " f)))
    (format f (mkstr "],~%"
                     "    \"rank\": ["))
    (loop for i from 0 below (n-players *state*)
          do (princ "0" f)
             (when (< i (- (n-players *state*) 1))
               (princ ", " f)))
    (format f (mkstr "],~%"
                     "    \"replayformat\": \"json\",~%"
                     "    \"replaydata\": {~%"
                     "        \"revision\": 2,~%"
                     "        \"players\": " (n-players *state*) ",~%"
                     "        \"loadtime\": " (load-time *state*) ",~%"
                     "        \"turntime\": " (turn-time *state*) ",~%"
                     "        \"turns\": " (turns *state*) ",~%"
                     "        \"viewradius2\": " (view-radius2 *state*) ",~%"
                   "        \"attackradius2\": " (attack-radius2 *state*) ",~%"
                     "        \"spawnradius2\": " (spawn-radius2 *state*) ",~%"
                     "        \"map\": {~%"
                     "             \"rows\": " (rows *state*) ",~%"
                     "             \"cols\": " (cols *state*) ",~%"
                     "             \"data\": [~%"))
    (loop for row from 0 below (rows *state*)
          do (format f "                     \"")
             (loop for col from 0 below (cols *state*)
                   for tile = (aref (game-map *state*) row col)
                   for type = (type-of tile)
                   do (case type
                        (land  (princ #\. f))
                        (water (princ #\% f))
                        (food  (princ #\* f))
                        (ant (if (dead tile)
                                 (princ (code-char (+ (pid tile) 65)) f)
                                 (princ (code-char (+ (pid tile) 97)) f)))
                        (otherwise (error "Unknown tile type: ~S (~D,~D)"
                                          tile row col))))
             (if (< (+ row 1) (rows *state*))
                 (format f "\",~%")
                 (format f "\"~%")))
    (format f (mkstr "             ]~%"
                     "        },~%"
                     "        \"ants\": [~%"))
    (loop for bot across (bots *state*)
          for i from 1
          do (loop for ant in (append (ants bot) (dead-ants bot))
                   for j from 1
                   do (format f "            [ ~D, ~D, ~D, ~D, ~D, ~D, ~S ]~A~%"
                              (initial-row ant)
                              (initial-col ant)
                              0 ;(start-turn ant)
                              0
                              (if (dead ant)
                                  (end-turn ant)
                                  (+ (turns *state*) 1))
                              (pid ant)
                              (coerce (orders ant) 'string)
                              (if (or (< i (length (bots *state*)))
                                      (< j (length (ants bot))))
                                   ","
                                   ""))))
    (format f (mkstr "        ],~%"
                     "        \"scores\": [~%"))
    (loop for bot across (bots *state*)
          for i from 1
          do (princ "            [" f)
             (loop for n across (scores bot)
                   for j from 1
                   do (princ n f)
                      (when (< j (length (scores bot)))
                        (princ "," f)))
             (princ "]" f)
             (when (< i (length (bots *state*)))
               (princ "," f))
             (terpri f))
    (format f (mkstr "        ]~%"
                     "    },~%"
                     "    \"score\": [~A],~%"
                     "    \"status\": [~A]~%")
            (players-score-string :sep ", ")
            (players-status-string :sep ", " :quotes t))
    (format f (mkstr "}~%"))))


;; A hack to make to scores compatible with playgame.py.  (This is mainly
;; for debugging, running diffs and unit tests.)
(defun scores-compatibility-hack ()
  (loop for bot across (bots *state*)
        do (setf (aref (scores bot) 0) 0)))


;; TODO implement vision code
(defun send-game-state (bot-id input turn)
  (format input "turn ~D~%" turn)
  (loop for row from 0 below (rows *state*)
        do (loop for col from 0 below (cols *state*)
                 for tile = (aref (game-map *state*) row col)
                 for type = (type-of tile)
                 do (case type
                      (water (format input "w ~D ~D~%" row col))
                      (food  (format input "f ~D ~D~%" row col))
                      (ant (if (dead tile)
                               (format input "d ~D ~D ~D~%" row col
                                       (cond ((= bot-id (pid tile))
                                              0)
                                             ((< bot-id (pid tile))
                                              (pid tile))
                                             ((> bot-id (pid tile))
                                              (+ (pid tile) 1))))
                               (format input "a ~D ~D ~D~%" row col
                                       (cond ((= bot-id (pid tile))
                                              0)
                                             ((< bot-id (pid tile))
                                              (pid tile))
                                             ((> bot-id (pid tile))
                                              (+ (pid tile) 1)))))))))
  (format input "go~%")
  (force-output input))


(defun send-initial-game-state ()
  (loop for bot across (bots *state*)
        for command-line = (command-line bot)
        for id = (bid bot)
        for proc = (process bot)
        for pin = (sb-ext:process-input proc)
        for pout = (sb-ext:process-output proc)
        do (if (equal :running (sb-ext:process-status proc))
               (progn
                 (format pin "turn 0~%loadtime ~D~%turntime ~D~%rows ~D~%cols ~D~%turns ~D~%viewradius2 ~D~%attackradius2 ~D~%spawnradius2 ~D~%ready~%"
                         (load-time *state*) (turn-time *state*) (rows *state*)
                         (cols *state*) (turns *state*)
                         (view-radius2 *state*)
                         (attack-radius2 *state*)
                         (spawn-radius2 *state*))
                 (force-output pin))
               (logmsg id ":" command-line " has stopped running...~%"))
           ;; TODO don't do this when bot has stopped running
           (let ((turn-start (wall-time)))
             (wait-for-output pout turn-start)
             (cond ((no-turn-time-left-p turn-start)
                    (logmsg id ":" command-line " timed out.~%"))
                   ((listen pout)
                    (let ((line (read-line pout nil)))
                      (unless (starts-with line "go")
                        (logmsg id ":" command-line " sent junk: " line
                                "~%"))))))))


;; TODO very innefficient: fix
(defun spawn-ants ()
  (let ((ants (loop for row from 0 below (rows *state*)
                    append (loop for col from 0 below (cols *state*)
                                 for tile = (aref (game-map *state*) row col)
                                 when (and (typep tile 'ant)
                                           (not (dead tile)))
                                   collect tile)))
        (foods (loop for row from 0 below (rows *state*)
                     append (loop for col from 0 below (cols *state*)
                                  for tile = (aref (game-map *state*) row col)
                                  when (typep tile 'food)
                                    collect tile))))
    (loop for food in foods
          for frow = (row food)
          for fcol = (col food)
          for nearby-ant-ids = nil
          do (loop for ant in ants
                   for aid = (pid ant)
                   for arow = (row ant)
                   for acol = (col ant)
                   do (when (<= (distance frow fcol arow acol)
                                (sqrt (spawn-radius2 *state*)))
                        (pushnew aid nearby-ant-ids))
                   finally (cond ((= 1 (length nearby-ant-ids))
                                  ;(logmsg "Spawning new ant: "
                                  ;        (first nearby-ant-ids) "~%")
                                  (incf (aref (scores (aref (bots *state*)
                                                       (first nearby-ant-ids)))
                                              (turn *state*)))
                                  ;; TODO needs to spawn ant with correct
                                  ;; start- and conversion-turns
                                  (let ((ant (make-instance 'ant :row frow
                                               :col fcol
                                               :pid (first nearby-ant-ids)
                                               :start-turn (turn *state*))))
                                    (push ant
                                          (slot-value (aref (bots *state*)
                                                        (first nearby-ant-ids))
                                                      'ants))
                                    (setf (aref (game-map *state*) frow fcol)
                                          ant)))
                                 ((> (length nearby-ant-ids) 1)
                                  ;(logmsg "Multiple contestants for food at "
                                  ;        frow ":" fcol ". Removing...~%")
                                  (setf (aref (game-map *state*) frow fcol)
                                        +land+)))))))


;; Very simple random food spawning.  Collects all land tile's coordinates
;; and randomly picks a number of tiles equal to the number of players.
(defun spawn-food ()
  (let ((food (loop for row from 0 below (rows *state*)
                    append (loop for col from 0 below (cols *state*)
                                 for tile = (aref (game-map *state*) row col)
                                 when (typep tile 'land)
                                   collect (list row col))
                      into result
                    finally (return (loop repeat (n-players *state*)
                                          collect (random-elt result))))))
    (loop for rc in food
          for row = (elt rc 0)
          for col = (elt rc 1)
          do (setf (aref (game-map *state*) row col)
                   (make-instance 'food :row row :col col
                                  :start-turn (turn *state*))))))


(defun start-bots ()
  (loop for command-line in (remainder)
        for proc = (run-program command-line)
        for bot = (make-instance 'bot :command-line command-line :process proc)
        do (vector-push-extend bot (bots *state*))))


(defun turn-time-left-p (turn-start-time)
  (<= (- (wall-time) turn-start-time)
      (/ (turn-time *state*) 1000)))


(defun update-immobile-ant-orders ()
  (loop for bot across (bots *state*)
        do (loop for ant in (ants bot)
                 for orders = (orders ant)
                 when (< (length orders) (turn *state*))
                 do (vector-push-extend #\- orders))))


(defun wait-for-output (output turn-start-time)
  (loop until (or (listen output)
                  (no-turn-time-left-p turn-start-time))))


(defun water? (row col direction)
  (let ((nl (new-location row col direction)))
    (typep (aref (game-map *state*) (elt nl 0) (elt nl 1)) 'water)))


;;; Main Program

(defsynopsis (:postfix "BOT1 BOT2 .. BOTn")
  (text :contents "Game engine for Ant Wars.
")
  (group (:header "Game options:")
    (path :short-name "m" :long-name "map_file" :argument-name "MAP"
          :type :file :description "Name of the map file. (required)")
    (stropt :short-name "r" :long-name "rounds" :argument-name "ROUNDS"
            :default-value "200"
            :description "Number of rounds to play. (synonym for --turns)")
    (stropt :short-name "t" :long-name "turns" :argument-name "TURNS"
            :default-value "200"
            :description "Number of turns in the game.")
    (path :short-name "o" :long-name "output_dir" :argument-name "OUTPUT_DIR"
          :type :directory :default-value #p"replays/"
          :description "Directory to dump replay files to. (defunct)")
    (flag :long-name "serial"
      :description "Run bots in serial, instead of parallel. (defunct)")
    (stropt :long-name "loadtime" :argument-name "LOADTIME"
            :default-value "3000"
            :description "Amount of time to give for load, in milliseconds.")
    (stropt :long-name "turntime" :argument-name "TURNTIME"
            :default-value "1000"
            :description "Amount of time to give each bot, in milliseconds.")
    (stropt :long-name "attack" :argument-name "ATTACK"
            :description "Attack method to use for engine. (ignored)"))
  (group (:header "Debug options:")
    (flag :short-name "v" :long-name "verbose"
      :description "Print out status as game goes.")
    (flag :short-name "I" :long-name "log_input"
      :description "Log input streams sent to bots. (defunct)")
    (flag :short-name "O" :long-name "log_output"
      :description "Log output streams from bots. (defunct)"))
  (group (:header "Immediate exit options:")
    (flag :short-name "?"
      :description "Print this help and exit.")
    (flag :short-name "h" :long-name "help"
      :description "Print this help and exit.")
    (flag :long-name "release"
	  :description "Print version/release number and exit.")))


(defun debug-output ()
  (logmsg "===============================================================~%")
  (describe *state*)
  (loop for bot across (bots *state*)
        do (describe bot))
  (logmsg "===============================================================~%"))


(defun main ()
  (make-context)
  (let ((*state* (make-instance 'play-game-state :error-stream *debug-io*))
        (*verbose* nil))
    (process-command-line-options)
    (start-bots)
    (parse-map (map-file *state*))
    (logmsg "running for " (turns *state*) " turns~%")
    (handler-bind (#+sbcl (sb-sys:interactive-interrupt #'user-interrupt))
                   ;(error #'error-handler))
      (play-game))
    (logmsg "score " (players-score-string) "~%")
    (logmsg "status " (players-status-string) "~%")
    ;(debug-output)
    (scores-compatibility-hack)
    (save-replay)))
