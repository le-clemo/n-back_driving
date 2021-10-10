;; Multitasking Fixed Choice Experiment, 2012
;; Copyright Menno Nijboer

(clear-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL EXPERIMENT VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A variable to help record movement times
(defvar *start-time* 0)

(defparameter *screen-h* 1024)
(defparameter *screen-w* 1200)
(defparameter *xC* (/ *screen-w* 2.0))
(defparameter *yC* (/ *screen-h* 2.0))

(defparameter *filepath* "~/Dropbox/Work/ACT-R/Models/Forcedcombi")

(defparameter *save-data* nil)

; Simulated participant performance
(defparameter *result-pp-nback-accuracy*        '( (N_ . nil) (NT . nil) (NC . nil)))
(defparameter *result-pp-nback-RT*              '( (N_ . nil) (NT . nil) (NC . nil)))
(defparameter *result-pp-tracking-accuracy*     '( (T_ . nil) (NT . nil) (CT . nil)))
(defparameter *result-pp-counting-accuracy*     '( (C_ . nil) (NC . nil) (CT . nil)))
(defparameter *result-pp-counting-errordist*    '( (C_ . nil) (NC . nil) (CT . nil)))

;; Global performance history
(defparameter *result-nback-accuracy* nil)
(defparameter *result-nback-RT* nil)
(defparameter *result-tracking-accuracy* nil)
(defparameter *result-counting-accuracy* nil)
(defparameter *result-counting-errordist* nil)

; Experiment data
(defparameter *data-nback-accuracy*     '( (N_ . 0) (NT . 0) (NC . 0)))
(defparameter *data-nback-RT*           '( (N_ . 0) (NT . 0) (NC . 0)))
(defparameter *data-tracking-accuracy*  '( (T_ . 0) (NT . 0) (CT . 0)))
(defparameter *data-counting-accuracy*  '( (C_ . 0) (NC . 0) (CT . 0)))
(defparameter *data-counting-errordist* '( (C_ . 0) (NC . 0) (CT . 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEVICE AND ITEM CLASSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; different screen displays
(defconstant +N+ 0)
(defconstant +T+ 1)
(defconstant +C+ 2)

; all task configurations
(defconstant +N_+ '(1 0 0))
(defconstant +T_+ '(0 1 0))
(defconstant +C_+ '(0 0 1))
(defconstant +NT+ '(1 1 0))
(defconstant +NC+ '(1 0 1))
(defconstant +CT+ '(0 1 1))
(defconstant +FX+ '(0 0 0))

(defclass exp-screen ()
    (
    ; display
    (active-tasks
      :initarg :active-tasks
      :initform '(0 1 0)
      :accessor active-tasks)

    ; NBACK (1 0 0)
    (nback-stimulus
      :initarg :nback-stimulus
      :initform nil
      :accessor nback-stimulus)
    (nback-feedback
      :initarg :nback-feedback
      :initform nil
      :accessor nback-feedback)

    ; TRACKING (0 1 0)
    (tracking-target
      :initarg :tracking-target
      :initform nil
      :accessor tracking-target)
    (tracking-circle
      :initarg :tracking-circle
      :initform nil
      :accessor tracking-circle)
    (tracking-bounds
      :initarg :tracking-bounds
      :initform nil
      :accessor tracking-bounds)

    ; TONE COUNTING (0 0 1)
    (counting-fixation
      :initarg :counting-fixation
      :initform nil
      :accessor counting-fixation)
    (counting-respond
      :initarg :counting-respond
      :initform nil
      :accessor counting-respond)
    (counting-feedback
      :initarg :counting-feedback
      :initform nil
      :accessor counting-feedback)

    ; Inter-trial items
    (fixation-cross
      :initarg :fixation-cross
      :initform nil
      :accessor fixation-cross)

    (trial-info
      :initarg :trial-info
      :initform nil
      :accessor trial-info)

    )

)

(defparameter *exp-device* (make-instance 'exp-screen))

; some accessors for convenience
(defun tracking-circle-loc ()
    (car (tracking-circle *exp-device*))
)
(defun tracking-target-loc ()
    (car (tracking-target *exp-device*))
)

(defun nback-stimulus-loc ()
    (car (nback-stimulus *exp-device*))
)
(defun nback-stimulus-obj ()
    (cdr (nback-stimulus *exp-device*))
)
(defun nback-feedback-loc ()
    (car (nback-feedback *exp-device*))
)

(defmethod task-is-active (n)
    (= 1 (nth n (active-tasks *exp-device*)))
)

(defmethod num-active-tasks ()
    (sum (active-tasks *exp-device*))
)

; where the magic happens: produces the set of items currently visible, depending on the active tasks
(defmethod compile-items ((device exp-screen) vis-loc)
    (let* (compiled)
        (with-accessors ((active active-tasks)) device 

            (when (task-is-active +N+) ; NBACK
                (with-accessors ((nb-stim   nback-stimulus)
                                 (nb-fb     nback-feedback)) device
                        (setf compiled (append compiled (list nb-stim nb-fb))) ; objects
                )
            )

            (when (task-is-active +T+) ; TRACKING
                (with-accessors ((tr-target tracking-target)
                                 (tr-circle    tracking-circle)
                                 (tr-bounds tracking-bounds)) device
                        (setf compiled (append compiled (list tr-target tr-circle))) ; objects
                )
            )

            (when (task-is-active +C+) ; TONE COUNTING
                (with-accessors ((co-fix    counting-fixation)
                                 (co-resp   counting-respond)
                                 (co-fb     counting-feedback)) device
                        (setf compiled (append compiled (list co-fix co-resp co-fb))) ; objects
                )
            )

            ; inter-trial items
            (with-accessors ((fix-cross    fixation-cross)
                             (trial-inf    trial-info)) device
                (setf compiled (append compiled (list fix-cross trial-inf)))
            )
        )

        (if (not vis-loc)
            (mapcar 'car compiled) ; just return the visual locations
            (cdr (assoc vis-loc compiled)) ; search in the list for the object(s)
        )
    ) 
)

(defun device-reset ()

    (setf (nback-stimulus *exp-device*) nil)
    (setf (nback-feedback *exp-device*) nil)
    (setf (tracking-target *exp-device*) nil)
    (setf (tracking-circle *exp-device*) nil)
    (setf (tracking-bounds *exp-device*) nil)
    (setf (counting-fixation *exp-device*) nil)
    (setf (counting-respond *exp-device*) nil)
    (setf (counting-feedback *exp-device*) nil)
    (setf (counting-feedback *exp-device*) nil)
    (setf (fixation-cross *exp-device*) nil)
    (setf (trial-info *exp-device*) nil)
)

(defun result-reset ()

    (setf *result-nback-accuracy*       (list (cons 'N_ nil) (cons 'NT nil) (cons 'NC nil)))
    (setf *result-nback-RT*             (list (cons 'N_ nil) (cons 'NT nil) (cons 'NC nil)))
    (setf *result-tracking-accuracy*    (list (cons 'T_ nil) (cons 'NT nil) (cons 'CT nil)))
    (setf *result-counting-accuracy*    (list (cons 'C_ nil) (cons 'NC nil) (cons 'CT nil)))
    (setf *result-counting-errordist*   (list (cons 'C_ nil) (cons 'NC nil) (cons 'CT nil)))
)

;;; DEVICE TO VISICON METHODS

(defmethod build-vis-locs-for ((device exp-screen) vis-mod)
    ;; just return the cars from all the sublists
    (compile-items *exp-device* nil)
)

(defmethod vis-loc-to-obj ((device exp-screen) vis-loc)
    ;; here we're just returning the pregenerated object from the list
    (compile-items *exp-device* vis-loc)
)

;(append (list (car (expScreen-trackingDot device))) (mapcar 'car (expScreen-allItems device)))
;(cdr (assoc vis-loc (append (list (expScreen-trackingDot device)) (expScreen-allItems device))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MODEL OUTPUT HANDLERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod device-move-cursor-to ((device exp-screen) loc)
  ;; ignore model's mouse move requests
  nil)

(defmethod get-mouse-coordinates ((device exp-screen))
  ;; always return the same location for the mouse
  (vector 0 0))

(defmethod device-handle-click ((device exp-screen))
  ;; ignore a mouse click
 nil)

(defmethod device-handle-keypress ((device exp-screen) key)
    (nback-input key)
    (tracking-input key)
    (counting-input key)
)

(defmethod device-speak-string ((device exp-screen) string)
    ;(counting-input string)
)


(defmethod cursor-to-vis-loc ((device exp-screen))
  nil)

;;; Still ignoring the mouse cursor for visual processing.

(defmethod cursor-to-vis-loc (device)
    nil
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXPERIMENT TASK FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *model-run-len* 40.0)
(defparameter *trial-order* nil)
(defparameter *current-trial* -1)
(defparameter *in-trial* nil)
(defparameter *trial-start-t* 0.0)

(defparameter *trial-duration* 30.0)
(defparameter *fixation-duration* 15.0)
(defparameter *info-duration* 2.0)
(defparameter *update-freq* (/ 1 20))


(defun trial-end-time ()
    (+ *trial-start-t* *trial-duration*)
)

(defun device-update-tasks ()
    ;(get-time t)

    (when *in-trial*
        (let* (
            (exp-t (mp-time))
            (trial-t (mp-trial-time))
            (done)
            )
            
            (cond ((> trial-t *trial-duration*) ; done with trial
                (cond ((task-is-active +C+) ; counting requires some end of trial stuff to be done
                    (when (not *counting-finished*)

                        (nback-unload)
                        (nback-save-trial-result)
                        (tracking-unload)
                        (tracking-save-trial-result)

                        (setf (active-tasks *exp-device*) '(0 0 1)) ; only counting is active
                        (counting-finish-trial)
                    )

                    (when *counting-concluded*
                        (counting-save-trial-result)
                        (setf done t)
                    )

                    (proc-display)
                )
                (t ; else
                    (nback-save-trial-result)
                    (tracking-save-trial-result)

                    (setf done t)
                ))
            ))

            (cond (done ; move to the next trial

                (setf *in-trial* nil)
                ;(schedule-break-relative 0)
                ;(run 2.0) ; finish up production firing
                ;(device-next-trial)
               (remove-all-m-buffer-chunks 'goal)
               (goal-focus nback-stop)
               ;(goal-focus tracking-stop)
               (tracking-stop)
               (goal-focus counting-stop)
              ;(setf (meta-p-events (current-mp)) nil)
    ;(schedule-periodic-event *update-freq* 'device-update-tasks)
                (trial-cooldown)

                (when *save-buffer-trace*
                    (write-fmri-results)
                )
            )
            (t ; update active tasks when the time limit hasn't been reached

                (nback-do-update exp-t)
                (tracking-do-update exp-t)
                (counting-do-update exp-t)

                (proc-display)
            ))
        )
    )
)

(defun trial-cooldown ()

    (schedule-event (+ (mp-time) 2) 'device-next-trial)
)

; setup the next trial
(defun device-next-trial ()

    (device-unload-tasks)
    ; ACT-R: clear the buffers for the next trial
    (hook-empty-buffers)

    (incf *current-trial*)
    (when (eq (device-trial-type) 'FX)
        ; skip fixation trials
        (incf *current-trial*)
    )

    (cond ((< *current-trial* (length *trial-order*))

        (if *debug* (format t "~%~%## Preparing next trial"))

        (setf (active-tasks *exp-device*) (nth *current-trial* *trial-order*))

       (fixation-show)
;(print "RUN NEXT")
        ;(run *model-run-len*)
    )
    (t ; else: finish the experiment

        (if *debug* (format t "~%~%## Done~%~%"))

        (proc-display)
        (schedule-break-relative 0)
    ))
)

(defun device-save-trial ()
)

(defun device-load-tasks ()

    (nback-load)
    (tracking-load)
    (counting-load)
)

(defun device-unload-tasks ()

    (nback-unload)
    (tracking-unload)
    (counting-unload)
)

(defun device-trial-type ()
    (let* (
        (current-trial (nth *current-trial* *trial-order*))
        )

        (cond
            ((equal current-trial +N_+) 'N_)
            ((equal current-trial +T_+) 'T_)
            ((equal current-trial +C_+) 'C_)
            ((equal current-trial +NT+) 'NT)
            ((equal current-trial +NC+) 'NC)
            ((equal current-trial +CT+) 'CT)
            (t 'FX)
        )
    )
)

(defun gather-pp-results ()
    (let* (
        (nback-con '(N_ NT NC))
        (tracking-con '(T_ NT CT))
        (counting-con '(C_ NC CT))
        )
        (if *debug-sim* (format t "~%NBACK~%"))
        (if *debug-sim* (format t "Condition   Performance    RT~%"))
        (loop for con in nback-con do
            (let* (
                (prev-trials-perf (assoc-vals con *result-nback-accuracy*))
                (prev-trials-RT (assoc-vals con *result-nback-RT*))
                (acc (average (assoc-vals con *result-pp-nback-accuracy*)))
                (rt (average (assoc-vals con *result-pp-nback-RT*)))
                )
                (if *debug-sim* (if acc (format t "~a          ~a           ~a~%" con (round-to acc 2) (round-to rt 2))))
                (rplacd (assoc con *result-nback-accuracy*) (cons acc prev-trials-perf))
                (rplacd (assoc con *result-nback-RT*) (cons rt prev-trials-RT))
                (rplacd (assoc con *result-pp-nback-accuracy*) nil)
                (rplacd (assoc con *result-pp-nback-RT*) nil)
            )
        )

        (if *debug-sim* (format t "TRACKING~%"))
        (if *debug-sim* (format t "Condition   Accuracy~%"))
        (loop for con in tracking-con do
            (let* (
                (prev-trials-perf (assoc-vals con *result-tracking-accuracy*))
                (acc (average (assoc-vals con *result-pp-tracking-accuracy*)))
                )
                (if *debug-sim* (if acc (format t "~a          ~a~%" con (round-to acc 2))))
                (rplacd (assoc con *result-tracking-accuracy*) (cons acc prev-trials-perf))
                (rplacd (assoc con *result-pp-tracking-accuracy*) nil)
            )
        )

        (if *debug-sim* (format t "COUNTING~%"))
        (if *debug-sim* (format t "Condition   Accuracy     Error distance~%"))
        (loop for con in counting-con do
            (let* (
                (prev-trials-perf (assoc-vals con *result-counting-accuracy*))
                (prev-trials-err (assoc-vals con *result-counting-errordist*))
                (acc (average (assoc-vals con *result-pp-counting-accuracy*)))
                (err (average (assoc-vals con *result-pp-counting-errordist*)))
                )
                (if *debug-sim* (if acc (format t "~a          ~a           ~a~%" con (round-to acc 2) (round-to err 2))))
                (rplacd (assoc con *result-counting-accuracy*) (cons acc prev-trials-perf))
                (rplacd (assoc con *result-counting-errordist*) (cons err prev-trials-err))
                (rplacd (assoc con *result-pp-counting-accuracy*) nil)
                (rplacd (assoc con *result-pp-counting-errordist*) nil)
            )
        )
    )
)

(defun report-results ()

    (setf *data-nback-accuracy*       (list (cons 'N_ 0.91) (cons 'NT 0.83) (cons 'NC 0.76)))
    (setf *data-nback-RT*       (list (cons 'N_ 0.66) (cons 'NT 0.78) (cons 'NC 0.71)))
    (setf *data-tracking-accuracy*       (list (cons 'T_ 0.95) (cons 'NT 0.77) (cons 'CT 0.93)))
    (setf *data-counting-accuracy*       (list (cons 'C_ 0.87) (cons 'CT 0.74) (cons 'NC 0.50)))
    (setf *data-counting-errordist*       (list (cons 'C_ 0.18) (cons 'CT 0.27) (cons 'NC 1.00)))

    (let* (
        (nback-con '(N_ NT NC))
        (tracking-con '(T_ NT CT))
        (counting-con '(C_ NC CT))
        ;(c_-acc (assoc-vals 'C_ *result-counting-accuracy*))
        )
        (format t "### NBACK #############################~%")
        (format t "Condition   Performance                  RT~%")
        (loop for con in nback-con do
            (format t "~a (~a)        [~a]  ~a (~a - ~a)         [~a]  ~a (~a - ~a)~%" con
             (length (remove nil (assoc-vals con *result-nback-accuracy*)))
             (assoc-vals con *data-nback-accuracy*)
             (round-to (average (assoc-vals con *result-nback-accuracy*)) 2)
             (round-to (conf-interval95 (assoc-vals con *result-nback-accuracy*) 'lower) 2)
             (round-to (conf-interval95 (assoc-vals con *result-nback-accuracy*) 'upper) 2)
             (assoc-vals con *data-nback-RT*)
             (round-to (average (assoc-vals con *result-nback-RT*)) 2)
             (round-to (conf-interval95 (assoc-vals con *result-nback-RT*) 'lower) 2)
             (round-to (conf-interval95 (assoc-vals con *result-nback-RT*) 'upper) 2))
        )

        (format t "### TRACKING ##########################~%")
        (format t "Condition   Accuracy~%")
        (loop for con in tracking-con do
            (format t "~a (~a)        [~a]  ~a (~a - ~a)~%" con
             (length (remove nil (assoc-vals con *result-tracking-accuracy*)))
             (assoc-vals con *data-tracking-accuracy*)
             (round-to (average (assoc-vals con *result-tracking-accuracy*)) 2) 
             (round-to (conf-interval95 (assoc-vals con *result-tracking-accuracy*) 'lower) 2)
             (round-to (conf-interval95 (assoc-vals con *result-tracking-accuracy*) 'upper) 2))
        )

        (format t "### COUNTING ##########################~%")
        (format t "Condition   Accuracy                     Error distance~%")
        (loop for con in counting-con do
            (format t "~a (~a)       [~a]  ~a (~a - ~a)         [~a]  ~a (~a - ~a)~%" con
             (length (remove nil (assoc-vals con *result-counting-accuracy*)))
             (assoc-vals con *data-counting-accuracy*)
             (round-to (average (assoc-vals con *result-counting-accuracy*)) 2)
             (round-to (conf-interval95 (assoc-vals con *result-counting-accuracy*) 'lower) 2)
             (round-to (conf-interval95 (assoc-vals con *result-counting-accuracy*) 'upper) 2)
             (assoc-vals con *data-counting-errordist*)
             (round-to (average (assoc-vals con *result-counting-errordist*)) 2)
             (round-to (conf-interval95 (assoc-vals con *result-counting-errordist*) 'lower) 2)
             (round-to (conf-interval95 (assoc-vals con *result-counting-errordist*) 'upper) 2))
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIXATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fixation-show ()

    (let* (
        (xC *xC*)
        (yC *yC*)
        (fix-loc (car (define-chunks-fct `((isa visual-location screen-x, xC screen-y, yC 
                                                kind, 'prefixation value, "+" height, 40 width, 40 color, 'black)))))
        (fix-obj (car (define-chunks (isa visual-object value "+" height 40 width 40 color black))))
        )

        ;(setf (fixation-cross *exp-device*) (cons fix-loc fix-obj))
        (schedule-event (+ (mp-time) *fixation-duration*) 'fixation-remove)

        (if *debug* (format t "~%~%## Presenting fixation cross"))

        (proc-display)
    )
)

(defun fixation-remove ()

    ;(setf (fixation-cross *exp-device*) nil)
    (trial-info-show)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRIAL INFORMATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trial-info-show ()

    (let* (
        (xC *xC*)
        (yC *yC*)
        (trial-type (string (device-trial-type)))
        (trial-loc (car (define-chunks-fct `((isa visual-location screen-x, xC screen-y, yC 
                                                kind, 'trialinfo value, trial-type height, 40 width, 40 color, 'black)))))
        (trial-obj (car (define-chunks-fct `((isa visual-object value, trial-type height, 40 width, 40 color, 'black)))))
        )

        (setf (trial-info *exp-device*) (cons trial-loc trial-obj))
        (schedule-event (+ (mp-time) *info-duration*) 'trial-info-do-trial)

        (if *debug* (format t "~%~%## Presenting trial info: ~a" trial-type))

    ;(clear-buffer 'goal)
   ;(remove-all-m-buffer-chunks 'goal)

    ; ACT-R: set new goal(s)
    (hook-set-goals)

        (proc-display)
    )

)

(defun trial-info-do-trial ()

    (setf (trial-info *exp-device*) nil)


    ;(print (goal-focus))

    (do-trial)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NBACK TASK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NBACK VARIABLES
(defparameter *nback-active* t)

(defparameter *nback-n* 2)
(defparameter *nback-trial-len* 29.5)
(defparameter *nback-char-set* (string "CHIKLQRSTW"))
(defparameter *nback-letter-set* (list "B" "D" "J" "I" "U" "V" "E" "F" "N"))
(defparameter *nback-history* nil)
(defparameter *nback-probability* (/ 1 3))
(defparameter *nback-isi* 1.5)
(defparameter *nback-stim-pres-len* 1.0)
(defparameter *nback-stim-resp-len* 1.5)
(defparameter *nback-xC* 0)

(defparameter *nback-stim-start-t* 0)
(defparameter *nback-stim* nil)
(defparameter *nback-in-isi* nil)

(defparameter *nback-sequence* nil)
(defparameter *nback-current-letter* 0)
(defparameter *nback-responded* nil)

;; performance
(defparameter *nback-performance* nil)
(defparameter *nback-RT* nil)

;; NBACK METHODS

(defmethod nback-load ()
    (when (task-is-active +N+)

        (let* (
            (xC (if (> (sum (active-tasks *exp-device*)) 1) (/ *xC* 2.0) *xC*)) ; nback is always LH
            (yC *yC*)
            ;(stim-loc (car (define-chunks-fct `((isa visual-location screen-x, xC screen-y, yC 
                                                    ;kind, 'text value, "A" height, 40 width, 30 color, 'black)))))
            ;(stim-obj (car (define-chunks (isa visual-object value "A" height 10 width 10 color black))))
            ;(feedback-loc (car (define-chunks-fct `((isa visual-location screen-x, xC screen-y, yC
                                                   ;kind, 'bigcircle value, 'bigcircle height, 50 width, 50 color, 'red)))))
            ;(feedback-obj (car (define-chunks (isa visual-object value bigcircle height 50 width 50 color red))))
            )

            ;(setf (nback-stimulus *exp-device*) (cons stim-loc stim-obj))
            ;(setf (nback-feedback *exp-device*) (cons feedback-loc feedback-obj))
            (setf *nback-xC* xC)
        )
    )

)

(defmethod nback-unload ()

    (setf (nback-stimulus *exp-device*) nil)
    (setf (nback-feedback *exp-device*) nil)
)

(defmethod nback-new-trial ()
    (setf *nback-history* nil)
    (setf *nback-performance* nil)
    (setf *nback-RT* nil)
    (setf *nback-sequence* nil)
    (setf *nback-current-letter* 0)
    (setf *nback-stim-start-t* 0)
    (setf *nback-stim* nil)
    (setf *nback-in-isi* nil)

    (setf (nback-feedback *exp-device*) nil)

    (if *debug* (format t "~%~%### NBACK - New trial"))

    (if *preload-trial-info*
        (setf *nback-sequence* (cdr (assoc (+ 1 *current-trial*) *preload-block-nback*))) ; use preloaded instead of generated
        (nback-generate-set)
    )
    (nback-new-stimulus)
)

(defmethod nback-show-isi ()
    (when (task-is-active +N+)
        ;(set-chunk-slot-value-fct (nback-stimulus-loc) 'value "")
        ;(set-chunk-slot-value-fct (nback-stimulus-obj) 'value "")

        (setf (nback-stimulus *exp-device*) nil)

        (if *debug* (format t "~%~%### NBACK - Showing ISI"))

    )
)

(defmethod nback-new-stimulus ()
    (when (task-is-active +N+)

        (setf *nback-responded* nil)
        (setf (nback-feedback *exp-device*) nil)

        (let* (
            ;(stim (random-elt *nback-char-set*))
            ;(len (length *nback-history*))
            (stim (nth *nback-current-letter* *nback-sequence*))
            )

            (setf *nback-current-letter* (1+ *nback-current-letter*))
            ;(if (and (< (act-r-random 1.0) *nback-probability*) (> len *nback-n*))
                ;(setf stim (nth *nback-n* *nback-history*)) ; target presentation
            ;)

            ;(print stim)
            ;(print (string stim))
            (setf *nback-history* (cons (string stim) *nback-history*)) ; the history is 'reversed'
            (setf *nback-stim-start-t* (mp-time));(get-time t))
            (setf *nback-stim* (string stim))

            (if *debug* (format t "~%~%### NBACK - New stimulus: ~a History: ~a (len ~a)" *nback-stim* *nback-history* (length *nback-history*)))
        )

        (let (
            (stim-loc (car (define-chunks-fct `((isa visual-location screen-x, *nback-xC* screen-y, *yC* 
                                                    kind, 'text value, *nback-stim* height, 40 width, 30 color, 'black)))))
            (stim-obj (car (define-chunks-fct `((isa text value, *nback-stim* height, 40 width, 30 color 'black)))))
            )

            (setf (nback-stimulus *exp-device*) (cons stim-loc stim-obj))
        )
        ;(set-chunk-slot-value-fct (nback-stimulus-loc) 'value *nback-stim*)
        ;(set-chunk-slot-value-fct (nback-stimulus-obj) 'value *nback-stim*)
    )
)

(defmethod nback-generate-set ()
    (let* ( ; generate new set
        (num-letters (round (/ *nback-trial-len* (+ *nback-stim-pres-len* *nback-isi*))))
        (nl (length *nback-letter-set*))
        (sets (- (ceiling (/ num-letters nl)) 1))
        (letters *nback-letter-set*)
        (letseq nil)
        (numNB (ceiling (* *nback-probability* num-letters)))
        )

        (loop for i from 0 to sets do
            (setf letters (append *nback-letter-set* letters))
        )

        (setf letseq (subseq (scramble letters) 0 num-letters))
        (if (> (act-r-random 1.0) 0.5) (setf numNB (+ numNB 1)))

        (setf nb-places (range 2 num-letters))

        (loop for i from 0 to numNB do
            (let* (
                (idx (random (length nb-places)))
                (let-idx (nth idx nb-places))
                )
                (setf (nth (- let-idx 2) letseq) (nth let-idx letseq))

                ; remove both options from the set
                (setf nb-places (remove-nth idx nb-places))

                (when (<= (+ let-idx 2) (length letseq))
                    (setf x (car (all-positions (+ let-idx 2) nb-places)))
                    (if x (setf nb-places (remove-nth x nb-places)))
                )

            )

        )

        ;(print letseq)
        (setf *nback-sequence* letseq)
    )

)

(defmethod nback-show-feedback (correct)
    (let* (
        (clr (if (= 1 correct) 'green 'red))
        (feedback-loc (car (define-chunks-fct `((isa visual-location screen-x, *nback-xC* screen-y, *yC*
                                               kind, 'bigcircle value, 'bigcircle height, 50 width, 50 color, clr)))))
        (feedback-obj (car (define-chunks (isa visual-object value bigcircle height 50 width 50 color clr))))
        )

;(format t "~a~%" clr)
        (if *debug* (format t "~%~%### NBACK - Show feedback: ~a" clr))

        (setf (nback-feedback *exp-device*) (cons feedback-loc feedback-obj))
    )

)

(defmethod nback-do-update (tt)
    (when (task-is-active +N+)
        (cond
            ((not *nback-in-isi*) ; currently showing the stimulus
                ;(format t "!!!! current time: ~a start-t: ~a end time: ~a" tt *nback-stim-start-t* (+ *nback-stim-start-t* *nback-stim-pres-len*))
                (when (> tt (+ *nback-stim-start-t* *nback-stim-pres-len*))
                    (when (and (> (length *nback-history*) (+ *nback-n* 1)) (not *nback-responded*))
                        ; no reply yet, count as wrong
                        (let* (
                            (current-stim (car *nback-history*))
                            (nback-stim (nth *nback-n* *nback-history*))
                            )

                            (if *debug* (format t "~%~%### NBACK - No response, counting as wrong"))

                            (if (string-equal current-stim nback-stim)
                                (nback-input 'r)
                                (nback-input 'e) ; otherwise
                            )
                        )
                    )

                    (if *debug* (format t "~%~%### NBACK - showing ISI at ~a" tt))

                    (setf *nback-in-isi* t)
                    (nback-show-isi)
                )
            )
            (t ; else (showing ISI)
                (when (> tt (+ *nback-stim-start-t* *nback-stim-pres-len* *nback-isi*))

                    (if *debug* (format t "~%~%### NBACK - showing new stimulus at ~a" tt))

                    (setf *nback-in-isi* nil)
                    (nback-new-stimulus)
                )
            )

        )
    )
)

(defmethod nback-input (key)
    (when (task-is-active +N+)

        (let* (
            (c (string key))
            (his-len (length *nback-history*))
            (current-stim (car *nback-history*))
            (nback-stim (nth *nback-n* *nback-history*))
            (RT (- (mp-time) *nback-stim-start-t*))
            (correct 0)
            )

            (when (and (not *nback-responded*) (or (string-equal c "e") (string-equal c "r")))

                (setf *nback-responded* t)

                (when (> his-len *nback-n*)
                    (cond
                        ((string-equal current-stim nback-stim)
                            (if (string-equal c "e")
                                (setf correct 1)
                            )
                        )
                        (t ; else
                            (if (string-equal c "r")
                                (setf correct 1)
                            )
                        )
                    )

                    (setf *nback-performance* (cons correct *nback-performance*))
                    (setf *nback-RT* (cons RT *nback-RT*))

                    (when *save-trial-result*
                        (write-to-file t *pp* "nbackpress" (format nil "~a ~a ~a ~a ~a" *pp* *block* *trial* his-len (mp-time)))
                        (write-to-file t 0 "nbackpress" (format nil "~a ~a ~a ~a ~a" *pp* *block* *trial* his-len (mp-time)))
                    )

                    (if *debug* (format t "~%~%### NBACK - Received input: ~a" c))

                    (nback-show-feedback correct)
                )

            )
        )
    )
)

(defun nback-save-trial-result ()
    (when (task-is-active +N+)

        (let* (
            (accuracy (nback-accuracy))
            (RT (average *nback-RT*))
            (trtp (device-trial-type))
            (prev-trials-perf (cdr (assoc trtp *result-pp-nback-accuracy*)))
            (prev-trials-RT (cdr (assoc trtp *result-pp-nback-RT*)))
            )

            (rplacd (assoc trtp *result-pp-nback-accuracy*) (cons accuracy prev-trials-perf))
            (rplacd (assoc trtp *result-pp-nback-RT*) (cons RT prev-trials-RT))

            (when *save-trial-result*
                (write-to-file t *pp* "nback" (format nil "~a ~a ~a ~a ~a ~a" *pp* *block* *trial* *trialtype* accuracy RT))
                (write-to-file t 0 "nback" (format nil "~a ~a ~a ~a ~a ~a" *pp* *block* *trial* *trialtype* accuracy RT))
            )
        )
    )
)

(defun nback-accuracy ()
    (float (my/ (sum *nback-performance*) (length *nback-performance*)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRACKING TASK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TRACKING VARIABLES

;(defparameter *tracking-active* t)

(defparameter *tracking-t* 0)
(defparameter *tracking-t-per-sec* (/ 20 18))
(defparameter *tracking-dt* (/ *tracking-t-per-sec* (/ 1 *update-freq*))) ; stepsize through the moving function
;(defparameter *tracking-dt* (/ 1 45)) ; stepsize through the moving function
(defparameter *tracking-update-freq* (/ 1 1))
(defparameter *tracking-xC* 0) ; center of the tracking task screen
(defparameter *tracking-move-dist* 15) ; distance covered per keypress
(defparameter *tracking-r* 20) ; max distance
(defparameter *tracking-target-loc* 0)
(defparameter *tracking-circle-loc* 0)

;; performance
(defparameter *tracking-insideSamples* 0)
(defparameter *tracking-outsideSamples* 0)

(defparameter *tracking-debug-int* 0.2)
(defparameter *tracking-debug-last-t* 0)

;; TRACKING METHODS

(defmethod tracking-load ()
    (when (task-is-active +T+)

        (let* (
            (xC (if (> (sum (active-tasks *exp-device*)) 1) (/ (+ *screen-w* *xC*) 2.0) *xC*)) ; tracking is always RH
            (yC *yC*)
            (yC-t (- yC 30))
            (circle-loc (car (define-chunks-fct `((isa visual-location screen-x, xC screen-y, yC-t 
                                                    kind, 'circle value, 'circle height, 10 width, 10 color, 'blue)))))
            (circle-obj (car (define-chunks (isa visual-object value circle height 10 width 10 color blue))))
            (target-loc (car (define-chunks-fct `((isa visual-location screen-x, xC screen-y, yC
                                                   kind, 'dot value, 'dot height, 4 width, 4 color, 'black)))))
            (target-obj (car (define-chunks (isa visual-object value dot height 4 width 4 color black))))
            )

;(print "loading tracking")
            (setf *tracking-xC* xC)
            ;(print (chunk-slot-value-fct dot-loc screen-x))
            (setf (tracking-target *exp-device*) (cons target-loc target-obj))
            (setf (tracking-circle *exp-device*) (cons circle-loc circle-obj))
        )
    )
)

(defmethod tracking-unload ()

    (setf (tracking-target *exp-device*) nil)
    (setf (tracking-circle *exp-device*) nil)
)

(defmethod tracking-new-trial ()
    (setf tt (act-r-random 300.0))
    (if *preload-trial-info* 
        (setf tt (float (cdr (assoc (+ 1 *current-trial*) *preload-block-tracking*))))
    ) ; preloaded trial info

    (let* (
        (tt (act-r-random 300.0))
        (offset (tracking-movement tt))
        (tPos (round-to (+ offset *tracking-xC*) 0))
        )


        (setf *tracking-t* tt)
        (setf *tracking-insideSamples* 0)
        (setf *tracking-outsideSamples* 0)

        (if *debug* (format t "~%~%### TRACKING - New trial"))

        (tracking-set-target-pos tPos)
        (tracking-set-circle-pos tPos)
    )
)

(defmethod tracking-movement (tt)
    (let* (
        (pi2 (* 2 pi))
        (dt *tracking-dt*)
        )

        ;(format t "forcing: ~4,4F , ~4,4F time: ~4,4F ~%" xOff yOff time)
        (setf *tracking-t* (+ tt dt))

        (+ (* 55 (sin (* pi2 0.05 tt))) (* 39 (sin (* pi2 0.2 tt))) (* 24 (sin (* pi2 0.08 tt))))
    )
)

(defmethod tracking-set-target-pos (pos)

  ;(print pos)
  ;(print "setting pos")
    (set-chunk-slot-value-fct (tracking-target-loc) 'screen-x pos)
    (setf *tracking-target-loc* pos)
    ;(set-chunk-slot-value-fct (car (tracking-dot *exp-device*) 'screen-y *yC*)

    ;(proc-display)
    ;(print-visicon)
    (model-update-tracked-target pos) ; replaces ACT-R tracking
)

(defmethod tracking-set-circle-pos (pos)

  ;(pint *tracking-dot-loc*)
    ;(set-chunk-slot-value-fct (car (tracking-dot *exp-device*)) 'screen-x pos)
    (set-chunk-slot-value-fct (tracking-circle-loc) 'screen-x pos)
    (setf *tracking-circle-loc* pos)

    ;(set-chunk-slot-value-fct *tracking-dot-loc* 'screen-x pos)
    ;(set-chunk-slot-value-fct *tracking-dot-loc* 'screen-y *yC*)

    ;(proc-display)
)

(defmethod tracking-input (key)
    (when (task-is-active +T+)

        (let* (
            (c (string key))
            (m *tracking-move-dist*)
            (circle-x (chunk-slot-value-fct (tracking-circle-loc) 'screen-x))
            (target-x (chunk-slot-value-fct (tracking-target-loc) 'screen-x))
            )

            (cond 
                ((string-equal c "u")
                    (setf circle-x (my- circle-x m)) 
                )
                ((string-equal c "i")
                    (setf circle-x (+ circle-x m)) 
                )
            )

            ;(if *debug* (format t "~%~%### TRACKING - distance: ~a limit: ~a" (abs (- target-x dot-x)) (* m 0.5)))
            (when (< (abs (my- target-x circle-x)) (* m 0.5))
                (if *debug* (format t "~%~%### TRACKING - snapping circle to target"))
                (setf circle-x target-x)
            )

            (if *debug* (format t "~%~%### TRACKING - Received input: ~a. Target at ~a, circle at ~a" c target-x circle-x))

            (tracking-set-circle-pos circle-x)

        )
    )
)

(defmethod tracking-do-update (tt)
    (when (task-is-active +T+)
        (let* (
            (offset (tracking-movement *tracking-t*))
            (tPos (round-to (+ offset *tracking-xC*) 0))
            (distance (tracking-distance))
            )

            (if (< distance *tracking-r*)
                (incf *tracking-insideSamples*)
                (incf *tracking-outsideSamples*)
            )

            (when (and *debug* (< (+ *tracking-debug-last-t* *tracking-debug-int*) tt))
                (setf circle-x (chunk-slot-value-fct (tracking-circle-loc) 'screen-x))
                (setf *tracking-debug-last-t* tt)
                (format t "~%~%### TRACKING - New position: target at ~a, circle at ~a, distance is ~a" tPos circle-x (tracking-distance)))

            (tracking-set-target-pos tPos)
        )
    )

    ;t ; suppress output
)

(defun tracking-stop ()
    (when (task-is-active +T+)

        (set-chunk-slot-value-fct (tracking-target-loc) 'color 'white)
        (set-chunk-slot-value-fct (tracking-circle-loc) 'color 'white)
    )
)

(defmethod tracking-distance ()
    (let* (
        (circle-x (chunk-slot-value-fct (tracking-circle-loc) 'screen-x))
        (target-x (chunk-slot-value-fct (tracking-target-loc) 'screen-x))
        )

        (sqrt (expt (my- circle-x target-x) 2))
    )
)

;(defun tracking-model-perceived-dist ()
    ;(if *debug* (format t "~%~%### TRACKING: Observed position: target at ~a, circle at ~a" *tracking-target-loc* *tracking-circle-loc*))
    ;(- *tracking-circle-loc* *tracking-target-loc*)
;)


(defmethod tracking-accuracy ()
    (if (> (+ *tracking-insideSamples* *tracking-outsideSamples*) 0)
        (/ *tracking-insideSamples* (+ *tracking-insideSamples* *tracking-outsideSamples*))
        0
    )
)

(defmethod tracking-print-state ()
    (let* (
        (circle-x (chunk-slot-value-fct (tracking-circle-loc) 'screen-x))
        (target-x (chunk-slot-value-fct (tracking-target-loc) 'screen-x))
        (distance (tracking-distance))
        (accuracy (tracking-accuracy))
        )

        (format t "Target: ~d Dot: ~d Distance: ~d Accuracy: ~$" target-x circle-x distance accuracy)
    )
)

(defun tracking-save-trial-result ()
    (when (task-is-active +T+)

        (let* (
            (accuracy (tracking-accuracy))
            (trtp (device-trial-type))
            (prev-trials (cdr (assoc trtp *result-pp-tracking-accuracy*)))
            )

            (rplacd (assoc trtp *result-pp-tracking-accuracy*) (cons accuracy prev-trials))

            (when *save-trial-result*
                (write-to-file t *pp* "tracking" (format nil "~a ~a ~a ~a ~a ~a" *pp* *block* *trial* *trialtype* (float accuracy) 0))
                (write-to-file t 0 "tracking" (format nil "~a ~a ~a ~a ~a ~a" *pp* *block* *trial* *trialtype* (float accuracy) 0))
            )
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COUNTING TASK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; COUNTING VARIABLES
(defparameter *counting-trial-len* 29.5)
(defparameter *counting-pres-rate* 1.5)
(defparameter *counting-tone-counts* (list 0 0))
(defparameter *counting-freqs* '(500 1250))
(defparameter *counting-tone-probs* '(0.25 0.75))
(defparameter *counting-target-tone* 1)
(defparameter *counting-foil-tone* 0)
(defparameter *counting-tone-duration* 0.15)
(defparameter *counting-fb-duration* 0.5)

(defparameter *counting-answer-space* (list 10 11 12 13 14 15 16 17))
(defparameter *counting-remaining-answers* nil)
(defparameter *counting-next-tone-idx* 0)
(defparameter *counting-next-tone-t* 0)
(defparameter *counting-next-tone-id* 0)
(defparameter *counting-tone-sequence* nil)
(defparameter *counting-tone-times* nil)

(defparameter *counting-finished* nil)
(defparameter *counting-finished-timeout* nil)
(defparameter *counting-showing-fb* nil)
(defparameter *counting-fb-t* 0)
(defparameter *counting-responded* nil)
(defparameter *counting-response-hand* 0) ; 0 = right, 1 = left

(defparameter *counting-typed-answer* 0)
(defparameter *counting-answer* 0)
(defparameter *counting-correct* 0)
(defparameter *counting-errordist* 0)

(defparameter *counting-xC* 0)

(defparameter *counting-concluded* nil)

;; COUNTING METHODS
(defmethod counting-load ()
    (when (task-is-active +C+)

        (let* (
            (xC (cond ((> (sum (active-tasks *exp-device*)) 1) ; counting switches sides depending on the other task
                    (if (task-is-active +N+)
                        (/ (+ *screen-w* *xC*) 2.0)
                        (/ *xC* 2.0)
                    )
                )
                (t *xC*)
                ))
            (yC *yC*)
            (fix-loc (car (define-chunks-fct `((isa visual-location screen-x, xC screen-y, yC 
                                                    kind, 'text value, "X" height, 10 width, 10 color, 'black)))))
            (fix-obj (car (define-chunks (isa visual-object value "X" height 10 width 10 color black))))
            )

            (setf *counting-xC* xC)
            ;(setf (counting-fixation *exp-device*) (cons fix-loc fix-obj))

            (if *debug* (format t "~%~%### COUNTING - Screen center at ~a" xC))
        )
    )
)

(defmethod counting-unload ()

    (setf (counting-fixation *exp-device*) nil)
)

(defmethod counting-new-trial ()
    (when (task-is-active +C+)

        (setf *counting-finished* nil)
        (setf *counting-concluded* nil)
        (setf *counting-responded* nil)

        (setf *counting-tone-counts* (list 0 0))
        (setf *counting-typed-answer* 0)
        (setf *counting-answer* 0)
        (setf *counting-correct* 0)
        (setf *counting-errordist* 0)

        (setf *counting-next-tone-idx* 0)
        (setf *counting-next-tone-t* 0)
        (setf *counting-next-tone-id* 0)
        (setf *counting-tone-sequence* nil)
        (setf *counting-tone-times* nil)

    ;(print *counting-tone-counts*)
        (if *debug* (format t "~%~%### COUNTING - New trial"))

        (if *preload-trial-info* ; load trial info from file
            (counting-preload-trial)
            (counting-schedule-tones)
        )
    )
)

(defun add-current-time (x)
    (+ x (mp-time))
)

(defun counting-preload-trial ()
    (setf preloaded (cdr (assoc (+ 1 *current-trial*) *preload-block-counting*)))
    (setf *counting-tone-sequence* (loop for el in preloaded collect (if (> (car el) 300) 1 0 ))) ; convert Hz to 0/1
    (setf times (loop for el in preloaded collect (cdr el)))
    (setf *counting-tone-times* (mapcar 'add-current-time times)) ; add the current exp time to the presentation times
)

(defun counting-schedule-tones ()
    (setf *counting-tone-sequence* nil)
    (setf *counting-tone-times* nil)

    ;(print (mp-time))

    (let* (
        (s (* (/ 2 5) *counting-pres-rate*))
        (times (loop for i from 0.5 to (- *counting-trial-len* 0.4) by *counting-pres-rate* 
                     collect (+ i (- (act-r-random (* s 2)) s)))) ; evenly space the tones, then add some jitter
        (num (length times))
        (lst (- num 1))
        (toneseq (make-list num :initial-element *counting-target-tone*))
        (places (range 1 num)) ; first tone (0) is always the target tone
        )
        
        ; constrain the first and last tones
        (if (< (car times) 0.3) (setf (nth 0 times) 0.3))
        (if (> (nth lst times) *counting-trial-len*) (setf (nth lst times) (- *counting-trial-len* 0.1)))
        (setf times (mapcar 'add-current-time times)) ; add the current exp time to the presentation times

        ; decide on the number of target tones
        (if (= (length *counting-remaining-answers*) 0) (setf *counting-remaining-answers* *counting-answer-space*))
        (setf pick-idx (act-r-random (length *counting-remaining-answers*)))

        (setf answer (min (nth pick-idx *counting-remaining-answers*) num))
        (setf *counting-remaining-answers* (remove-nth pick-idx *counting-remaining-answers*))
        ;(print *counting-remaining-answers*

        ;(print answer)
        ;(print times)
        ;(print places)

        (setf foils (- num answer))

        (loop for i from 1 to foils do
            (setf j (act-r-random (length places)))
            (setf rep-idx (nth j places))
            ;(print rep-idx)
            (setf places (remove-nth j places))
            ;(print places)

            (setf (nth rep-idx toneseq) *counting-foil-tone*)
        )

        ;(print toneseq)
        (setf *counting-tone-sequence* toneseq)
        (setf *counting-tone-times* times)
    )

)

(defmethod counting-play-tone ()
    
    (new-tone-sound (nth *counting-next-tone-id* *counting-freqs*) *counting-tone-duration*)
    (incf (nth *counting-next-tone-id* *counting-tone-counts*))
    ;(print *counting-tone-counts*)
    ;(print-audicon)
)

(defmethod counting-finish-trial ()

    (let* (
        (resp-loc (car (define-chunks-fct `((isa visual-location screen-x, *counting-xC* screen-y, *yC*
                                                kind, 'text value, "#Tones?" height, 20 width, 80 color, 'black)))))
        (resp-obj (car (define-chunks (isa visual-object value "#Tones?" height 20 width 80 color black))))
        )

        (if *debug* (format t "~%~%### COUNTING - Showing answer prompt"))

        (setf (counting-fixation *exp-device*) nil)
        (setf (counting-respond *exp-device*) (cons resp-loc resp-obj))

        (new-tone-sound 2000 1.5)
        (model-set-counting-done)
        (setf *counting-finished* t)
        (setf *counting-finished-timeout* (+ (mp-time) 10))
    )
)

(defmethod counting-show-feedback (correct)

    (let* (
        (fb-text (if correct "Correct" "Wrong"))
        (fb-color (if correct 'green 'red))
        (fb-size (if correct 20 40))
        (fb-loc (car (define-chunks-fct `((isa visual-location screen-x, *counting-xC* screen-y, *yC*
                                                kind, 'text value, fb-text height, fb-size width, (* fb-size 4) color, fb-color)))))
        (fb-obj (car (define-chunks-fct `((isa visual-object value, fb-text height, fb-size width, (* fb-size 4) color, fb-color)))))
        )

        (if *debug* (format t "~%~%### COUNTING - Showing feedback: ~a" fb-text))

        (setf (counting-respond *exp-device*) nil)
        (setf (counting-feedback *exp-device*) (cons fb-loc fb-obj))

        (setf *counting-showing-fb* t)
        (setf *counting-fb-t* (+ (mp-time) *counting-fb-duration*))
    )

)

(defmethod counting-do-update (tt)
    (when (task-is-active +C+)
;(format t "!!! counting update. Finished: ~a" *counting-finished*)
        (cond
            ((not *counting-finished*)

                (when (> tt *counting-next-tone-t*)

                    (if *debug* (format t "~%~%### COUNTING - Playing tone of ~a Hz at time ~a (# ~a)" 
                                        (nth *counting-next-tone-id* *counting-freqs*) tt (nth *counting-next-tone-id* *counting-tone-counts*)))

                    (counting-play-tone)
                    ;(counting-schedule-tone tt)
                    (incf *counting-next-tone-idx*)
                    (cond ((< *counting-next-tone-idx* (length *counting-tone-times*))
                        (setf *counting-next-tone-id* (nth *counting-next-tone-idx* *counting-tone-sequence*))
                        (setf *counting-next-tone-t* (nth *counting-next-tone-idx* *counting-tone-times*))
                        (if *debug* (format t "~%~%### COUNTING - Scheduling tone of ~a Hz at time ~a" (nth *counting-next-tone-id* *counting-freqs*) *counting-next-tone-t*))
                    )
                    (t
                        (setf *counting-next-tone-t* (+ tt 9999))
                    ))
                )
            )
            (t ; else

              ;(format t "!!!counting finished. Time: ~a Feedback time: ~a" tt *counting-fb-t*)
                (cond 
                    ((and *counting-showing-fb* (> tt *counting-fb-t*))
                        ; trial is really finished now

                        (setf *counting-showing-fb* nil)
                        (setf *counting-fb-t* 0)
                        (setf (counting-feedback *exp-device*) nil)

                        (setf *counting-concluded* t)
                    )
                    (t
                        (when (> tt *counting-finished-timeout*)
                            (counting-input "+")
                        )
                    )
                )
            )
        )
    )
)

(defmethod counting-input (key)
    (when (and (task-is-active +C+) *counting-finished* (not *counting-responded*))
        (let* (
            (c (string key))
            (tens (if (= *counting-response-hand* 0) "u" "e"))
            (ones (if (= *counting-response-hand* 0) "i" "r"))
            )

            (cond 
                ((string-equal c tens)
                    (setf *counting-typed-answer* (+ *counting-typed-answer* 10))
                )
                ((string-equal c ones)
                    (setf *counting-typed-answer* (+ *counting-typed-answer* 1))
                )
                ((string-equal c "+")

                    (let* (
                        (ans (nth *counting-target-tone* *counting-tone-counts*))
                        (response *counting-typed-answer*)
                        (error-dist (abs (- response ans)))
                        (correct (if (= ans response) t nil))
                        )

                        (if *debug* (format t "~%~%### COUNTING - Received input: ~a (answer is ~a)" response ans))
                        ;(if t (format t "~%~%### COUNTING - Received input: ~a (answer is ~a)~%" word ans))

                        (setf *counting-responded* t)

                        (setf *counting-errordist* error-dist)
                        (if correct (setf *counting-correct* 1))

                        (counting-show-feedback correct)
                    )
                )
            )
        )
    )
)

(defun counting-save-trial-result ()
    (when (task-is-active +C+)

        (let* (
            (accuracy *counting-correct*)
            (err *counting-errordist*)
            (trtp (device-trial-type))
            (prev-trials-perf (cdr (assoc trtp *result-pp-counting-accuracy*)))
            (prev-trials-err (cdr (assoc trtp *result-pp-counting-errordist*)))
            )

            (rplacd (assoc trtp *result-pp-counting-accuracy*) (cons accuracy prev-trials-perf))
            (rplacd (assoc trtp *result-pp-counting-errordist*) (cons err prev-trials-err))

            (when *save-trial-result*
                (write-to-file t *pp* "tones" (format nil "~a ~a ~a ~a ~a ~a" *pp* *block* *trial* *trialtype* accuracy err))
                (write-to-file t 0 "tones" (format nil "~a ~a ~a ~a ~a ~a" *pp* *block* *trial* *trialtype* accuracy err))
            )
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXPERIMENT FLOW FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun do-exp ()

    (device-reset)
    (model-reset)


    ;; Start by resetting the model. Doesn't always seem to work
    (reset)

    (when *save-buffer-trace*
        (eval `(sgp :model-warnings nil))
        (eval `(sgp :save-buffer-trace t))
        (eval `(sgp :traced-buffers (IMAGINAL RETRIEVAL MANUAL VISUAL VISUAL-LOCATION AURAL)))
    )

    (when *fit-active*
        (eval `(sgp ,*fit-param* ,*fit-x*))
        ;(print (sgp :rt))
    )

    (if *debug* (format t "##########################~%Starting run"))

    (install-device *exp-device*)

    (setf *current-trial* -1)
    (when (not *preload-trial-info*)
      (print "YEAHAHDHAD")
        (device-set-trial-order)
    )

    (schedule-periodic-event *update-freq* 'device-update-tasks)
    ;(schedule-periodic-event 0.1 'device-update-tasks)

    ;(schedule-periodic-event 1.0 'print-visicon)

    ;(setf *block* 1)

    (device-next-trial)

    (run *model-run-len*)

    (gather-pp-results)
)

(defun do-trial ()
  
    (if *debug-sim* (format t "trial ~a (~a) # " *current-trial* (string (device-trial-type))))
    (if *debug* (format t "~%~%## Starting trial ~a" *current-trial*))

    (setf *trial* (+ 1 *current-trial*))
    (setf *trialtype* (string (device-trial-type)))

    (when *save-trial-result*
        (write-to-file t *pp* "trials" (format nil "~a ~a ~a ~a ~a" *pp* *block* *trial* *trialtype* (mp-time)))
        (write-to-file t 0 "trials" (format nil "~a ~a ~a ~a ~a" *pp* *block* *trial* *trialtype* (mp-time)))
    )

    (device-load-tasks)

    (setf *in-trial* t)
    (setf *trial-start-t* (mp-time))

    (setf *model-allow-visloc-buffer-stuffing* nil)
    (if (task-is-active +N+)
        (if (not (task-is-active +T+))
            (setf *model-allow-visloc-buffer-stuffing* t)  ; only one visual task
        )
    )

    (if (task-is-active +N+)
        (nback-new-trial))
    (if (task-is-active +T+)
        (tracking-new-trial))
    (if (task-is-active +C+)
        (counting-new-trial))

    ;;(delete-chunk-fct *tracking-dot-loc*)
)

; run m simulated runs with random trial content
(defun do-sim (m)

    (result-reset)

    (when *save-trial-result*
        (write-to-file nil 0 "trials" "pp block trial condition starttime")
        (write-to-file nil 0 "nback" "pp block trial condition correct RT")
        (write-to-file nil 0 "tones" "pp block trial condition correct mismatch")
        (write-to-file nil 0 "tracking" "pp block trial condition accuracy distance")
    )

    (let ((start-time (get-universal-time)))
        (loop for pp from 1 to m do
            (let ((pp-time (get-universal-time)))
                (setf *pp* pp)
                (setf *block* 1)

                (when *save-trial-result*
                    (write-to-file nil *pp* "trials" "pp block trial condition starttime")
                    (write-to-file nil *pp* "nback" "pp block trial condition correct RT")
                    (write-to-file nil *pp* "tones" "pp block trial condition correct mismatch")
                    (write-to-file nil *pp* "tracking" "pp block trial condition accuracy distance")
                )

                (if *debug-sim* (format t "Participant ~a~%" pp))

                (do-exp)

                (if *debug-sim* (format t "~%Participant runtime: ~a minutes~%" (round-to (my/ (- (get-universal-time) pp-time) 60.0) 2)))
            )
        )
        (if *debug-sim* (format t "~%Total simulation time: ~a minutes~%" (round-to (my/ (- (get-universal-time) start-time) 60.0) 2)))
    )

    (if *debug-sim* (format t "~%~%~%############ AVERAGED RESULTS ##############~%~%"))
    (report-results)
)

;; LOAD TRIAL INFORMATION FROM FILE
(defparameter *preload-trial-info* nil)
(defparameter *preload-file-dir* "~/Projects/fMRI/behavioral")
(defparameter *preload-participants* (list "001" "002" "003" "004" "005" "006" "012" "013" "014" "015" "016" "019" "020" "021" "023" "024" "029" "032" "033" "035"))
(defparameter *preload-block-trials* nil)
(defparameter *preload-block-nback* nil)
(defparameter *preload-block-counting* nil)
(defparameter *preload-block-tracking* nil)

; do the simulation with preloaded trials
(defun do-preload-sim ()
    ; preload the trial content from behavioral data
    (setf *preload-trial-info* t)
    (setf sv-bfr *save-buffer-trace*)
    (setf *save-buffer-trace* t)

    (result-reset)

    (when *save-trial-result*
        (write-to-file nil 0 "trials" "pp block trial condition starttime")
        (write-to-file nil 0 "nback" "pp block trial condition correct RT")
        (write-to-file nil 0 "nbackpress" "pp block trial num press")
        (write-to-file nil 0 "tones" "pp block trial condition correct mismatch")
        (write-to-file nil 0 "tracking" "pp block trial condition accuracy distance")
    )

    (setf ppnum 1)

    (let ((start-time (get-universal-time)))
        (loop for pp in *preload-participants* do
            (let ((pp-time (get-universal-time)))
                (setf *pp-id* pp)
                (setf *pp* ppnum)

                (when *save-trial-result*
                    (write-to-file nil *pp* "trials" "pp block trial condition starttime")
                    (write-to-file nil *pp* "nback" "pp block trial condition correct RT")
                    (write-to-file nil *pp* "nbackpress" "pp block trial num press")
                    (write-to-file nil *pp* "tones" "pp block trial condition correct mismatch")
                    (write-to-file nil *pp* "tracking" "pp block trial condition accuracy distance")
                )

                (loop for ppblock from 1 to 6 do
                    (setf *pp-block* ppblock)
                    (setf *block* ppblock)

                    (format t "Participant ~a (~a), Block ~a~%" *pp* *pp-id* *pp-block*)

                    ; load trial order from file
                    (setf *preload-block-trials* (read-a-table-from-file (format nil "~a/fmt~a-trials-block~a.dat" *preload-file-dir* *pp-id* *pp-block*) 10))
                    (setf numtrials (length *preload-block-trials*))
                    (setf *trial-order* nil)

                    (loop for trial from 1 to numtrials do
                        (setf trialtype (nth 2 (nth trial *preload-block-trials*)))
                        (cond 
                            ((string-equal trialtype "N_") (push +N_+ *trial-order*))
                            ((string-equal trialtype "T_") (push +T_+ *trial-order*))
                            ((string-equal trialtype "C_") (push +C_+ *trial-order*))
                            ((string-equal trialtype "NC") (push +NC+ *trial-order*))
                            ((string-equal trialtype "NT") (push +NT+ *trial-order*))
                            ((string-equal trialtype "CT") (push +CT+ *trial-order*))
                            (t (push +FX+ *trial-order*))
                        )

                    )
                    (setf *trial-order* (reverse *trial-order*))
                    (setf *model-run-len* (* (+ 10 (length *trial-order*)) (+ *trial-duration* *fixation-duration* *info-duration* 5)))

                    ; load task data from file
                    (preload-nback)
                    (preload-tracking)
                    (preload-counting)

                    (do-exp)

                )
                (setf ppnum (+ ppnum 1))

                (format t "~%Participant runtime: ~a minutes~%" (round-to (my/ (- (get-universal-time) pp-time) 60.0) 2))
            )
        )
        (format t "~%Total simulation time: ~a minutes~%" (round-to (my/ (- (get-universal-time) start-time) 60.0) 2))
    )

    (setf *preload-trial-info* nil)
    (setf *save-buffer-trace* sv-bfr)
)

; get the data needed to preload nback trials from file
(defun preload-nback ()
    (setf preload (read-a-table-from-file (format nil "~a/fmt~a-nback-block~a.dat" *preload-file-dir* *pp-id* *pp-block*) 14))
    (setf keys (loop for i from 1 to (length preload) collect (nth 1 (nth i preload))))
    (setf keys (remove nil (remove-duplicates keys)))
    (setf vals nil)

    (loop for trl in keys do ; go over each trial, collect stimuli letters
        (setf letter-order (loop for i from 1 to (length preload) collect 
                            (if (eq (nth 1 (nth i preload)) trl) (symbol-name (nth 7 (nth i preload))))
        ))
        (push (remove nil letter-order) vals)
    )

    (setf *preload-block-nback* (pairlis keys (reverse vals)))
)

; get the data needed to preload tracking trials from file
(defun preload-tracking ()
    (setf preload (read-a-table-from-file (format nil "~a/fmt~a-trackingstart-block~a.dat" *preload-file-dir* *pp-id* *pp-block*) 4))
    (setf keys (loop for i from 1 to (length preload) collect (nth 1 (nth i preload))))
    (setf vals (loop for i from 1 to (length preload) collect (nth 3 (nth i preload))))

    (setf *preload-block-tracking* (pairlis keys vals))
)

; get the data needed to preload tone counting trials from file
(defun preload-counting ()
    (setf preload (read-a-table-from-file (format nil "~a/fmt~a-tonetimes-block~a.dat" *preload-file-dir* *pp-id* *pp-block*) 7))
    (setf keys (loop for i from 1 to (length preload) collect (nth 1 (nth i preload))))
    (setf keys (reverse (remove nil (remove-duplicates keys))))
    (setf vals nil)

    (loop for trl in keys do ; go over each trial, collect stimuli tones and onset times
        (setf tone-order (loop for i from 1 to (length preload) collect 
                            (if (eq (nth 1 (nth i preload)) trl) (cons (nth 3 (nth i preload)) (nth 4 (nth i preload))))
        ))
        (push (remove nil tone-order) vals)
    )

    (setf *preload-block-counting* (pairlis keys (reverse vals)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MODEL RESULT FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bold response data
(defparameter *pp* nil)
(defparameter *pp-id* nil)
(defparameter *block* nil)
(defparameter *pp-block* nil)
(defparameter *trial* nil)
(defparameter *trialtype* nil)
(defparameter *motor-right-hand* nil)
(defparameter *motor-left-hand* nil)
(defparameter *visloc-spatial-event* nil)

; result directories
(defparameter *output-pathname*
  (merge-pathnames 
   (make-pathname :name "fixed-combinations" :directory '(:relative "output") :type "dat")  
   "~/Dropbox/Work/ACT-R/Models/Forcedcombi/"))
(defparameter *preload-output-pathname*
  (merge-pathnames 
   (make-pathname :name "fixed-combinations" :directory '(:relative "pp-output") :type "dat")  
   "~/Dropbox/Work/ACT-R/Models/Forcedcombi/"))

;; write module activity
(defun write-fmri-results ()
    (let ((act-trace (parse-trace-lists-for-bold (get-module bold))))
        (write-to-file-bold (format nil "dualfixed-bold-~a_~a_~a_~a" *pp* *block* *trial* *trialtype*) act-trace)
    )
    ; clear trace lists
    (get-current-buffer-trace t)
    (setf *motor-right-hand* nil)
    (setf *motor-left-hand* nil)
    (setf *visloc-spatial-event* nil)
)

;; format the module activity file
(defun write-to-file-bold (name lst)
    (setf pathn *output-pathname*)
    (if *preload-trial-info*
        (setf pathn *preload-output-pathname*) 
    )
    (with-open-file 
        (out (ensure-directories-exist
        (merge-pathnames 
            (make-pathname :name name :type "dat")  
            pathn))
            :direction :output :if-does-not-exist :create :if-exists :supersede)
        (dolist (line lst)
            (dolist (times (cdr line)) 
                (format out "~a ~a ~a~%" (car line) (first times) (second times))
                ;(if (eq (car line) 'MANUAL)
                    ;; add the seperation of left and right manual buffer
                    ;(if (find (first times) *motor-right-hand*)
                        ;(format out "LEFTMOTOR ~a ~a~%" (first times) (second times))
                        ;(format out "RIGHTMOTOR ~a ~a~%" (first times) (second times))
                    ;)
                ;)
            )
        )
        ; add left/right manual
        (loop for times in *motor-right-hand* do
            (format out "~a ~a ~a~%" "RIGHTMOTOR" times (+ times 0.170))
        )
        (loop for times in *motor-left-hand* do
            (format out "~a ~a ~a~%" "LEFTMOTOR" times (+ times 0.170))
        )
        (loop for times in *visloc-spatial-event* do
            (format out "SPATIAL ~a ~a~%" times (+ times 0.05))
        )

    )
)

; write model performance data
(defun write-to-file (do-append pp filename row)
    (setf pathn *output-pathname*)
    (if *preload-trial-info*
        (setf pathn *preload-output-pathname*) 
    )
    (let* (
        (name (format nil "dualfixed-~a-~d" filename pp))
        (path (merge-pathnames (make-pathname :name name :type "dat") pathn))
           )
        (if do-append
            (with-open-file 
                (out (ensure-directories-exist path) :direction :output :if-does-not-exist :create :if-exists :append)
                (format out "~a~%" row)
            )
            (with-open-file 
                (out (ensure-directories-exist path) :direction :output :if-does-not-exist :create :if-exists :supersede)
                (format out "~a~%" row)
            )
        )
    )
)

(defparameter *fit-active* nil)
(defparameter *fit-x* 0.0)
(defparameter *fit-param* nil)

(defun est-param (m)

    (setf *debug-sim* nil)
    (setf *debug* nil)
    (setf *fit-active* t)

    (let* (
        (start-time (get-universal-time))
        (param ':rt)
        (x -0.5)
        (lim 0.5)
        (d 0.08)
        (filename (concatenate 'string *filepath* "/data/est-param-" (string param)  ".dat"))
        )

        (setf *fit-param* param)

        (with-open-file (out filename :direction :output :if-exists :supersede)
            (format out "param value condition task measure result~%")
        )

        (loop while (<= x lim) do
            (setf *fit-x* x)

            (format t "~% ## Parameter Value: ~a~%" *fit-x*)

            ;(eval `(sgp :rt ,*fit-x*))
            ;(print (sgp :rt))
            ;(sgp :rt x)
            (do-sim m)

            ; get correlations and deviations
            (let (
                (N-acc-corr (sample-corr (avg-values-only *result-nback-accuracy*) (values-only *data-nback-accuracy*)))
                ;(T-acc-corr (sample-corr (avg-values-only *result-tracking-accuracy*) (values-only *data-tracking-accuracy*)))
                ;(C-acc-corr (sample-corr (avg-values-only *result-counting-accuracy*) (values-only *data-counting-accuracy*)))
                ;(N-RT-corr  (sample-corr (avg-values-only *result-nback-RT*) (values-only *data-nback-RT*)))
                ;(C-err-corr (sample-corr (avg-values-only *result-counting-errordist*) (values-only *data-counting-errordist*)))

                ;(N-acc-dev (mae (avg-values-only *result-nback-accuracy*) (values-only *data-nback-accuracy*)))
                ;(T-acc-dev (mae (avg-values-only *result-tracking-accuracy*) (values-only *data-tracking-accuracy*)))
                ;(C-acc-dev (mae (avg-values-only *result-counting-accuracy*) (values-only *data-counting-accuracy*)))
                ;(N-RT-dev  (mae (avg-values-only *result-nback-RT*) (values-only *data-nback-RT*)))
                ;(C-err-dev (mae (avg-values-only *result-counting-errordist*) (values-only *data-counting-errordist*)))
                )

                (with-open-file (out filename :direction :output :if-exists :append)
                    ;(format out "~a ~a corr N acc ~a~%" param x N-acc-corr)
                    ;(format out "~a ~a corr T acc ~a~%" param x T-acc-corr)
                    ;(format out "~a ~a corr C acc ~a~%" param x C-acc-corr)
                    ;(format out "~a ~a corr N RT ~a~%" param x N-RT-corr)
                    ;(format out "~a ~a corr C err ~a~%" param x C-err-corr)
                    (format out "~a ~a N_ N acc ~a~%" param x (average (assoc-vals 'N_ *result-nback-accuracy*)))
                    (format out "~a ~a NC N acc ~a~%" param x (average (assoc-vals 'NC *result-nback-accuracy*)))
                    (format out "~a ~a N_ N RT ~a~%" param x (average (assoc-vals 'N_ *result-nback-RT*)))
                    (format out "~a ~a NC N RT ~a~%" param x (average (assoc-vals 'NC *result-nback-RT*)))
                    (format out "~a ~a C_ C acc ~a~%" param x (average (assoc-vals 'C_ *result-counting-accuracy*)))
                    (format out "~a ~a NC C acc ~a~%" param x (average (assoc-vals 'NC *result-counting-accuracy*)))
                    (format out "~a ~a C_ C err ~a~%" param x (average (assoc-vals 'C_ *result-counting-errordist*)))
                    (format out "~a ~a NC C err ~a~%" param x (average (assoc-vals 'NC *result-counting-errordist*)))

                )
            )

            ;(print (sgp :rt))

            (setf x (+ x d))
        )
        (format t "~%Total estimation time: ~a minutes~%" (round-to (my/ (- (get-universal-time) start-time) 60.0) 2))
        (setf *fit-active* nil)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACT-R MODEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; empty buffers between trials to avoid deadlocks
(defun hook-empty-buffers ()

    ;(clear-buffer 'goal)
    (remove-all-m-buffer-chunks 'goal) ; multi-buffer
    (clear-buffer 'imaginal)
    (clear-buffer 'retrieval)
    (clear-buffer 'visual)
    (clear-buffer 'visual-location)
    (clear-buffer 'manual)
    (clear-buffer 'aural)
    (clear-buffer 'aural-location)
)

; initialize the goals for each task
(defun hook-set-goals ()

    ; visloc stuffing only works when a task can have full visual attention
    (setf atten 'full)
    (if (task-is-active +T+) (setf attention 'partial))

    (if (task-is-active +N+) (goal-focus-fct (car (define-chunks-fct
                              `((isa nback-goal id, *current-trial* state, 'start num, 0 attention, atten)))))
    )
    (if (task-is-active +T+) (goal-focus-fct (car (define-chunks-fct
                              `((isa tracking-goal id, *current-trial* finished, 'false state, 'watching tpos, 0 cpos, 0)))))
    )
    (if (task-is-active +C+) (goal-focus-fct (car (define-chunks-fct
                              `((isa counting-goal id, *current-trial* state, 'start heard, 'false storedcount, "0" imrehearse, 'false)))))
    )

    ; counting answering can be done with the left or right hand, depending on the other task
    (if (task-is-active +C+)
        (if (task-is-active +T+)
            (setf *counting-response-hand* 1) ; 0 = right, 1 = left
            (setf *counting-response-hand* 0)
        )
    )
)

; set the number of trials and the order
(defun device-set-trial-order ()

    (setf *trial-order* (list 
                              +N_+ ;+N_+ +N_+ +N_+ +N_+ +N_+ +N_+ +N_+ +N_+ +N_+ 
                              +T_+ ;+T_+ +T_+ +T_+ +T_+ +T_+ +T_+ +T_+ +T_+ +T_+ 
                              +C_+ ;+C_+ +C_+ +C_+ +C_+ +C_+ +C_+ +C_+ +C_+ +C_+ 
                              +NT+ ;+NT+ +NT+ +NT+ +NT+ +NT+ +NT+ +NT+ +NT+ +NT+ 
                              +NC+ ;+NC+ +NC+ +NC+ +NC+ +NC+ +NC+ +NC+ +NC+ +NC+ 
                              +CT+ ;+CT+ +CT+ +CT+ +CT+ +CT+ +CT+ +CT+ +CT+ +CT+ 
                              ))

    ; make sure the model runs long enough
    (setf *model-run-len* (* (+ 10 (length *trial-order*)) (+ *trial-duration* *fixation-duration* *info-duration* 5)))
)

; ACT-R tracking is disabled as it doesn't work well with threaded cognition in concurrent situations
; These functions do the object tracking for the tracking task, by updating the goal with new position info
(defun model-update-tracked-target (pos)

    (setf tracking-chunk (car (find-matching-chunks (define-chunk-spec-fct 
                                                `(isa tracking-goal = id, *current-trial* > tpos 0)))))

    (when tracking-chunk 
        (set-chunk-slot-value-fct tracking-chunk 'trackedpos pos)

        (setf cpos (chunk-slot-value-fct tracking-chunk 'cpos))
        (if (not cpos) (setf cpos 0))
        (setf dist (- cpos *tracking-target-loc*))
        (set-chunk-slot-value-fct tracking-chunk 'trackeddist dist)
    )

)

(defun model-set-counting-done ()

    ; stop tracking from trying to track
    (setf tracking-chunk (car (find-matching-chunks (define-chunk-spec-fct 
                                                `(isa tracking-goal = id, *current-trial* > tpos 0)))))

    (when tracking-chunk 
        (set-chunk-slot-value-fct tracking-chunk 'finished 'true)
    )
)

; if the tone count is completely forgotten, the model makes an educated guess
(defun model-counting-guess (storedcount)
    (setf num (parse-integer storedcount))
    (setf difference (- 1 (act-r-random 3)))
    (setf guess (max 0 (+ num difference)))

    (write-to-string guess)
)

; each nback state has a unique id to avoid activation inflation
(defun model-get-new-nback-state-id ()
    (incf *model-nback-state-free-id*)
)

; record which hand was used to later split motor response into left and right manual resources
(defun model-record-hand (hand)
    (if (= hand 0)
        (push (mp-time) *motor-right-hand*)
        (push (mp-time) *motor-left-hand*)
    )
    t
)

; record just the spatial refocussing
(defun model-record-spatial-event ()
    (setf *visloc-spatial-event* (push (mp-time) *visloc-spatial-event*))
)

; disable buffer stuffing
(defmethod stuff-visloc-buffer ((vis-mod vision-module)) nil)

;;; Disable updating attended location
(defmethod update-attended-loc ((vis-mod vision-module)) nil)

;(defmethod stuff-visloc-buffer ((vis-mod vision-module))

  ;; only enable buffer stuffing when there is one task with visual stimuli
  ;; otherwise buffer stuffing will really mess with TC
  ;; essentially, this is like saying that when the model can focus on a single visual area,
  ;; locations are stuffed, as there is no need for visual 'roaming' to find the stimuli
    ;(when *model-allow-visloc-buffer-stuffing*
      ;(print "Stuffing buffer!")

        ;(unless (or (buffer-read 'visual-location)
                  ;(not (zerop (locks (current-device-interface))))
                  ;(tracked-obj-last-feat vis-mod))
        ;(awhen (find-current-locs-with-spec vis-mod (default-spec vis-mod))
               
               ;(schedule-set-buffer-chunk 'visual-location
                                          ;(random-item (sort (objs-max-val it 'chunk-visual-tstamp) #'string< :key #'symbol-name))
                                          ;0
                                          ;:module :vision
                                          ;:requested nil
                                          ;:priority 10)
               ;(lock-device (current-device-interface))
               ;(schedule-event-relative 0 'unlock-device 
                                    ;:module :vision
                                    ;:destination :device
                                    ;:priority 9
                                    ;:output nil
                                    ;:maintenance t)))
    ;)
    ;(when (not *model-allow-visloc-buffer-stuffing*) nil)
;)



;; OUTPUT DEFAULT VALUES
(defparameter *debug* t)
(defparameter *debug-trackpos* t)
(defparameter *debug-sim* t)

(defparameter *save-buffer-trace* t)
(defparameter *save-trial-result* t)

;; TURN OFF DEBUGGING
(setf *debug* nil)
(setf *debug-trackpos* nil)

;; TURN OFF OUTPUT SAVING
;(setf *save-buffer-trace* nil)
;(setf *save-trial-result* nil)


; turn visloc buffer stuffing on or off
(defparameter *model-allow-visloc-buffer-stuffing* nil)
; distraction-task kind of messes with the BOLD predictions, so it can be turned off
(defparameter *model-include-distraction* nil)
; how close the model wants to stay to the dot during tracking
(defparameter *model-max-allowed-tracking-drift* (* *tracking-r* (/ 1 3)))
; how many counting facts should be generated
(defparameter *model-num-count-facts* 30)
; initialise nback-state free id
(defparameter *model-nback-state-free-id* 0)

(define-model fixedchoice-2013
    
    (set-hand-location left 4 3) ; index finger over the R (middle over E)
    (set-hand-location right 7 3) ; index finger over the U (middle over I)

    ;; PARAMETERS

    (sgp
        :v nil

        ;:act t                        ; default: NIL  : Activation Trace

        :ncnar nil ;default: T improves performance when off

        ;:seed (999999 11) ; FOR DEBUGGING/TESTING ONLY
        :trace-detail low

        :esc t                          ; default: NIL  : Enable Subsymbolic Computations
        :er t                           ; default: NIL  : Enable Randomness
        :ol t                           ; default: T    : Optimized Learning
        :bll 0.5                        ; default: NIL  : Base Level Learning
        :alpha 0.2                      ; default: 0.2  : Production learning rate
        :lf 0.14                        ; default: 1.0  : Latency Factor
        :egs 0.5                        ; default: 0.0  : Expected Gain S
        :ans 0.05                       ; default: NIL  : Activation Noise S
        :rt 0.03                         ; default: 0.0  : Retrieval Threshold
        :ga 0                           ; default: 1    : source spread for the GOAL buffer

        :subvocalize-detect-delay 0.3   ; default: 0.3  : Sound detect time for a subvocalized word.

        :imaginal-delay 0.1             ; default: 0.2  : Time in seconds to respond to an imaginal request

        :visual-movement-tolerance 30   ; default: 0.5  : How far something can move while still being seen as the same object.
        :declarative-finst-span 20.0    ; default: 3.0  : Duration of declarative finst markers in seconds
        :declarative-num-finsts 20      ; default: 4    : Number of declarative finst markers
        ;:VISUAL-FINST-SPAN           30.0 ;            default: 3.0        : Lifespan of a visual finst

        :sound-decay-time 0.4          ; default: 3.0  : The amount of time after a sound has finished it takes for the sound to be deleted from the audicon
    )
  
    ; define chunk types
    (chunk-type number-order-fact value next)
    (chunk-type nback-goal      id state num attention)
    (chunk-type nback-state     id goalid original oldletter twoback access)
    (chunk-type tracking-goal   id state finished tpos cpos trackedpos trackeddist dist)
    (chunk-type counting-goal   id state heard storedcount ansremain imrehearse)
    (chunk-type counting-state  goal count access)
    (chunk-type distracted-goal state num)

    (add-dm
        ; add basic chunks to get rid of warnings
        (bigcircle isa chunk) (circle isa chunk) (dot isa chunk) (clr isa chunk)
        (attend-dot isa chunk) (find-circle isa chunk) (locate-circle isa chunk)
        (attend-circle isa chunk) (moving isa chunk)
        (observe-move isa chunk) (decide isa chunk) (retrieving isa chunk)
        (start isa chunk) (process isa chunk) (prefixation isa chunk)
        (fixation isa chunk) (true isa chunk) (false isa chunk)
        (listening isa chunk) (counting isa chunk) (remember isa chunk)
        (watching isa chunk) (hearing isa chunk) (feedback isa chunk)
        (answer isa chunk) (startcount isa chunk) (finishcount isa chunk)
        (noback isa text value "0") (stop isa chunk) (open isa chunk)
        (denied isa chunk) (locked isa chunk)

        ; goals
        ;(nback-goal     isa nback-goal      id -1 state start num 0)
        ;(counting-goal  isa counting-goal   id -1 state start heard false storedcount "0" imrehearse false)
        ;(tracking-goal  isa tracking-goal   id -1 state watching trackedpos 0 trackeddist 0)

        (nback-stop isa nback-goal id -1 state stop num 0)
        (counting-stop isa counting-goal id -1 state stop)
        (tracking-stop isa tracking-goal id -1 state stop)
    )

    ; add some basic number counting facts to the dm (current "0" next "1", etc.)
    (dotimes (i *model-num-count-facts*)
        (let* (
              (n (write-to-string i))
              (m (write-to-string (+ i 1)))
              (name (intern (concatenate 'string "number-order-" n "-" m)))
              )

            (eval `(add-dm (,name isa number-order-fact value ,n next ,m)))
            (eval `(sdp ,name :creation-time -1e12 :references 15000000))
        )
    )

    ;(goal-focus counting-goal)
    ;(goal-focus tracking-goal)
    (goal-focus empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRODUCTION RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NBACK MODEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p nback-start
   "Start the nback with a blank imaginal chunk"
    =goal>
        isa nback-goal
        id =goal-id
        state start
    ?imaginal>
        state free
==>
    =goal>
        state watching
    +imaginal>
        isa nback-state
        id 0
        goalid =goal-id
        original true
        oldletter ""
        twoback noback
        access open
)

(p nback-switch-im
   "Switch the content of the imaginal buffer"
    =goal>
        isa nback-goal
    =imaginal>
        isa nback-state
        access denied
    ?imaginal>
        state free

    =retrieval>
        isa nback-state
        id =state-id
        original =ori
        oldletter =letter
        twoback =twoback-id
==>
    =goal>
    =imaginal>
        id =state-id
        original false
        oldletter =letter
        twoback =twoback-id
        access locked

    =retrieval>
        goalid -1 ; don't merge this chunk with the existing one: that would inflate the activation
    -retrieval>

    !output! (retrieval =state-id x =letter x =twoback-id x =ori)
)

(p nback-switch-im-failed
    =goal>
        isa nback-goal
    =imaginal>
        isa nback-state
        access denied
    ?imaginal>
        state free

    ?retrieval>
        state error
==>
    -retrieval>
    =goal>
        num 0
    =imaginal>
        access locked
)

(p nback-find-letter
   "Look for a new nback letter"
    =goal>
        isa nback-goal
        state watching
        ;attention partial

    ?visual-location>
        state free
        buffer empty
    ?visual>
        state free
        buffer empty
==>
    =goal>

    +visual-location>
        isa visual-location
        kind text
        :attended nil
)

(p nback-attend-new-letter
   "Attend the new letter, and count how many letters have been seen"
    =goal>
        isa nback-goal
        state watching
        num =oldnum

    =visual-location>
        isa visual-location
        kind text
    ?visual>
        state free
        buffer empty
    !bind! =newnum (+ =oldnum 1)
==>
    =goal>
        num =newnum
        state process

    +visual>
        isa move-attention
        screen-pos =visual-location

    !safe-eval! (model-record-spatial-event)
)

(p nback-decide-response
   "When more than two letters have been seen, retrieve the 2-back letter to determine the response"
    =goal>
        isa nback-goal
        id =goal-id
        state process
        > num 2 ; no need to respond to the first two letters
    =imaginal>
        isa nback-state
        id =state-id
        twoback =twoback-id
        - access denied

    =visual>
        isa text
        value =letter
    ?retrieval>
        state free
        buffer empty
==>
    =goal>
        state retrieving
    =imaginal>
        access locked
        ;access open ; too soon, open buffer at the end of the action (that is, the response)

    =visual>
    +retrieval>
        isa nback-state
        id =twoback-id
        goalid =goal-id
        original true

    !output! (Decide retrieving =twoback-id)
)
(spp nback-decide-response :u 2.2) 

(p nback-decide-response-fumble
   "When more than two letters have been seen, retrieve the 2-back letter to determine the response"
    =goal>
        isa nback-goal
        id =goal-id
        state process
        > num 2 ; no need to respond to the first two letters
    =imaginal>
        isa nback-state
        id =state-id
        twoback =twoback-id
        - access denied

    =visual>
        isa text
        value =letter
    ?retrieval>
        state free
        buffer empty
==>
    =goal>
        state retrieving
    =imaginal>
        access locked
        ;access open ; too soon, open buffer at the end of the action (that is, the response)

    =visual>
    +retrieval>
        isa nback-state
        id =state-id
        goalid =goal-id
        original true

    !output! (Decide retrieving =state-id)
)
(spp nback-decide-response-fumble :u 1) 

(p nback-decide-response-wrong-im
    =goal>
        isa nback-goal
        id =goal-id
        state process
        > num 2
    =imaginal>
        isa counting-state
        access open

    =visual>
        isa text
        value =letter
    ?retrieval>
        state free
        buffer empty
==>
    =goal>
    +imaginal>
        isa nback-state
        id 0
        original true
        goalid =goal-id
        oldletter ""
        twoback noback
        access denied

    =visual>
    +retrieval>
        isa nback-state
        goalid =goal-id
        original true
)

(p nback-respond-same
   "The current letter is the same as the 2-back, respond by pressing a key"
    =goal>
        isa nback-goal
        state retrieving
        num =num
    =imaginal>
        isa nback-state
        - access denied

    =visual>
        isa text
        value =letter
    =retrieval>
        isa nback-state
        oldletter =letter
    ?manual>
        state free

==>
    =goal>
        state remember
    =imaginal>
        access open

    =retrieval>
        goalid -1
    -retrieval>

    =visual>
    +manual>
        isa punch
        hand left
        finger middle

    ;!safe-eval! (model-record-hand 1)
    !safe-eval! (push (mp-time) *motor-left-hand*)
)

(p nback-respond-different
   "The current letter is the different from the 2-back, respond by pressing a key"
    =goal>
        isa nback-goal
        state retrieving
        num =num
    =imaginal>
        isa nback-state
        - access denied

    =visual>
        isa text
        value =letter
    =retrieval>
        isa nback-state
        - oldletter =letter
    ?manual>
        state free

==>
    =goal>
        state remember
    =imaginal>
        access open

    =retrieval>
        goalid -1
    -retrieval>

    =visual>
    +manual>
        isa punch
        hand left
        finger index

    ;!safe-eval! (model-record-hand 1)
    !safe-eval! (push (mp-time) *motor-left-hand*)
)

(p nback-retrieval-failed-different
   "Failed to remember the 2back... Don't worry, it happens to everybody. Guess that the letter is different"
    =goal>
        isa nback-goal
        state retrieving
        num =num
    =imaginal>
        isa nback-state
        - access denied

    =visual>
        isa text
        value =letter
    ?retrieval>
        state error
    ?manual>
        state free

==>
    =goal>
        state remember
    =imaginal>
        access open

    =visual>
    +manual>
        isa punch
        hand left
        finger index

    ;!safe-eval! (model-record-hand 1)
    !safe-eval! (push (mp-time) *motor-left-hand*)
)

(p nback-retrieval-failed-same
   "Failed to remember the 2back... Don't worry, it happens to everybody. Guess that the letter is the same"
    =goal>
        isa nback-goal
        state retrieving
        num =num
    =imaginal>
        isa nback-state
        - access denied

    =visual>
        isa text
        value =letter
    ?retrieval>
        state error
    ?manual>
        state free

==>
    =goal>
        state remember
    =imaginal>
        access open

    =visual>
    +manual>
        isa punch
        hand left
        finger middle

    ;!safe-eval! (model-record-hand 1)
    !safe-eval! (push (mp-time) *motor-left-hand*)

)

(p nback-remember-first-two-letters
   "Remember the current letter by updating the imaginal with a new chunk containing the order of the last two letters"
    =goal>
        isa nback-goal
        id =goal-id
        state process
        <= num 2
    =imaginal>
        isa nback-state
        id =state-id
        original =ori
        oldletter =letter
        twoback =twoback-id
        - access denied
    ?imaginal>
        state free

    =visual>
        isa text
        value =newletter
    !bind! =new-state-id (model-get-new-nback-state-id)

==>
    =goal>
        state watching
    =imaginal>
        access open
    -imaginal>
    +imaginal>
        isa nback-state
        id =new-state-id
        goalid =goal-id
        original true
        oldletter =newletter
        twoback =state-id
        access open
        

    -visual>

    !output! (imaginal =state-id x =letter x =twoback-id x =ori)
    !output! (new imaginal =new-state-id x =newletter x =state-id x true)
)

(p nback-remember-first-two-letters-wrong-im
    =goal>
        isa nback-goal
        id =goal-id
        state process
        <= num 2
    =imaginal>
        isa counting-state
        access open

    =visual>
        isa text
        value =letter
    ?retrieval>
        state free
        buffer empty
==>
    =goal>
    +imaginal>
        isa nback-state
        id 0
        goalid =goal-id
        original true
        oldletter ""
        twoback noback
        access denied

    =visual>
    +retrieval>
        isa nback-state
        goalid =goal-id
        original true
)

(p nback-remember-letter
   "Does the same as nback-remember-first-two-letters, but always follows the response"
    =goal>
        isa nback-goal
        state remember
        id =goal-id
    =imaginal>
        isa nback-state
        id =state-id
        original =ori
        oldletter =letter
        twoback =twoback-id
        - access denied
    ?imaginal>
        state free

    =visual>
        isa text
        value =newletter

    !bind! =new-state-id (model-get-new-nback-state-id)
==>
    =goal>
        state watching
    =imaginal>
        access open
    -imaginal>
    +imaginal>
        isa nback-state
        id =new-state-id
        goalid =goal-id
        original true
        oldletter =newletter
        twoback =state-id
        access open

    -visual>

    !output! (imaginal =state-id x =letter x =twoback-id x =ori)
    !output! (new imaginal =new-state-id x =newletter x =state-id x true)
)

(p nback-remember-letter-wrong-im
    =goal>
        isa nback-goal
        id =goal-id
        state remember
    =imaginal>
        isa counting-state
        access open

    =visual>
        isa text
        value =letter
    ?retrieval>
        state free
        buffer empty
==>
    =goal>
    +imaginal>
        isa nback-state
        id 0
        goalid =goal-id
        original true
        oldletter ""
        twoback noback
        access denied

    =visual>
    +retrieval>
        isa nback-state
        goalid =goal-id
        original true
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRACKING MODEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p tracking-note-white-target
   "Done with the trial" ; just to make sure the trial ends 'gracefully'
    =goal>
        isa tracking-goal
        finished false

    =visual-location>
        isa visual-location
        kind dot
        screen-x =x
        screen-y =y
        color white
==>
    =goal>
        state stop
)
(p tracking-note-white-circle
   "Done with the trial" ; just to make sure the trial ends 'gracefully'
    =goal>
        isa tracking-goal
        finished false

    =visual-location>
        isa visual-location
        kind circle
        screen-x =x
        screen-y =y
        color white
==>
    =goal>
        state stop
)


(p tracking-reacquire-retrieve-last-position
    =goal>
        isa tracking-goal
        finished false
        state reacquire
        tpos =targetpos

    ?retrieval>
        state free
        buffer empty

    ?visual-location>
        state free
        buffer empty
==>
    ;+retrieval>
        ;isa visual-object
        ;value tracked

    +visual-location>
        isa visual-location
        kind dot
        > screen-x =min-x
        < screen-x =max-x

    !bind! =min-x (- =targetpos 10)
    !bind! =max-x (+ =targetpos 10)
    !output! (Between =min-x and =max-x)

    ;!safe-eval! (model-record-spatial-event)
)

(p tracking-reacquire-valid
    =goal>
        isa tracking-goal
        finished false
        state reacquire

    ;?visual-location>
        ;state free
        ;buffer empty

    =visual-location>
        isa visual-location
        kind dot

    ;=retrieval>
        ;isa visual-object
        ;value tracked
==>
    =goal>
        state locate-circle

    ;-visual-location>
    +visual-location>
        isa visual-location
        kind circle

    ;!safe-eval! (model-record-spatial-event)
)

(p tracking-reacquire-invalid
   =goal>
        isa tracking-goal
        finished false
        state reacquire

    ?visual-location>
        state error

    ;?retrieval>
        ;state error
==>
    =goal>
        state watching

    +visual-location>
        isa visual-location
        kind dot
    ;-visual-location>

    !safe-eval! (model-record-spatial-event)
)

(p tracking-acquire-locate-target
   "Look for the tracking target"
    =goal>
        isa tracking-goal
        finished false
        state watching
      
    ?visual-location>
        state free
        buffer empty
==>
    =goal>

    +visual-location>
        isa visual-location
        kind dot

    ;!safe-eval! (model-record-spatial-event)
)

;(p tracking-after-reacquire-acquire-locate-target
   ;"Look for the tracking target"
    ;=goal>
        ;isa tracking-goal
        ;state watching
      
    ;=visual-location>
        ;isa visual-location
        ;kind circle
;==>
    ;=goal>

    ;-visual-location>
    ;+visual-location>
        ;isa visual-location
        ;kind dot
;)

(p tracking-acquire-found-target
   "Found the target, now note its position. While encoding it, start looking for the circle"
    =goal>
        isa tracking-goal
        finished false
        state watching

    =visual-location>
        isa visual-location
        kind dot
        screen-x =x
        screen-y =y
        color black
    ;?visual>
        ;state free
        ;buffer empty
==>
    =goal>
        ;state attend-dot
        state find-circle
        tpos =x

    =visual-location>
    ;-visual>
    ;+visual>
        ;isa move-attention
        ;screen-pos =visual-location
    !safe-eval! (model-record-spatial-event)
)

;(p tracking-encode-target
   ;"Process the target visual when it has been located"
    ;=goal>
        ;isa tracking-goal
        ;state attend-dot
    ;=visual>
        ;isa visual-object
    ;;=visual-location>
        ;;isa visual-location
        ;;kind dot
;==>
    ;=goal>
        ;state find-circle
    ;;=visual-location>

    ;=visual>
;)

(p tracking-acquire-locate-circle
   "Once the target has been found, locate the circle to observe its relative position"
    =goal>
        isa tracking-goal
        finished false
        state find-circle

    =visual-location>
        isa visual-location
        kind dot
==>
    =goal>
        state locate-circle

    -visual-location>
    +visual-location>
        isa visual-location
        kind circle

    !safe-eval! (model-record-spatial-event)
)

(p tracking-acquire-found-circle
   "Found the circle, now note its position. Make a movement judgement based on both positions"
    =goal>
        isa tracking-goal
        finished false
        state locate-circle
        tpos =tx

    =visual-location>
        isa visual-location
        kind circle
        screen-x =x
        screen-y =y
        color blue
    ?visual>
        state free
        buffer empty
    !bind! =distance (my- =x =tx)
==>
    =goal>
        state moving
        cpos =x
        dist =distance

    +visual>
        isa move-attention
        screen-pos =visual-location
)


;(p tracking-encode-circle
   ;"Process the circle visual when it has been located"
    ;=goal>
        ;isa tracking-goal
        ;state attend-circle
    ;=visual>
        ;isa visual-object
    ;!bind! =trackedtarget *tracking-target-loc*
    ;!bind! =trackedcircle *tracking-circle-loc*
    ;!bind! =distance (my- =trackedcircle =trackedtarget)
;==>
    ;=goal>
        ;state moving
        ;cpos =trackedcircle
        ;tpos =trackedtarget
        ;dist =distance

    ;=visual> ; maintain attention
;)

(p tracking-adjust-fast-right
   "The circle is further from the target than the 'safe' boundary value, so decide to move the circle"
    =goal>
        isa tracking-goal
        finished false
        state moving
        cpos =circlepos
        < trackeddist =threshold

        dist =curdist
        trackedpos =trackpos
        trackeddist =trackdist

    ?manual>
        preparation free
        processor free

    =visual>
        isa visual-object

    !bind! =threshold -10
    ;!bind! =threshold (- *model-max-allowed-tracking-drift*)
    !bind! =newcirclepos (+ =circlepos 15)

==>
    =goal>
        state moving
        cpos =newcirclepos
        
    +manual>
        isa punch
        hand right
        finger middle

    =visual> ; maintain until move is done
    !output! (Circle =circlepos New circle =newcirclepos)
    !output! (MR Distance before =curdist); Comp distance =distance)
    !output! (Trackeddist =trackdist Trackedpos =trackpos)

    ;!safe-eval! (model-record-hand 0)
    !safe-eval! (push (mp-time) *motor-right-hand*)
)

(p tracking-adjust-fast-left
   "Quickly tap the left key to adjust the circle position to the target"
    =goal>
        isa tracking-goal
        finished false
        state moving
        cpos =circlepos
        > trackeddist =threshold

        dist =curdist
        trackedpos =trackpos
        trackeddist =trackdist

    ?manual>
        preparation free
        processor free

    =visual>
        isa visual-object

    !bind! =threshold 10
    ;!bind! =newdist (my- *tracking-circle-loc* *tracking-target-loc*)
    !bind! =newcirclepos (- =circlepos 15)

==>
    =goal>
        state moving
        cpos =newcirclepos

    +manual>
        isa punch
        hand right
        finger index

    =visual> ; maintain until move is done
    !output! (Circle =circlepos New circle =newcirclepos)
    !output! (ML Distance before =curdist );Comp distance =distance)
    !output! (Trackeddist =trackdist Trackedpos =trackpos)

    ;!safe-eval! (model-record-hand 0)
    !safe-eval! (push (mp-time) *motor-right-hand*)
)

(p tracking-adjust-slow-observe-right
   "When very close to the target, stop quickly tapping, but do careful taps"
    =goal>
        isa tracking-goal
        finished false
        state moving
        >= trackeddist -10
        < trackeddist -5

        dist =curdist
        trackedpos =trackpos
        trackeddist =trackdist

    ?manual>
        preparation free
        processor free
        execution free

    =visual>
        isa visual-object

==>
    !output! (MR Distance =curdist )
    !output! (Trackeddist =trackdist Trackedpos =trackpos)
    =goal>
        state carefulmove

    =visual>
)

(p tracking-adjust-slow-observe-left
   "When very close to the target, stop quickly tapping, but do careful taps"
    =goal>
        isa tracking-goal
        finished false
        state moving
        <= trackeddist 10
        > trackeddist 5

        dist =curdist
        trackedpos =trackpos
        trackeddist =trackdist

    =visual>
        isa visual-object
    ?manual>
        preparation free
        processor free
        execution free

==>
    !output! (ML Distance =curdist )
    !output! (Trackeddist =trackdist Trackedpos =trackpos)
    =goal>
        state carefulmove

    =visual>
)

(p tracking-adjust-slow-right
    =goal>
        isa tracking-goal
        finished false
        state carefulmove
        cpos =circlepos
        < trackeddist -5

        dist =curdist
        trackedpos =trackpos
        trackeddist =trackdist

    ?manual>
        preparation free
        processor free
        execution free

    =visual>
        isa visual-object

    !bind! =threshold -10
    ;!bind! =threshold (- *model-max-allowed-tracking-drift*)
    !bind! =newcirclepos (+ =circlepos 15)

==>
    =goal>
        state moving
        cpos =newcirclepos
        
    +manual>
        isa punch
        hand right
        finger middle

    =visual> ; maintain until move is done
    !output! (Circle =circlepos New circle =newcirclepos)
    !output! (MR Distance before =curdist); Comp distance =distance)
    !output! (Trackeddist =trackdist Trackedpos =trackpos)

    ;!safe-eval! (model-record-hand 0)
    !safe-eval! (push (mp-time) *motor-right-hand*)
)

(p tracking-adjust-slow-left
    =goal>
        isa tracking-goal
        finished false
        state carefulmove
        cpos =circlepos
        > trackeddist 5

        dist =curdist
        trackedpos =trackpos
        trackeddist =trackdist

    ?manual>
        preparation free
        processor free
        execution free

    =visual>
        isa visual-object

    !bind! =threshold 10
    ;!bind! =threshold *model-max-allowed-tracking-drift*
    !bind! =newcirclepos (- =circlepos 15)

==>
    =goal>
        state moving
        cpos =newcirclepos

    +manual>
        isa punch
        hand right
        finger index

    =visual> ; maintain until move is done
    !output! (Circle =circlepos New circle =newcirclepos)
    !output! (ML Distance before =curdist); Comp distance =distance)
    !output! (Trackeddist =trackdist Trackedpos =trackpos)

    ;!safe-eval! (model-record-hand 0)
    !safe-eval! (push (mp-time) *motor-right-hand*)
)

(p tracking-adjust-slow-stop
   "Decide not to move the circle to the target, as it is still within the distance that is percieved as safe"
   =goal>
    isa tracking-goal
    finished false
    state carefulmove
    <= trackeddist =threshold
    >= trackeddist =mintresh
    trackedpos =trackpos

    =visual>
        isa visual-object
        value circle

    ?retrieval>
        buffer empty
        state free

    !bind! =threshold *model-max-allowed-tracking-drift*
    !bind! =mintresh (- *model-max-allowed-tracking-drift*)
==>
    =goal>
        state reacquire
        tpos =trackpos

    =visual>
        value tracked
    -visual> ; stop attending

    !output! (Remembered pos =trackpos)
    ;+retrieval>
        ;isa visual-object
        ;value noted

)

(p tracking-adjust-quick-stop
   "Decide not to move the circle to the target, as it is still within the distance that is percieved as safe"
   =goal>
    isa tracking-goal
    finished false
    state moving
    <= trackeddist =threshold
    >= trackeddist =mintresh
    trackedpos =trackpos

    =visual>
        isa visual-object
        value circle

    ?retrieval>
        buffer empty
        state free

    !bind! =threshold *model-max-allowed-tracking-drift*
    !bind! =mintresh (- *model-max-allowed-tracking-drift*)
==>
    =goal>
        state reacquire
        tpos =trackpos

    =visual>
        value tracked
    -visual> ; stop attending

    !output! (Remembered pos =trackpos)
    ;+retrieval>
        ;isa visual-object
        ;value noted

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TONE COUNTING MODEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p counting-start
   "Initialize count to zero by loading a new counting state into imaginal"
    =goal>
        isa counting-goal
        state start
    ?imaginal>
        state free
        ;buffer empty
==>
    =goal>
        state listening
        heard false
        storedcount "0"
    +imaginal>
        isa counting-state
        goal =goal
        count "0"
        access open
)

(p counting-switch-im
   "Switch the content of the imaginal buffer"
    =goal>
        isa counting-goal
    =imaginal>
        isa counting-state
        access denied
    ?imaginal>
        state free

    =retrieval>
        isa counting-state
        count =num
==>
    =goal>
    =imaginal>
        count =num
        access locked

    =retrieval>
        goal nil ; don't merge this chunk with the existing one: that would inflate the activation
    -retrieval>
    ;!eval! (format t "Retrieved ~a " =num)
)

(p counting-switch-im-failed
    =goal>
        isa counting-goal
    =imaginal>
        isa counting-state
        access denied
    ?imaginal>
        state free

    ?retrieval>
        state error
==>
    =goal>
    =imaginal>
        access locked
)
   

(p counting-heard-a-tone
   "Begin encoding the stuffed audio event"
    =goal>
        isa counting-goal
        heard false

    ?manual>
        state free
        buffer empty
    =aural-location>
        isa audio-event
    ?aural>
        state free
        buffer empty
==>
    =goal>
        ;state hearing
    +aural>
        isa sound
        event =aural-location
)
;(spp counting-heard-a-tone :u 4.1) 


; sometimes you just mess up in counting! Probably because its so boring
;(p counting-heard-a-tone-fumble
   ;"Miss a tone by accident"
    ;=goal>
        ;isa counting-goal
        ;heard false

    ;?manual>
        ;state free
        ;buffer empty
    ;=aural-location>
        ;isa audio-event
    ;?aural>
        ;state free
        ;buffer empty
;==>
    ;=goal>
        ;;state hearing
;)
;(spp counting-heard-a-tone-fumble :u 1) 

(p counting-high-tone
   "Only the high tones should be counted"
    =goal>
        isa counting-goal
        state listening

    ;?manual>
        ;state free
        ;buffer empty
    =aural>
        isa sound
        kind tone
        content 1250 ; high tone
==>
    =goal>
        state startcount
)
(spp counting-high-tone :u 4.0) 

(p counting-high-tone-fumble
   "Only the high tones should be counted"
    =goal>
        isa counting-goal
        state listening

    ;?manual>
        ;state free
        ;buffer empty
    =aural>
        isa sound
        kind tone
        content 1250 ; high tone
==>
    =goal>
        state listening
)
(spp counting-high-tone-fumble :u 1) 

(p counting-low-tone
   "Ignore the low tones"
    =goal>
        isa counting-goal
        state listening

    ;?manual>
        ;state free
        ;buffer empty
    =aural>
        isa sound
        kind tone
        content 500
==>
    =goal>
        state listening
)
(spp counting-low-tone :u 3.6) 

;(p counting-low-tone-fumble
   ;"Ignore the low tones"
    ;=goal>
        ;isa counting-goal
        ;state listening

    ;;?manual>
        ;;state free
        ;;buffer empty
    ;=aural>
        ;isa sound
        ;kind tone
        ;content 500
;==>
    ;=goal>
        ;state startcount
;)
;(spp counting-low-tone-fumble :u 1) 

(p counting-trial-done-tone
    =goal>
        isa counting-goal

    =aural>
        isa sound
        kind tone
        content 2000
==>
    =goal>
        state answer
)
(spp counting-trial-done-tone :u 1000) 

(p counting-queue-tone
    =goal>
        isa counting-goal
        - state listening

    ;?manual>
        ;state free
        ;buffer empty
    =aural>
        isa sound
        kind tone
==>
    =goal>
        heard =aural
    -aural>
)

(p counting-retrieve-queued-tone
    =goal>
        isa counting-goal
        state listening
        - heard false
        heard =tone
    =imaginal>
        isa counting-state
        count =num
        - access denied
    ?retrieval>
        buffer empty
        state free
==>
    =imaginal> ; don't open access here yet, do the actual count update first
        access locked
    +retrieval>
        =tone
)

(p counting-retrieve-queued-tone-wrong-im
    =goal>
        isa counting-goal
        storedcount =storedcount
        state listening
        - heard false
        heard =tone
    =imaginal>
        isa nback-state
        access open
    ?retrieval>
        buffer empty
        state free

    !bind! =guess (model-counting-guess =storedcount)
==>
    -imaginal>
    +imaginal> ; create a placeholder im chunk to keep the buffer busy
        isa counting-state
        goal =goal
        count =guess
        access denied
    +retrieval>
        isa counting-state
        goal =goal
)

(p counting-retrieve-queued-tone-failed
    =goal>
        isa counting-goal
        state listening
        - heard false
    =imaginal>
        isa counting-state
        - access denied
    ?retrieval>
        state error
==>
    =imaginal>
    =goal>
        heard false
)

(p counting-retrieve-queued-tone-low
    =goal>
        isa counting-goal
        state listening
        - heard false
    =imaginal>
        isa counting-state
        - access denied
    =retrieval>
        isa sound
        kind tone
        content 500
==>
    =goal>
        heard false
    =imaginal>
    -retrieval>
)

(p counting-retrieve-queued-tone-high
    =goal>
        isa counting-goal
        state listening
        - heard false
    =imaginal>
        isa counting-state
        - access denied
    =retrieval>
        isa sound
        kind tone
        content 1250
==>
    =goal>
        state startcount
        heard false
    =imaginal>
    -retrieval>
)

(p counting-start-count-inc
   "The tone was the correct type, so begin retrieval of the new count value"
    =goal>
        isa counting-goal
        state startcount
    =imaginal>
        isa counting-state
        count =num
        - access denied

    ?manual>
        state free
        buffer empty
    ?retrieval>
        state free
        buffer empty
==>
    =goal>
        state finishcount
    =imaginal> ; don't open access here yet, do the actual count update first
        access locked

    +retrieval>
        isa number-order-fact
        value =num
)

(p counting-start-count-inc-wrong-im
    =goal>
        isa counting-goal
        storedcount =storedcount
        state startcount
    =imaginal>
        isa nback-state
        access open

    ?retrieval>
        state free
        buffer empty

    !bind! =guess (model-counting-guess =storedcount)
==>
    =goal>
    +imaginal> ; create a placeholder im chunk to keep the buffer busy
        isa counting-state
        goal =goal
        count =guess
        access denied

    +retrieval>
        isa counting-state
        goal =goal
)

(p counting-update-count
   "Update the count to the new value and continue listening for new tones"
    =goal>
        isa counting-goal
        state finishcount
    =imaginal>
        isa counting-state
        - access denied

    =retrieval>
        isa number-order-fact
        next =newcount
    ?retrieval>
        state free
==>
    =goal>
        state listening
        ;state rehearse
        storedcount =newcount
    =imaginal>
        goal xxx
    +imaginal>
        count =newcount
        goal =goal
        access open ; after the whole count update, make the im buffer available to other tasks
    ; rehearse counting
    ;-imaginal>
    ;+imaginal>
        ;isa counting-state
        ;goal =goal
        ;count =newcount
        ;access locked
    ;+retrieval>
        ;isa counting-state
        ;goal =goal
        ;count =newcount
)

(p counting-rehearsal
    =goal>
        isa counting-goal
        state rehearse
    =imaginal>
        isa counting-state
        - access denied

    =retrieval>
        isa counting-state
        count =newcount
==>
    =goal>
        state listening
    =imaginal>
        access open
    -retrieval>
)

(p counting-rehearsal-error
    =goal>
        isa counting-goal
        state rehearse
    =imaginal>
        isa counting-state
        - access denied

    ?retrieval>
        state error
==>
    =goal>
        state listening
    =imaginal>
        access open
)

;(p counting-wrong-im-start-rehearse
    ;=goal>
        ;isa counting-goal
        ;imrehearse false
    ;=imaginal>
        ;isa nback-state
        ;- access open

    ;?retrieval>
        ;state free
        ;buffer empty
;==>
    ;=goal>
        ;imrehearse true
    ;=imaginal>
    ;+retrieval>
        ;isa counting-state
        ;goal =goal

;)

;(p counting--wrong-im-rehearsal-error
    ;=goal>
        ;isa counting-goal
        ;imrehearse true
    ;=imaginal>
        ;isa nback-state

    ;?retrieval>
        ;state error
;==>
    ;=goal>
        ;imrehearse false
    ;=imaginal>
;)

;(p counting--wrong-im-rehearsal-success
    ;=goal>
        ;isa counting-goal
        ;imrehearse true
    ;=imaginal>
        ;isa nback-state

    ;=retrieval>
        ;isa counting-state
;==>
    ;-retrieval>
    ;=goal>
        ;imrehearse false
    ;=imaginal>
;)

(p counting-update-count-wrong-im
   "Large interference here, get the imaginal and just try to do the number retrieval again"
    =goal>
        isa counting-goal
        storedcount =storedcount
        imrehearse false
        state finishcount
    =imaginal>
        isa nback-state
        access open

    =retrieval>
        isa number-order-fact
        next =newcount

    !bind! =guess (model-counting-guess =storedcount)
==>
    =goal>
    +imaginal>
        isa counting-state
        goal =goal
        count =guess
        access denied

    +retrieval>
        isa counting-state
        goal =goal
)

(p counting-update-count-failed
   "Couldn't retrieve the count fact for some reason (should not happen; number-order-facts have huge activation)"
    =goal>
        isa counting-goal
        storedcount =storedcount
        imrehearse false
        state finishcount
    =imaginal>
        isa counting-state
        - access denied

    ?retrieval>
        state error

    !bind! =guess (model-counting-guess =storedcount)
==>
    =goal>
        state listening
    =imaginal>
        count =guess
        access open ; after the whole count update, make the im buffer available to other tasks
)

; COUNTING - ANSWER PROMP

(p counting-start-answer
    =goal>
        isa counting-goal
        state answer
    ;?visual-location>
        ;state free
        ;buffer empty
==>
    =goal>

    +visual-location>
        isa visual-location
        kind text
        color black
)

(p counting-answer-found-promp
    =goal>
        isa counting-goal
        state answer

    =visual-location>
        isa visual-location
        kind text
        color black
        screen-x =x
        screen-y =y

    ;?visual>
        ;state free
        ;buffer empty

    !bind! =scrc (float (/ *screen-w* 2)) ; < center = left handed
==>
    =goal>
        state answer-promp

    +visual>
        isa move-attention
        screen-pos =visual-location

    !output! (Found promp at =x by =y center =scrc)
)

(p counting-answer-attended-promp
    =goal>
        isa counting-goal
        state answer-promp
        storedcount =count

    =visual>
        isa visual-object
        value =txt

    !bind! =ans (parse-integer =count)
==>
    =goal>
        state answer-press
        ansremain =ans
    !output! (Seen =txt)
)

(p counting-answer-enter-ten
    =goal>
        isa counting-goal
        state answer-press
        > ansremain 9
        ansremain =ans

    ?manual>
        state free
        buffer empty

    !bind! =newremain (- =ans 10)
    !bind! =hand (if (= *counting-response-hand* 0) 'right 'left)
    !bind! =finger (if (= *counting-response-hand* 0) 'index 'middle)
==>
    =goal>
        ansremain =newremain
        state answer-check

    +visual-location>
        isa visual-location
        kind text
        color black

    +manual>
        isa punch
        hand =hand
        finger =finger

    ;!output! (press ten)
    !safe-eval! (model-record-hand *counting-response-hand*)
)

(p counting-answer-enter-one
    =goal>
        isa counting-goal
        state answer-press
        > ansremain 0
        < ansremain 10
        ansremain =ans

    ?manual>
        state free
        buffer empty

    !bind! =newremain (- =ans 1)
    !bind! =hand (if (= *counting-response-hand* 0) 'right 'left)
    !bind! =finger (if (= *counting-response-hand* 0) 'middle 'index)
==>
    =goal>
        ansremain =newremain
        state answer-check

    +visual-location>
        isa visual-location
        kind text
        color black

    +manual>
        isa punch
        hand =hand
        finger =finger

    ;!output! (press one)
    !safe-eval! (model-record-hand *counting-response-hand*)
)

(p counting-answer-check-promp-change
    =goal>
        isa counting-goal
        state answer-check

    =visual-location>
        isa visual-location
        kind text
        color black
        screen-x =x
        screen-y =y
==>
    +visual>
        isa move-attention
        screen-pos =visual-location
)

(p counting-answer-attend-promp-change
    =goal>
        isa counting-goal
        state answer-check

    =visual>
        isa visual-object
        value =txt
==>
    =goal>
        state answer-press
)

;;; END
)
 
(defun model-reset ()
  (verify-current-mp  
   "reset called with no current meta-process."
    (let ((mp (current-mp)))
     ;(reset-mp mp)
      (setf (meta-p-events mp) nil)
      (setf (meta-p-delayed mp) nil)
      
      (maphash #'(lambda (name model)
                   (declare (ignore name))
                   (reset-my-model mp model))
               (meta-p-models mp)))))


(defun reset-my-model (mp model)
 
  (let ((previous-model (meta-p-current-model mp)))
    (setf (meta-p-current-model mp) model)
    
    (clrhash (act-r-model-chunk-types-table model))
    (clrhash (act-r-model-chunks-table model))
    (clrhash (act-r-model-chunk-ref-table model))
    
    (setf (act-r-model-chunk-update model) t)
    (setf (act-r-model-dynamic-update model) t)
    (setf (act-r-model-delete-chunks model) nil)
    
    (setf (act-r-model-largest-chunk-type model) 0)
    
    (maphash #'(lambda (buffer-name buffer)
                 (declare (ignore buffer-name))
                 (setf (act-r-buffer-chunk buffer) nil))
             (act-r-model-buffers model))
    
    (create-model-default-chunk-types-and-chunks)
    
    (maphash #'(lambda (module-name instance)
                 (declare (ignore instance))
                  (if (not (equalp module-name 'buffer-trace)) (reset-module module-name)))
             (global-modules-table))
    
    (maphash #'(lambda (parameter-name parameter)
                 (sgp-fct (list parameter-name (act-r-parameter-default parameter))))
             *act-r-parameters-table*)
    
    
    (maphash #'(lambda (module-name val)
                 (declare (ignore val))
                  (if (not (equalp module-name 'buffer-trace)) (secondary-reset-module module-name)))
             (global-modules-table))    
    
    (dolist (form (act-r-model-code model))
      (eval form))
    
    (maphash #'(lambda (module-name val)
                 (declare (ignore val))
                  (if (not (equalp module-name 'buffer-trace))(tertiary-reset-module module-name)))
             (global-modules-table))
    
    (setf (meta-p-current-model mp) previous-model)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mp-trial-time ()
    (- (mp-time) *trial-start-t*)
)

(defun one-of (set)
    "Pick one element of set, and make a list of it."
    (list (random-elt set))
)

(defun random-elt (choices)
    "Choose an element from a list at random."
    (elt choices (act-r-random (length choices)))
)

(defun scramble (sequence) ;scrambles any sequence, including strings: "jelmer" -> "eljrme"
  (setf sequence (copy-seq sequence))
  (let ((length (length sequence)))
    (dotimes (i (1- length) sequence)
      (rotatef
       (elt sequence i)
       (elt sequence (act-r-random (+ i (- length  i))))))))

; create a list with the values from start until end
(defun range (start end) 
        (loop for i from start below end collect i)) 

(defun remove-nth (n list)
  (remove-if (constantly t) list :start n :end (1+ n)))

(defun all-positions (needle haystack)
  (loop
    for element in haystack 
    and position from 0
     when (eql element needle)
      collect position))

; get the average from a list of numbers
(defun average (lst)
  (let ((lst (remove nil lst)))
    (if (null lst)
        nil
        (let ((sum 0))
          (dolist (x lst) (incf sum x))
          (float (my/ sum (length lst))))))
)

(defun variance (samples)
    (let ((samples (remove nil samples)))
    (if (null samples)
        nil
        (let* ((n (length samples))
         (mean (my/ (reduce #'+ samples) n))
         (tmp (mapcar (lambda (x) (sqr (- x mean))) samples)))

        (my/ (reduce #'+ tmp) n)
        )
    ))
)

(defun std-dev (samples)
    (let ((samples (remove nil samples)))
    (if (null samples)
        nil
        (sqrt (variance samples))
    ))
)

(defun conf-interval95 (x side)
    (let ((x (remove nil x)))
    (if (null x)
        nil
        (let (
            (mu (average x))
            (sd (std-dev x))
            (n (length x))
            (t-value 1.96)
            )

            (case side
                ('lower (- mu (* t-value (my/ sd (sqrt n)))))
                ('upper (+ mu (* t-value (my/ sd (sqrt n)))))
            )
        )
    ))
)

(defun sample-corr
       (seq-1
        seq-2
        &key
        (start 0)
        )

    (let ((seq-1 (remove nil seq-1)) (seq-2 (remove nil seq-2)))
    (if (or (< (length seq-1) 2) (< (length seq-2) 2))
        nil
        (let* ((sum 0.0)
              (end (length seq-1))
                (mean-1 (average seq-1))
                (mean-2 (average seq-2))
                (variance-1 (variance seq-1))
                (variance-2 (variance seq-2))
                (n (- end start))
                (j (1- start)))
            (dotimes (i n (my/ (my/ sum n) (sqrt (* variance-1 variance-2))))
              (incf sum (* (- (elt seq-1 (incf j)) mean-1)
                           (- (elt seq-2 j) mean-2))))
        )
    ))
)

; root mean square error
(defun rmse (lst &optional (xx 0))
  (let ((lst (remove nil lst)))
    (sqrt (my/ (let ((sum 0))
                 (dolist (x lst) (incf sum (* (- x xx) (- x xx))))
                 sum)
               (length lst)))))

(defun mae (seq-1 seq-2 &key (n (length seq-1)))
    (let ((seq-1 (remove nil seq-1)) (seq-2 (remove nil seq-2)))
    (if (or (< (length seq-1) 2) (< (length seq-2) 2))
        nil
        (let* (
            (sum 0.0)
            (j -1)
            )
            (dotimes (i n)
                (incf sum (abs (- (elt seq-1 (incf j)) (elt seq-2 j))))
            )

            (my/ sum n)
        )
    ))


)

(defun compute-error (data)
  (rmse (mapcar #'(lambda (lst)
                    ;; in ron chong's system, degrees_to_pixels = .125
                    (/ (pm-pixels-to-angle (sqrt (+ (sqr (first lst)) (sqr (second lst))))) .125))
                data)))

(defun round-to (number precision &optional (what #'round))
    (if number
        (let ((div (expt 10 precision)))
            (float (/ (funcall what (* number div)) div))
        )
        nil
    )
)

(defun assoc-vals (key alist)
    (cdr (assoc key alist))
)

(defun values-only (alist)
    (mapcar 'cdr alist)
)

(defun avg-values-only (alist)
    (mapcar 'average (mapcar 'cdr alist))
)

(defun my- (x y)
    (let* (
        (xx (if (not x) 0 x))
        (yy (if (not y) 0 y))
        )
        (- xx yy)
    )
)

(defun sum (x) (apply '+ x))
(defun my/ (x y) (if (zerop y) 0 (/ x y)))
(defun sqr (x) (let ((v x)) (* v v)))

;(defun slurp-stream (stream)
  ;(with-output-to-string (out)
    ;(loop
     ;(multiple-value-bind (line nl) (read-line stream nil stream)
       ;(when (eq line stream)
         ;(return))
       ;(write-string line out)
       ;(unless nl
         ;(write-char #Newline out))))))

(defun read-a-table (stream numcols)
  (cons (loop repeat numcols collect (read stream))
        (loop while (peek-char t stream nil nil)
              collect (loop repeat numcols collect (read stream)))))

(defun read-a-table-from-file (file numcols)
  (with-open-file (stream file)
    (read-a-table stream numcols)))

;(defun file-to-string (path)
  ;(with-open-file (stream path)
    ;(let ((data (make-string (file-length stream))))
      ;(read-sequence data stream)
      ;data))
;)

;(defun parse-csv (string &optional (sep #\,) &aux (state 0) (pos 0) (results (list (list ""))))
  ;(labels ((noop   (c) (declare (ignore c)))
           ;(ship   (c) (declare (ignore c)) (push (list "") results))
           ;(addc   (c) (setf (caar results) (concatenate 'string (caar results) (list c))))
           ;(next   (c) (declare (ignore c)) (setf results (cons (cons "" (car results)) (cdr results))))
           ;(addlf  (c) (declare (ignore c)) (addc #\Newline))
           ;(instlf (c) (declare (ignore c)) (addlf) (ship))
           ;(err    (c) (error "CSV Parse error, quoted value ended with ~a instead of ~a at position ~d" c sep pos)))
    ;(let ((fn (make-array (* (* 6 2) 7) :initial-contents
                          ;;;    WHITE,    RETURN,     LF,         QUOTE,     SEP,       OTHER
                          ;(list #'noop 0  #'ship 1    #'ship 0    #'noop 3   #'next 0   #'addc 2 ;; Start
                                ;#'noop 0  #'ship 1    #'noop 0    #'noop 0   #'next 0   #'addc 2 ;; Seen Return
                                ;#'addc 2  #'ship 1    #'ship 0    #'addc 2   #'next 0   #'addc 2 ;; Unquoted
                                ;#'addc 3  #'addlf 4   #'addlf 3   #'noop 5   #'addc 3   #'addc 3 ;; Quoted
                                ;#'addc 3  #'addlf 4   #'noop 3    #'noop 5   #'addc 3   #'addc 3 ;; Quoted seen Return
                                ;#'noop 6  #'ship 1    #'ship 0    #'addc 3   #'next 0   #'err  0 ;; Quoted seen quote.
                                ;#'noop 6  #'ship 1    #'ship 0    #'err  0   #'next 0   #'err  0 ;; Quoted seen quote space
                                ;))))
      ;(loop for char across string
            ;for class = (case char (#\Space 0) (#\Return 1) (#\Linefeed 2) (#\" 3) (otherwise (if (char= char sep) 4 5)))
            ;do (funcall (elt fn (+ (* class 2) (* state 12))) char)
            ;do (incf pos)
            ;do (setf state (elt fn (+ 1 (* class 2) (* state 12))))
            ;finally (return (remove '("") (nreverse (mapcar #'nreverse results)) :test #'equalp))
      ;)
    ;)
  ;)
;)
