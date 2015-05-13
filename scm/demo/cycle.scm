;; cycle.scm --- Cycle through some windows
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Demo of how you can use timer events to do something at regular
;; intervalls, in this case to automatically cycle through a set of
;; windows of a specified type.
;; Start with (cycle-wins TYPE TIME) where TYPE matches the window type
;; and TIME is the delay, and stop with (cycle-stop TYPE).
;;
;; A more useful action would perhaps be to change the screen background.
;; See e.g. the "caleido.scm" example.
;;

(define (cycle-lowest cnd)
  (or-map (lambda (w)
            (if (matches-token w cnd)
                w
                #f))
          (list-of-windows 'mapped 'window 'stacking-order)))

(define cycle-behavior 
  (let ((stop-list '()))
    (make-behavior (on (user-event 'cycle-step)
                       (let* ((cnd (cadr (event-data event)))
                              (time (caddr (event-data event)))
                              (win (cycle-lowest cnd)))
                         (if (member cnd stop-list)
                             (set! stop-list (delete cnd stop-list))
                             (if win
                                 (begin
                                   (send-timer-event (list 'cycle-step cnd time) deco time)
                                   (raise-window win))))))
                   (on (user-event 'cycle-stop)
                       (let ((cnd (cadr (event-data event))))
                         (if (not (member cnd stop-list))
                             (set! stop-list (cons cnd stop-list))))))))

(define (cycle-wins cnd time)
  (send-user-event (list 'cycle-step cnd time) (screen)))

(define (cycle-stop cnd)
  (send-user-event (list 'cycle-stop cnd) (screen)))

(modify-screen-behavior cycle-behavior)
