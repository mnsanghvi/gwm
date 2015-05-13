;; timer.scm --- Run some code after a while, or repeatedly
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Example of how to implement a simple timer in Gwm.
;; (delayed-eval DELAY THUNK) makes THUNK be run after DELAY seconds.
;; (repeated-eval DELAY THUNK) makes THUNK be run every DELAY seconds,
;; until the function returned by the call is called.
;;

(define timer-arc (on (user-event 'global-timer)
                      (let ((d (event-data event)))
                        (if (and d (pair? d) (procedure? (cdr d)))
                            ((cdr d))))))

(define (delayed-eval delay thunk)
  (send-timer-event (cons 'global-timer thunk) (screen) delay))

(define (repeated-eval delay thunk)
  (letrec ((block #f)
           (func (lambda ()
                   (if (not block) 
                       (begin
                         (send-timer-event (cons 'global-timer func) (screen) delay)
                         (thunk)))))
           (remove (lambda () (set! block #t))))
    (send-timer-event (cons 'global-timer func) (screen) delay)
    remove))
                          
(modify-screen-behavior timer-arc) 

(define timer #t)

;; Example application - pop up a reminder note in a number of seconds
(define (reminder-launch str)
  (let* ((beh (make-behavior (on (button any any) (delete-window deco))))
         (d (make-deco (make-label str
                                   :font (make-font "10x20")
                                   :background (make-color "orange")
                                   :horizontal-margin 20
                                   :vertical-margin 20)
                       :behavior beh)))
    (place-menu d (screen)
                (quotient (+ (width (screen)) (width d)) 2)
                (quotient (+ (height (screen)) (height d)) 2))
    (bell)))

(define (reminder seconds message)
  (delayed-eval seconds (lambda () (reminder-launch message))))


