;; tooltip.scm --- Add tooltips strings to decorations
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This is an example of how to add tooltips to decorations, i.e. make
;; a decoration show a descriptive text when it is entered (and the
;; mouse don't move for enought time). 
;;
;; Call (tooltip-install DECO STRING) on the decoration to install a
;; tooltip string.
;;

(define (tooltip-launch str)
  (let ((pos (pointer-position))
        (d (make-deco (make-label str) :background (make-color "lightyellow") :borderwidth 1)))
    (place-menu d (screen) (car pos) (- (cadr pos) (height d) 2) :decoration id)))

(define (tooltip-behavior str)
  (let ((entered #f)
        (moved #f)
        (waiting #f)
        (done #f)
        (tt #f))
  (make-behavior (on (enter)
                     (set! entered #t)
                     (set! moved waiting))
                 (on (movement)
                     (if (not done)
                         (if waiting
                             (set! moved #t)
                             (begin
                               (set! waiting #t)
                               (set! moved #f)
                               (send-timer-event 'tt deco 0.3)))))
                 (on (leave)
                     (set! entered #f)
                     (set! done #f)
                     (if tt
                         (begin
                           (delete-window tt)
                           (set! tt #f))))
                 (on (user-event 'tt)
                     (cond ((or (not entered)
                                done)
                            (set! moved #f)
                            (set! waiting #f))
                           (moved
                            (set! moved #f)
                            (send-timer-event 'tt deco 0.3))
                           (#t
                            (set! waiting #f)
                            (set! done #t)
                            (set! tt (tooltip-launch str))))))))

(define (tooltip-install deco str)
  (if (deco-behavior deco)
      (modify-behavior (deco-behavior deco) (tooltip-behavior str))
      (set-deco-behavior! deco (tooltip-behavior str))))
