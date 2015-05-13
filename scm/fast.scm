;; fast.scm --- Fast decorations (minimal)
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;; A minimal profile, to invoke with "gwm -f fast". The original idea is
;; from Jay Berkenbilt (ejb@ql.org), adjusted for gwm-2.0 by Anders Holst.

(define modifiers shift-mask)

(define (user-move-window win ev)
  (let ((xoff (- (window-x win) (event-x ev)))
        (yoff (- (window-y win) (event-y ev))))
    (with-user-feedback (lambda (e)
                          (move-window win (+ xoff (event-x e)) (+ yoff (event-y e))))
                        ev :no-freeze #t)))

(define (describe-window win)
  (modify-deco win
               :behavior (make-behavior
                          (on (button 1 modifiers)
                              (lower-window deco)
                              :steal #t)
                          (on (button 2 modifiers)
                              (user-move-window deco event)
                              :steal #t)
                          (on (button 3 modifiers)
                              (raise-window deco)
                              :steal #t)))
  win)

(define (describe-icon win)
  (make-deco (make-label (window-icon-name win))
             :behavior (make-behavior
                        (on (button 1 modifiers)
                            (deiconify-window deco))
                        (on (button 2 modifiers)
                            (user-move-window deco event)))))

(define (describe-screen win)
  (set-deco-behavior! win 
                      (make-behavior
                       (on (button 3 modifiers)
                           (wait-for-release event)
                           (end))
                       (on (opening)
                           (bell))))
  win)
