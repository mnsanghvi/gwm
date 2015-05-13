;; clip-window.scm --- Window decoration to show only part of a client window
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This is a demo decoration to illustrate how you can make a window 
;; that shows only a small part of a client decoratoin, for example to
;; be able to put two large windows next to each other and see the
;; relevant parts of both.
;;
;; Shift-rightbutton resizes the inner client, shift-leftbutton moves
;; the inner client inside the decoration, just rightbutton resizes
;; the outer window, and just leftbutton moves the outer window.
;;
;; This decoration has no other visible attributes, and would perhaps
;; suit the best inside another decoration.
;;


(define clip-window-width (quotient (screen-width) 3))

(define clip-window-height (quotient (screen-height) 2))

(define (clip-window-behavior)
  (make-behavior 
   (on-event (button 3 shift-mask) ;; inner resize
             (lambda (deco event)
               (let ((xoff (- (event-x event)))
                     (yoff (- (event-y event)))
                     (ix (deco-x (deco-part deco 1)))
                     (iy (deco-y (deco-part deco 1)))
                     (iw (deco-width (deco-part deco 1)))
                     (ih (deco-height (deco-part deco 1)))
                     (ow (deco-width deco))
                     (oh (deco-height deco))
                     (res #f))
                 (set! res (with-user-feedback 
                            (lambda (ev)
                              (draw-rubber-rectangle (screen) ix iy
                                                     (max (+ (event-x ev) xoff iw) ow)
                                                     (max (+ (event-y ev) yoff ih) oh)))
                            event
                            :no-freeze #t))
                 (resize-window deco
                                (+ (max (+ (event-x res) xoff iw) ow) (- ow iw))
                                (+ (max (+ (event-y res) yoff ih) oh) (- oh ih)))))
             :steal #t)
   (on-event (button 1 shift-mask) ;; inner move
             (lambda (deco event)
               (let ((xoff (- (event-x event) (deco-x deco) (car (dimensions (deco-part deco 1)))))
                     (yoff (- (event-y event) (deco-y deco) (cadr (dimensions (deco-part deco 1)))))
                     (w1 (deco-width deco))
                     (h1 (deco-height deco))
                     (x1 (deco-x deco))
                     (y1 (deco-y deco))
                     (w2 (deco-width (deco-part deco 1)))
                     (h2 (deco-height (deco-part deco 1))))
                 (with-user-feedback 
                  (lambda (ev)
                    (let ((x (- (event-x ev) (deco-x deco)))
                          (y (- (event-y ev) (deco-y deco)))
                          (xfact 0) (yfact 0))
                      (set! xfact (if (= w1 w2) 0.5 (max 0.0 (min 1.0 (/ (- xoff x) (- w2 w1))))))
                      (set! yfact (if (= h1 h2) 0.5 (max 0.0 (min 1.0 (/ (- yoff y) (- h2 h1))))))
                      (modify-deco (deco-part deco 1) :anchor (list (list xfact 0 xfact) (list yfact 0 yfact)))
                      (draw-rubber-rectangle (screen)
                                             (min x1 (max (- (+ x1 w1) w2) (- (event-x ev) xoff)))
                                             (min y1 (max (- (+ y1 h1) h2) (- (event-y ev) yoff)))
                                             w2 h2)))
                  event
                  :no-freeze #t)))
             :steal #t)
   (on-event (button 3 alone) ;; outer resize
             (lambda (deco event)
               (let ((xoff (- (event-x event) (deco-x deco) (deco-width deco)))
                     (yoff (- (event-y event) (deco-y deco) (deco-height deco))))
                 (with-user-feedback 
                  (lambda (ev)
                    (let ((x (- (event-x ev) (deco-x deco)))
                          (y (- (event-y ev) (deco-y deco)))
                          (w2 (deco-width (deco-part deco 1)))
                          (h2 (deco-height (deco-part deco 1))))
                      (modify-deco deco
                                   :width (min (- x xoff) w2)
                                   :height (min (- y yoff) h2))))
                  event
                  :no-freeze #t)))
             :steal #t)
   (on-event (button 1 alone) ;; outer move
             (lambda (deco event)
               (let ((xoff (- (event-x event) (deco-x deco)))
                     (yoff (- (event-y event) (deco-y deco))))
                 (with-user-feedback 
                  (lambda (ev)
                    (let ((x (event-x ev))
                          (y (event-y ev)))
                      (move-window deco (- x xoff) (- y yoff))))
                  event
                  :no-freeze #t)))
             :steal #t)
   window-behavior
   std-window-behavior))

(define (clip-window win)
  (let ((w (deco-width win))
        (h (deco-height win))
        (nw clip-window-width)
        (nh clip-window-height))
    (modify-deco win :anchor `((0.5 0 0.5) (0.5 0 0.5)))
    (make-deco win 
               :width w :height h
               :behavior (clip-window-behavior))))

