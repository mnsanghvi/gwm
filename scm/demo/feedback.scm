;; feedback.scm --- Demos of with-timer-feedback and with-user-feedback
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 


;; Iconification by a shrinking outline
(define (shrink-to-icon win)
  (let* ((w (window-deco win))
         (ic (icon-deco win))
         (i 0)
         (x1 (window-x w))
         (y1 (window-y w))
         (x2 (window-x ic))
         (y2 (window-y ic))
         (w1 (window-width w))
         (h1 (window-height w))
         (w2 (window-width ic))
         (h2 (window-height ic)))
    (hide-window w 'shrink)
    (with-timer-feedback (lambda ()
                           (draw-rubber-rectangle (screen)
                                                  (inexact->exact (/ (+ (* i x2) (* (- 20 i) x1)) 20))
                                                  (inexact->exact (/ (+ (* i y2) (* (- 20 i) y1)) 20))
                                                  (inexact->exact (/ (+ (* i w2) (* (- 20 i) w1)) 20))
                                                  (inexact->exact (/ (+ (* i h2) (* (- 20 i) h1)) 20)))
                           (set! i (+ i 1)))
                         0.02 21 :no-freeze #t)
    (iconify-window w)
    (unhide-window w 'shrink)))

;; Deiconification by a growing outline
(define (grow-from-icon win)
  (let* ((w (window-deco win))
         (ic (icon-deco win))
         (i 0)
         (x1 (window-x w))
         (y1 (window-y w))
         (x2 (window-x ic))
         (y2 (window-y ic))
         (w1 (window-width w))
         (h1 (window-height w))
         (w2 (window-width ic))
         (h2 (window-height ic)))
    (hide-window w 'shrink)
    (deiconify-window w)
    (with-timer-feedback (lambda ()
                           (draw-rubber-rectangle (screen)
                                                  (inexact->exact (/ (+ (* i x1) (* (- 20 i) x2)) 20))
                                                  (inexact->exact (/ (+ (* i y1) (* (- 20 i) y2)) 20))
                                                  (inexact->exact (/ (+ (* i w1) (* (- 20 i) w2)) 20))
                                                  (inexact->exact (/ (+ (* i h1) (* (- 20 i) h2)) 20)))
                           (set! i (+ i 1)))
                         0.02 21 :no-freeze #t)
    (unhide-window w 'shrink)))

;; Iconification by a shrinking rotating outline
(define (rotate-to-icon win)
  (let* ((w (window-deco win))
         (i 0)
         (x (window-x w))
         (y (window-y w))
         (wdt (window-width w))
         (hgt (window-height w))
         (vx (/ wdt 2))
         (vy (/ hgt 2))
         (cx (+ x vx))
         (cy (+ y vy))
         (s 0.0)
         (c 1.0)
         (ang 0.0)
         (2pi (* (atan 1.0) 8)))
    (hide-window w 'shrink)
    (with-timer-feedback (lambda ()
                           (let* ((s (* (sin (/ (* i 2pi) 40)) (/ (- 40 i) 40)))
                                  (c (* (cos (/ (* i 2pi) 40)) (/ (- 40 i) 40)))
                                  (dx1 (- (* vx c) (* vy s)))
                                  (dy1 (+ (* vx s) (* vy c)))
                                  (dx2 (+ (* vx c) (* vy s)))
                                  (dy2 (- (* vx s) (* vy c))))
                             (draw-rubber-line (screen)
                                               (inexact->exact (+ cx dx1))
                                               (inexact->exact (+ cy dy1))
                                               (inexact->exact (+ cx dx2))
                                               (inexact->exact (+ cy dy2)))
                             (draw-rubber-line (screen)
                                               (inexact->exact (+ cx dx2))
                                               (inexact->exact (+ cy dy2))
                                               (inexact->exact (- cx dx1))
                                               (inexact->exact (- cy dy1)))
                             (draw-rubber-line (screen)
                                               (inexact->exact (- cx dx1))
                                               (inexact->exact (- cy dy1))
                                               (inexact->exact (- cx dx2))
                                               (inexact->exact (- cy dy2)))
                             (draw-rubber-line (screen)
                                               (inexact->exact (- cx dx2))
                                               (inexact->exact (- cy dy2))
                                               (inexact->exact (+ cx dx1))
                                               (inexact->exact (+ cy dy1)))
                             (set! i (+ i 1))))
                         0.02 41 :no-freeze #f)
    (iconify-window w)
    (unhide-window w 'shrink)))

;; Deiconification by a growing rotating outline
(define (rotate-from-icon win)
  (let* ((w (window-deco win))
         (i 0)
         (x (window-x w))
         (y (window-y w))
         (wdt (window-width w))
         (hgt (window-height w))
         (vx (/ wdt 2))
         (vy (/ hgt 2))
         (cx (+ x vx))
         (cy (+ y vy))
         (s 0.0)
         (c 1.0)
         (ang 0.0)
         (2pi (* (atan 1.0) 8)))
    (hide-window w 'shrink)
    (deiconify-window w)
    (with-timer-feedback (lambda ()
                           (let* ((s (* (sin (/ (* i 2pi) 40)) (/ i 40)))
                                  (c (* (cos (/ (* i 2pi) 40)) (/ i 40)))
                                  (dx1 (- (* vx c) (* vy s)))
                                  (dy1 (+ (* vx s) (* vy c)))
                                  (dx2 (+ (* vx c) (* vy s)))
                                  (dy2 (- (* vx s) (* vy c))))
                             (draw-rubber-line (screen)
                                               (inexact->exact (+ cx dx1))
                                               (inexact->exact (+ cy dy1))
                                               (inexact->exact (+ cx dx2))
                                               (inexact->exact (+ cy dy2)))
                             (draw-rubber-line (screen)
                                               (inexact->exact (+ cx dx2))
                                               (inexact->exact (+ cy dy2))
                                               (inexact->exact (- cx dx1))
                                               (inexact->exact (- cy dy1)))
                             (draw-rubber-line (screen)
                                               (inexact->exact (- cx dx1))
                                               (inexact->exact (- cy dy1))
                                               (inexact->exact (- cx dx2))
                                               (inexact->exact (- cy dy2)))
                             (draw-rubber-line (screen)
                                               (inexact->exact (- cx dx2))
                                               (inexact->exact (- cy dy2))
                                               (inexact->exact (+ cx dx1))
                                               (inexact->exact (+ cy dy1)))
                             (set! i (+ i 1))))
                         0.02 41 :no-freeze #f)
    (unhide-window w 'shrink)))

;; Iconification by shrinking the window like a shade
(define (shade-to-icon win)
  (let* ((w (window-deco win))
         (inner (inner-deco w))
         (i 1)
         (steps 21)
         (x1 (window-x w))
         (y1 (window-y w))
         (w1 (window-width w))
         (h1 (window-height w))
         (delta (quotient (window-height inner) (- steps 1))))
    (with-timer-feedback (lambda ()
                           (if (= i steps)
                               (begin
                                 (hide-window w 'shade)
                                 (move-resize-window w x1 y1 w1 h1))
                               (begin
                                 (move-resize-window w x1 y1 w1 (- h1 (* i delta)))
                                 (set! i (+ i 1)))))
                         0.02 steps :no-freeze #t)
    (iconify-window w)
    (unhide-window w 'shade)))

;; Deiconification by growing the window like a shade
(define (shade-from-icon win)
  (let* ((w (window-deco win))
         (inner (inner-deco w))
         (steps 21)
         (i steps)
         (x1 (window-x w))
         (y1 (window-y w))
         (w1 (window-width w))
         (h1 (window-height w))
         (delta (quotient (- (window-height inner) 1) (- steps 1))))
    (with-timer-feedback (lambda ()
                           (if (= i steps)
                               (begin
                                 (hide-window w 'shade)
                                 (deiconify-window w)
                                 (set! i (- i 1))
                                 (move-resize-window w x1 y1 w1 (- h1 (* i delta)))
                                 (unhide-window w 'shade))
                               (begin
                                 (set! i (- i 1))
                                 (move-resize-window w x1 y1 w1 (- h1 (* i delta))))))
                         0.02 steps :no-freeze #t)))

(define (make-spec-label txt font fg1 fg2 bg)
  (let* ((dim (string-dimensions txt font))
        (pix (make-pixmap (+ (car dim) 2) (+ (cadr dim) 2) :background bg))
        (x (+ (caddr dim) 1))
        (y (+ (cadddr dim) 1)))
    (draw-text pix (- x 1) (- y 1) txt :font font :color fg2)
    (draw-text pix x (- y 1) txt :font font :color fg2)
    (draw-text pix (+ x 1) (- y 1) txt :font font :color fg2)
    (draw-text pix (- x 1) y txt :font font :color fg2)
    (draw-text pix (+ x 1) y txt :font font :color fg2)
    (draw-text pix (- x 1) (+ y 1) txt :font font :color fg2)
    (draw-text pix x (+ y 1) txt :font font :color fg2)
    (draw-text pix (+ x 1) (+ y 1) txt :font font :color fg2)
    (draw-text pix x y txt :font font :color fg1)
    pix))

;; Flash a text on screen, to mimic windowmakers room notifications
(define (flash-text txt)
  (let ((font (make-font "*-fixed-*-40-*"))
        (trans (make-color 'transparent))
        (bp1 (make-pixmap 2 2 :background (make-color "black")))
        (bp2 (make-pixmap 2 2 :background (make-color "white")))
        (i 0)
        (p1 #f)
        (p2 #f)
        (p3 #f)
        (p4 #f)
        (lst #f)
        (delay #f)
        (deco #f))
    (set! p4 (make-spec-label txt font bp1 bp2 trans))
    (draw-point bp1 0 0 :color trans)
    (draw-point bp2 0 0 :color trans)
    (set! p3 (make-spec-label txt font bp1 bp2 trans))
    (draw-point bp1 1 1 :color trans)
    (draw-point bp2 1 1 :color trans)
    (set! p2 (make-spec-label txt font bp1 bp2 trans))
    (draw-point bp1 0 1 :color trans)
    (draw-point bp2 0 1 :color trans)
    (set! p1 (make-spec-label txt font bp1 bp2 trans))
    (set! lst `((,p1 . 0.05) (,p2 . 0.05) (,p3 . 0.05) (,p4 . 1.0) (,p3 . 0.05) (,p2 . 0.05) (,p1 . 0.05) (,p1 . #f)))
    (set! deco (make-deco p1 :background (make-color 'hole)))
    (place-menu deco (screen)
                (quotient (- (screen-width) (deco-width deco)) 2)
                (quotient (- (screen-height) (deco-height deco)) 2)
                :decoration (lambda (x) x) :shape 'hole)
    (with-timer-feedback (lambda ()
                           (let ((ele (list-ref lst i)))
                             (set-deco-part! deco 1 (car ele))
                             (set! i (+ 1 i))
                             (set! delay (cdr ele))))
                         (lambda ()
                           delay)
                         :no-freeze #t)
    (delete-window deco)
    ))


(define (calc-accel x1 v1 a dt m k1 k2)
  (let ((v2 (- v1 (* a dt)))
        (x2 (+ x1 (* v1 dt) (* a dt dt -0.5)))
        (t 0.0))
    (cond ((and (<= x1 0.0) (= v1 0.0))
           (list x1 v1))
          ((>= x2 0.0)
           (list x2 v2))
          (#t
           (if (< x1 0.0) (set! x1 0.0))
           (set! t (+ (/ v1 a) (sqrt (+ (/ (* v1 v1) (* a a)) (/ (* 2 x1) a)))))
           (set! v2 (- v1 (* a t)))
           (set! v1 (max 0.0 (- 0 (* v2 k1) (* k2 (sqrt m)))))
           (set! dt (- dt t))
           (set! v2 (- v1 (* a dt)))
           (set! x2 (+ (* v1 dt) (* a dt dt -0.5)))
           (if (< x2 0.0) (begin (set! x2 0.0) (set! v2 0.0)))
           (list x2 v2)))))

;; Drop and bounce a window like on the NeXT
(define (drop-window win)
  (let ((x (window-x win))
        (scrh (screen-height (root-deco win)))
        (winh (window-height win))
        (h (- (screen-height (root-deco win)) (window-y win) (window-height win)))
        (v 0.0)
        (m (* (window-height win) (window-width win)))
        (k1 0.7)
        (k2 0.5)
        (a 2000.0)
        (dt 0.05))
    (with-timer-feedback (lambda ()
                           (let ((res (calc-accel h v a dt m k1 k2)))
                             (set! h (car res))
                             (set! v (cadr res))
                             (move-window win x (inexact->exact (- scrh h winh)))))
                         (lambda ()
                           (if (or (> h 0.0) (not (= v 0.0)))
                               dt
                               #f))
                         :no-freeze #t)))

;; Draw a haircross over the screen following the mouse
(define (scales ev)
  (let ((small 32)
        (large 8)
        (ssize 10)
        (lsize 20)
        (hmax (screen-width))
        (vmax (screen-height))
        (i 0)
        (j 0)
        (res #f))
    (let ((sfunc (lambda ()
                   (set! i small)
                   (set! j 1)
                   (while (< i hmax)
                     (draw-rubber-line (screen) i 0 i (if (= 0 (remainder j large)) lsize ssize))
                     (set! i (+ i small))
                     (set! j (+ j 1)))
                   (set! i small)
                   (set! j 1)
                   (while (< i vmax)
                     (draw-rubber-line (screen) 0 i (if (= 0 (remainder j large)) lsize ssize) i)
                     (set! i (+ i small))
                     (set! j (+ j 1)))))
          (xfunc (lambda (x y)
                   (draw-rubber-line (screen) x 0 x (- vmax 1))
                   (draw-rubber-line (screen) 0 y (- hmax 1) y)
                   (draw-rubber-text (screen) (+ x 3) (+ y 16) (format #f "(~A, ~A)" x y)))))
      (set! res (with-user-feedback (lambda (e)
                                      (sfunc)
                                      (if e (xfunc (event-x e) (event-y e))))
                                    (lambda (e)
                                      (if (event-code e) e #f))))
      (list (event-x res) (event-y res)))))

