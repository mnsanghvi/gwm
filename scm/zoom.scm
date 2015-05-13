;; zoom-window.scm --- Window zooming functions
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; The main function in this file is (zoom-window WINDOW) which toggles
;; the zoomed (i.e. maximized) state of the window. There is also a 
;; variable 'zoom-window-method-list' that maps diffrent window types to
;; different zoom methods. For example, Emacs is reasonable to zoom to a
;; full screen, but an XTerm may be better to just zoom vertically, and
;; a picture should perhaps be zoomed proportionally. The different zoom
;; methods can also be called directly, without going through the
;; 'zoom-window' function.
;; 

(defvar zoom-offset 3 "Minimum distance to screen edge." 'integer)
(defvar zoom-window-method-list '() "List of (wind-type zoom-func) specs." 'list)
       ; This is to make zoom-window use different zoom methods for different
       ; types of windows.

(if (not (and (defined? 'virtual-x) (defined? 'virtual-y)))
    (begin
      (define (virtual-x x) x)
      (define (virtual-y y) y)))

;; You can use this to define several further zooming functions, as below.
(define (zoom-window-aux win name method)
  (pop-to-window win)
  (let ((zoomed (get-property win 'zoom))
        (old-pos (get-property win 'zpos)))         ;; virtual screen is tricky
    (cond ((eq? zoomed name)                 ;; unzoom window
           (set-property! win 'zoom #f)
           (zoom-move-resize win
                             (virtual-x (car old-pos)) (virtual-y (cadr old-pos))
                             (caddr old-pos) (cadddr old-pos)))
          ((not zoomed)                      ;; zoom window
           (set-property! win 'zoom name)
           (set-property! win 'zpos (dimensions win))
           (method zoom-offset zoom-offset
                   (- (screen-width) (* zoom-offset 2))
                   (- (screen-height) (* zoom-offset 2))))
          (#t                            ;; rezoom window
           (set-property! win 'zoom name)
           (method zoom-offset zoom-offset
                   (- (screen-width) (* zoom-offset 2))
                   (- (screen-height) (* zoom-offset 2)))))))

(define (zoom-move-resize win x y w h)
  (let ((bw (* 2 (deco-borderwidth (top-deco win)))))
    (move-resize-window win x y (- w bw) (- h bw))))

(define (zoom-window-full win)
  (zoom-window-aux win 'zoom-window-full
                   (lambda (x y w h)
                     (zoom-move-resize win x y w h))))

(define (zoom-window-vert win)
  (zoom-window-aux win 'zoom-window-vert
                   (lambda (x y w h)
                     (zoom-move-resize win (window-x win) y (window-width win) h))))

(define (zoom-window-horiz win)
  (zoom-window-aux win 'zoom-window-horiz
                   (lambda (x y w h)
                     (zoom-move-resize win x (window-y win) w (window-height win)))))

(define (zoom-window-left win)
  (zoom-window-aux win 'zoom-window-left
                   (lambda (x y w h)
                     (zoom-move-resize win x y
                                       (quotient (- w x 1) 2) h))))

(define (zoom-window-right win)
  (zoom-window-aux win 'zoom-window-right
                   (lambda (x y w h)
                     (zoom-move-resize win (quotient (+ w (* 3 x) 1) 2) y
                                       (quotient (- w x 1) 2)  h))))

(define (zoom-window-top win)
  (zoom-window-aux win 'zoom-window-top
                   (lambda (x y w h)
                     (zoom-move-resize win x y
                                       w (quotient (- h y 1) 2)))))

(define (zoom-window-bottom win)
  (zoom-window-aux win 'zoom-window-bottom
                   (lambda (x y w h)
                     (zoom-move-resize win x (quotient (+ h (* 3 y) 1) 2)
                                       w (quotient (- h y 1) 2)))))

(define (zoom-window-prop win)
  (zoom-window-aux win 'zoom-window-prop
                   (lambda (x y w h)
                     (let* ((clw (window-client-width win))
                            (clh (window-client-height win))
                            (wbs (- (window-width win) clw))
                            (hbs (- (window-height win) clh))
                            (prop (min (/ (- w wbs) clw)
                                       (/ (- h hbs) clh)))
                            (neww (+ (inexact->integer (* clw prop)) wbs))
                            (newh (+ (inexact->integer (* clh prop)) hbs))
                            (newx (quotient (- (screen-width) neww) 2))
                            (newy (quotient (- (screen-height) newh) 2)))
                       (zoom-move-resize win newx newy neww newh)))))

(define (zoom-window win)
  (let ((method (or (matches-cond win zoom-window-method-list)
                    zoom-window-full)))
    (method win)))


