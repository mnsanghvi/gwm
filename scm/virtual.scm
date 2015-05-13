;; virtual.scm --- General virtual screens package
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file implements virtual screen functionality, i.e. allowing
;; windows to be spread out on a much larger area than the visible 
;; portion of the screen, and then to pan around in that area.
;;
;; This is just the basic functionality for dealing with a virtual
;; screen, and not any graphical interface it, which is instead 
;; implemented by other packages. However, the code is gnome compliant,
;; so it should be possible to use a gnome panner to interact with it.
;; Also, moving around in the virtual screen can be done with the arrow
;; keys together with some suitable modifiers.
;;

;;    User customizable variables. Adjust these in your own profile.
(defvar virtual-modifiers (logior control-mask alt-mask) "modifiers for arrow keys" 'integer)
(defvar virtual-horizontal-step (/ (screen-width) 2) "amount to move by keys" 'integer)
(defvar virtual-vertical-step (/ (screen-height) 2) "amount to move by keys" 'integer)
(defvar virtual-nailed-list '(Gwm) "initially nailed windows" 'list)
(defvar virtual-update-hook (make-hook 0) "Hook to run when configuration has changed" 'hook)
(defvar virtual-size #f "preferred size of virtual area, list of number of screens wide and high" 'list)

(define virt-block-update #f)
(define virtual #t)

(require 'flet)

(define (virt-pos-x)
  (car (get-property (root-window) 'virt-pos)))

(define (virt-pos-y)
  (cdr (get-property (root-window) 'virt-pos)))

(define (set-virt-pos! x y)
  (let ((pos (get-property (root-window) 'virt-pos)))
    (set-car! pos x)
    (set-cdr! pos y)))

(define (reg-virt-pos)
  (set-property! (root-window) 'virt-pos (cons 0 0)))

(define (virtual-nailed win)
  ;; Determine whether the current window is nailed or not.
  (if (not (window-valid? win)) ;; still under construction
      (or (matches-list win virtual-nailed-list)
                (and (string=? (window-client-class win) "Gwm")
                     (equal? (window-name win) "virtual")))
      (let* ((win (window-deco win))
             (nail (get-property win 'nailed)))
        (if nail
            (not (eq? nail 'no))
            (if (or (matches-list win virtual-nailed-list)
                    (and (string=? (window-client-class win) "Gwm")
                         (equal? (window-name win) "virtual")))
                (begin
                  (set-property! win 'nailed #t)
                  #t)
                (begin
                  (set-property! win 'nailed 'no)
                  #f))))))

(define (virtual-nail win)
  (set-property! (window-deco win) 'nailed #t)
  (send-user-event 'nail (window-deco win))
  (virtual-update))

(define (virtual-unnail win)
  (set-property! (window-deco win) 'nailed 'no)
  (send-user-event 'unnail (window-deco win))
  (virtual-update))

(define (virtual-toggle-nail win)
  (if (virtual-nailed win)
      (virtual-unnail win)
      (virtual-nail win)))

(define (virt-movable-windows)
  ;; Returns a list of movable windows
  (let ((movable '()))
    (for-each (lambda (w)
                (if (not (virtual-nailed w))
                    (set! movable (cons w movable))))
              (list-of-windows 'window))
    (reverse! movable)))

(define (virtual-move-windows deltax deltay)
  ;; Moves windows by deltax and deltay adjusting virt-pos 
  ;; appropriately
  (flet ((virt-block-update #t))
    (for-each (lambda (w)
                (move-window w (+ (window-x w) deltax) (+ (window-y w) deltay)))
              (virt-movable-windows)))
  (set-virt-pos! (- (virt-pos-x) deltax)
                 (- (virt-pos-y) deltay))
  (gnome-virtual-update)
  (virtual-update))

(define (virtual-move-left)
  (virtual-move-windows virtual-horizontal-step 0))

(define (virtual-move-right)
  (virtual-move-windows (- virtual-horizontal-step) 0))

(define (virtual-move-up)
  (virtual-move-windows 0 virtual-vertical-step))

(define (virtual-move-down)
  (virtual-move-windows 0 (- virtual-vertical-step)))

(define (virtual-move-home)
  (virtual-move-windows (virt-pos-x) (virt-pos-y)))

(define (virtual-move-to x y)
  (virtual-move-windows (- (virt-pos-x) x) (- (virt-pos-y) y)))

(define (virtual-make-window-visible win)
  ;; Move the virtual screen to make the current window visible.
  (if (not (virtual-nailed win))
      (let ((wtop (window-y win))
            (wbot (+ (window-height win) (window-y win)))
            (wleft (window-x win))
            (wright (+ (window-width win) (window-x win)))
            (stop 0)
            (sbot (screen-height))
            (sleft 0)
            (sright (screen-width))
            (dx 0)
            (dy 0))
        (if (or (>= wright sright)  ; Check that no part visible
                (<= wleft sleft)
                (>= wbot sbot)
                (<= wtop stop))
            (begin
              (set! dx (- (* (inexact->integer (- (/ (+ wleft wright)
                                                   (* 2 virtual-horizontal-step)) 0.5))
                             virtual-horizontal-step)))
              (set! dy (- (* (inexact->integer (- (/ (+ wtop wbot)
                                                   (* 2 virtual-vertical-step)) 0.5))
                             virtual-vertical-step)))
              (virtual-move-windows dx dy))))))

(define (virtual-placement win)
  (if (and (not (window-was-on-screen win))
           (deco-window? win))
      (if (not (virtual-nailed win))
          (move-window win
                       (- (window-x win) (virt-pos-x))
                       (- (window-y win) (virt-pos-y))))))

(define (virtual-x x)
  (+ x (virt-pos-x)))

(define (virtual-y y)
  (+ y (virt-pos-y)))

(define (virtual-coord-string x y)
  (string-append "+" (number->string (virtual-x x))
                 "+" (number->string (virtual-y y))))

(define (virtual-update)
  (if (not virt-block-update)
      (run-hook virtual-update-hook)))

(define (gnome-virtual-update)
  (if (check-gnome-compliance)
      (let ((x (quotient (virt-pos-x) (screen-width)))
            (y (quotient (virt-pos-y) (screen-height))))
        (set-x-property! (root-window) '_WIN_AREA (list x y)))))
      
(define (gnome-virtual-move-to pos)
  (if (check-gnome-compliance)
      (let ((x (quotient (virt-pos-x) (screen-width)))
            (y (quotient (virt-pos-y) (screen-height))))
        (if (not (and (= x (car pos)) (= y (cadr pos))))
            (virtual-move-to (* (screen-width) (car pos))
                             (* (screen-height) (cadr pos)))))))

(define (virtual-behavior)
  (make-behavior
   (on (key "Left" virtual-modifiers)
       (virtual-move-left)
       :steal #t)
   (on (key "Right" virtual-modifiers)
       (virtual-move-right)
       :steal #t)
   (on (key "Up" virtual-modifiers)
       (virtual-move-up)
       :steal #t)
   (on (key "Down" virtual-modifiers)
       (virtual-move-down)
       :steal #t)
   (on (client-message '_WIN_AREA)
       (gnome-virtual-move-to (event-data event)))))

(define (virtual-init-screen s)
  (set-screen! s)
  (reg-virt-pos)
  (modify-behavior (deco-behavior (root-window)) (virtual-behavior))
  (if (check-gnome-compliance)
      (begin
        (register-gnome-feature '_WIN_AREA)
        (set-x-property! (root-window) '_WIN_AREA_COUNT (or virtual-size (list 1 1)))
        (set-x-property! (root-window) '_WIN_AREA (list 0 0)))))

(if (gwm-is-starting)
    (add-to-hook! screen-opening virtual-init-screen)
    (map virtual-init-screen (list-of-screens)))

(add-to-hook-first! pop-to-window-hook virtual-make-window-visible)

(add-to-hook! screen-closing (lambda (s) (virtual-move-home)))

(advice raise-window 'virtual 'after
  (virtual-update))

(advice lower-window 'virtual 'after
  (virtual-update))

(advice move-window 'virtual 'after
  (if (window-mapped? (car args))
      (virtual-update)))

(advice resize-window 'virtual 'after
  (if (window-mapped? (car args))
      (virtual-update)))

(advice move-resize-window 'virtual 'after
  (if (window-mapped? (car args))
      (virtual-update)))

(add-to-hook! map-window-hook (lambda (w) (virtual-update)))

(add-to-hook! unmap-window-hook (lambda (w) (virtual-update)))

(custom-menu-package-name '("Virtual Screen"))

