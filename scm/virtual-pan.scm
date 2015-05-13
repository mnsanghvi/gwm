;; virtual-pan.scm --- Autopanning or pan on click for the virtual screen
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file defines "pan-lists" to put around the edges of the
;; screen. These can be used either for autopanning on the virtual
;; screen (ie. the real window moves when the mouse enters a
;; pan-list), or "pan on click" (ie. panning occurs when the user
;; clicks on the list). Which mode to use is controlled by the
;; variable 'pan-on-click'.
;;
;; When "panning on click", the length to pan is the same as the
;; horizontal and vertical step lengths in "virtual.scm". When
;; "autopanning", the step length is controlled by 'pan-x-step' and
;; 'pan-y-step'.
;;
;; The pan lists are installed with '(install-pan-lists)' and removed
;; with '(remove-pan-lists)'.
;; 

;;    User customizable variables. Adjust these in your own profile.
(defvar show-pan-lists #t "Enable pan lists" 'boolean)
(defvar pan-on-enter #f "Pan on entering edge instead of clicking" 'boolean)
(defvar pan-x-step (quotient (screen-width) 4) "How much to pan when autopanning" 'integer)
(defvar pan-y-step (quotient (screen-height) 4) "How much to pan when autopanning" 'integer)
(defvar pan-delay #f "Time in milliseconds before autopanning" 'integer)
(defvar pan-warp-step 4 "Movement of cursor from edge on autopan" 'integer)
(defvar pan-warp-wrapped #f "Move cursor to opposite edge on autopan" 'boolean)
(defvar pan-corner-width 30 "Diagonal pan when this close to corner" 'integer)

(require 'virtual)

(define pan-behavior
  (make-behavior
    (on (enter)
        (if pan-on-enter
            (let* ((xpos (event-x event))
                   (ypos (event-y event))
                   (xdir (cond ((< xpos pan-corner-width) 1)
                               ((> xpos (- (screen-width) pan-corner-width 1)) -1)
                               (#t 0)))
                   (ydir (cond ((< ypos pan-corner-width) 1)
                               ((> ypos (- (screen-height) pan-corner-width 1)) -1)
                               (#t 0))))
              (if (or (not pan-delay)
                      (begin
                        (usleep (* 1000 pan-delay))
                        (let ((pos (pointer-position)))
                          (eq? deco (window-at-position (car pos) (cadr pos))))))
                  (begin
                    (virtual-move-windows (* pan-x-step xdir)
                                          (* pan-y-step ydir))
                    (if pan-warp-wrapped
                        (warp-pointer (* (- pan-x-step pan-warp-step) xdir)
                                      (* (- pan-y-step pan-warp-step) ydir))
                        (warp-pointer (* pan-warp-step xdir)
                                      (* pan-warp-step ydir))))))))
    (on (button any any)
        (if (not pan-on-enter)
            (let* ((xpos (event-x event))
                   (ypos (event-y event))
                   (xdir (cond ((< xpos pan-corner-width) 1)
                               ((> xpos (- (screen-width) pan-corner-width 1)) -1)
                               (#t 0)))
                   (ydir (cond ((< ypos pan-corner-width) 1)
                               ((> ypos (- (screen-height) pan-corner-width 1)) -1)
                               (#t 0))))
              (virtual-move-windows (* virtual-horizontal-step xdir)
                                    (* virtual-vertical-step ydir)))))
    (on (opening)
        (raise-window deco))
    ))

(define (make-pan-list x y xs ys)
  (place-menu (make-deco :width xs
                         :height ys
                         :background (make-color "black")
                         :behavior pan-behavior)
              (root-window)
              x y 
              :name "panlist"
              :decoration (lambda (win)
                            (stack-set-priority win 10)
                            win)))

(define (remove-pan-lists)
  (for-each (lambda (w)
              (if (string=? (window-name w) "panlist")
                  (delete-window w)))
            (list-of-windows)))

(define (install-pan-lists)
  (remove-pan-lists)
  (make-pan-list 0 0 1 (screen-height))
  (make-pan-list 1 0 (- (screen-width) 2) 1)
  (make-pan-list (- (screen-width) 1) 0 1 (screen-height))
  (make-pan-list 1 (- (screen-height) 1) (- (screen-width) 2) 1))

(define (toggle-pan-lists)
  (set! show-pan-lists (not show-pan-lists))
  (if show-pan-lists
      (install-pan-lists)
      (remove-pan-lists)))

;; Install virtual-pan

(add-to-hook! screen-opening (lambda (s) (if show-pan-lists (install-pan-lists))))

(add-to-hook! screen-resize-hook (lambda () (if show-pan-lists (install-pan-lists))))

(custom-menu-install-hook '("Virtual Screen" "Pan Lists") (lambda ()
                                                            (if show-pan-lists
                                                                (install-pan-lists)
                                                                (remove-pan-lists))))


(define virtual-pan #t)
