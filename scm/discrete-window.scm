;; discrete-window.scm --- Very lightweight window and icon decorations
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file implements a window, icon, and menu style where the entire
;; decoration consists of the window name on a transparent background. 
;; 

(defvar discrete-window-font "9x15bold" "Font name of discrete window title" 'string)
(defvar discrete-window-color "blue" "Color of discrete window title" 'string)
(defvar discrete-window-active-color "forestgreen" "Color of discrete window title when active" 'string)
(defvar discrete-menu-font #f "Font name of discrete menu text" 'string)
(defvar discrete-menu-color "darkred" "Color of discrete menu text" 'string)
(defvar discrete-menu-active-color "red" "Color of discrete menu text when active" 'string)
(defvar discrete-window-simple-list '(Gwm XLoad XClock XBatt XBiff) "List of window types with no title." 'list)

(defaults-to discrete-window-title-behavior (make-behavior))

(define (discrete-window-label str font coln cola type)
  (let* ((strsize (string-dimensions str font))
         (isize (+ 2 (quotient (cadr strsize) 2)))
         (ioff (quotient (- (cadr strsize) isize) 2))
         (toffx (+ (cadr strsize) (caddr strsize)))
         (toffy (cadddr strsize))
         (trans (make-color 'transparent))
         (gray (make-color "gray90"))
         (labeln (make-pixmap (+ (cadr strsize) (car strsize)) (cadr strsize)
                              :background trans))
         (labela (make-pixmap (+ (cadr strsize) (car strsize)) (cadr strsize)
                              :background trans)))
    (draw-text labeln (- toffx 1) (- toffy 1) str :font font :color gray) 
    (draw-text labeln (- toffx 1) (+ toffy 1) str :font font :color gray) 
    (draw-text labeln (+ toffx 1) (- toffy 1) str :font font :color gray) 
    (draw-text labeln (+ toffx 1) (+ toffy 1) str :font font :color gray) 
    (draw-text labeln toffx toffy str :font font :color coln) 
    (draw-text labela (- toffx 1) (- toffy 1) str :font font :color gray) 
    (draw-text labela (- toffx 1) (+ toffy 1) str :font font :color gray) 
    (draw-text labela (+ toffx 1) (- toffy 1) str :font font :color gray) 
    (draw-text labela (+ toffx 1) (+ toffy 1) str :font font :color gray) 
    (draw-text labela toffx toffy str :font font :color cola) 
    (cond ((eq? type 0)
           (draw-ellipse labeln ioff ioff isize isize
                         :background coln :foreground gray :borderwidth 1)
           (draw-ellipse labela ioff ioff isize isize
                         :background cola :foreground gray :borderwidth 1))
          ((eq? type 1)
           (draw-rectangle labeln ioff ioff isize isize
                           :background coln :foreground gray :borderwidth 1)
           (draw-rectangle labela ioff ioff isize isize
                           :background cola :foreground gray :borderwidth 1))
          ((eq? type 2)
           (let ((l ioff) (h (+ ioff isize)) (m (+ ioff (quotient isize 2))))
             (draw-line labeln l h h h :color gray)
             (draw-line labeln l (- h 1) m (- l 1) :color gray)
             (draw-line labeln m (- l 1) h (- h 1) :color gray)
             (draw-polygon labeln l h h h m l :color coln)
             (draw-line labela l h h h :color gray)
             (draw-line labela l (- h 1) m (- l 1) :color gray)
             (draw-line labela m (- l 1) h (- h 1) :color gray)
             (draw-polygon labela l h h h m l :color cola)))
          ((eq? type 3)
           (let ((l ioff) (h (+ ioff isize)) (m (+ ioff (quotient isize 2))))
             (draw-line labeln (- l 1) l (- l 1) h :color gray)
             (draw-line labeln l l h m :color gray)
             (draw-line labeln l h h m :color gray)
             (draw-polygon labeln l l l h (- h 1) m :color coln)
             (draw-line labela (- l 1) l (- l 1) h :color gray)
             (draw-line labela l l h m :color gray)
             (draw-line labela l h h m :color gray)
             (draw-polygon labela l l l h (- h 1) m :color cola)))
          ((eq? type 4)
           (let ((l ioff) (h (+ ioff isize)) (m (+ ioff (quotient isize 2))))
             (draw-line labeln (- l 1) (- l 1) h (- l 1) :color gray)
             (draw-line labeln (- l 1) l m h :color gray)
             (draw-line labeln h l m h :color gray)
             (draw-polygon labeln l l h l m (- h 1) :color coln)
             (draw-line labela (- l 1) (- l 1) h (- l 1) :color gray)
             (draw-line labela (- l 1) l m h :color gray)
             (draw-line labela h l m h :color gray)
             (draw-polygon labela l l h l m h :color cola)))
          ((eq? type 5)
           (let ((l ioff) (h (+ ioff isize)) (m (+ ioff (quotient isize 2))))
             (draw-line labeln h l h h :color gray)
             (draw-line labeln (- h 1) l (- l 1) m :color gray)
             (draw-line labeln (- l 1) m (- h 1) h :color gray)
             (draw-polygon labeln h l h h l m :color coln)
             (draw-line labela h l h h :color gray)
             (draw-line labela (- h 1) l (- l 1) m :color gray)
             (draw-line labela (- l 1) m (- h 1) h :color gray)
             (draw-polygon labela h l h h l m :color cola))))
    (make-deco labeln
               :background trans
               :behavior (make-behavior (on (user-event 'focus-in)
                                            (set-deco-part! deco 1 labela))
                                        (on (user-event 'focus-out)
                                            (set-deco-part! deco 1 labeln))))))

(define (discrete-window win)
  (if (and (= (deco-borderwidth win) 0)
           (not (deco-shaped? win)))
      (set-deco-borderwidth! win 1))
  (if (matches-list win discrete-window-simple-list)
      (make-deco win
                 :behavior (make-behavior window-behavior std-window-behavior))
      (let* ((font (make-font discrete-window-font))
             (coln (make-color discrete-window-color))
             (cola (make-color discrete-window-active-color))
             (label (discrete-window-label (window-name win) font coln cola 0))
             (trans (make-color 'transparent))
             (beh (make-behavior window-behavior
                                 (on (user-event 'shade)
                                     (hide-deco (inner-deco deco)))
                                 (on (user-event 'unshade)
                                     (show-deco (inner-deco deco)))
                                 (on (user-event 'name-change)
                                     (set-deco-part! (deco-part deco 1) 1 
                                                     (discrete-window-label (window-name deco) font coln cola 0)))
                                 std-window-behavior)))
        (make-deco (make-deco label '() 
                              :behavior discrete-window-title-behavior
                              :background trans)
                   (make-deco win '() :background trans)
                   :property `((shaded . #f))
                   :background trans
                   :behavior beh))))

(define (discrete-icon win)
  (let* ((font (make-font discrete-window-font))
         (coln (make-color discrete-window-color))
         (cola (make-color discrete-window-active-color))
         (label (discrete-window-label (if (string=? (window-icon-name win) "icon")
                                           (window-name win)
                                           (window-icon-name win))
                                       font coln cola 2))
         (beh (make-behavior icon-behavior
                             (on (enter)
                                 (send-user-event 'focus-in deco))
                             (on (leave)
                                 (send-user-event 'focus-out deco))
                             (on (user-event 'name-change)
                                 (set-deco-part! deco 1 
                                                 (discrete-window-label (if (string=? (window-icon-name win) "icon")
                                                                            (window-name win)
                                                                            (window-icon-name win))
                                                                        font coln cola 2)))
                             std-icon-behavior)))
    (make-deco label
               :background (make-color 'transparent)
               :behavior beh)))

(define (discrete-menu-item-make str action)
  (let* ((font (make-font (or discrete-menu-font discrete-window-font)))
         (coln (make-color discrete-menu-color))
         (cola (make-color discrete-menu-active-color))
         (menu (if (deco? action) action #f))
         (label (discrete-window-label str font coln cola (if menu 3 1)))
         (trans (make-color 'transparent))
         (beh (make-behavior (on (user-event 'active)
                                 (send-user-event 'focus-in deco))
                             (on (user-event 'inactive)
                                 (send-user-event 'focus-out deco))
                             std-menu-item-behavior)))
    (make-deco label '()
               :background trans
               :behavior beh
               :property `((menu . ,menu)
                           (action . ,(if menu #f action))))))
     
(define (discrete-menu-label-make str)
  (let* ((font (make-font (or discrete-menu-font discrete-window-font)))
         (coln (make-color discrete-menu-color))
         (label (discrete-window-label str font coln coln -1)))
    (make-deco label '()
               :background (make-color 'transparent)
               :behavior std-menu-label-behavior)))

(define (discrete-menu-separator-make)
  (make-deco '()
             :height 1
             :background (make-color discrete-menu-color)
             :behavior std-menu-label-behavior))

(define (discrete-menu-make . args)
  (let ((ctx (list :behavior std-menu-behavior
                   :background (make-color 'transparent))))
    (apply make-deco (append args ctx))))

(define discrete-menu-context
  (list :menu-func discrete-menu-make
        :item-func discrete-menu-item-make
        :label-func discrete-menu-label-make
        :separator-func discrete-menu-separator-make
        :foreground (lambda () discrete-menu-color)
        :background (lambda () 'hole)
        :font (lambda () (or discrete-menu-font discrete-window-font))))

