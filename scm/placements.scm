;; placements.scm --- Window placement functionality
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; The functions in this file controls where and how different windows
;; are positioned on screen when they are first mapped. 
;;
;; The function (set-placement MATCH FUNCTION) makes all windows matching
;; MATCH use the placement function FUNCTION. For icons there is a
;; corresponding function 'set-icon-placement'. 
;; The function (add-conditional-placement MATCH FUNCTION) makes the
;; conditional placement function FUNCTION to be run on all windows
;; matching MATCH. A conditional placement function returns true if it
;; has successfully placed the window. If it returns false, the next
;; (conditional or ordinary) function that matches is run instead. For
;; icons there is a corresponding function 'add-conditional-icon-placement'.
;;
;; A placement function can call (set-unplacement WINDOW FUNCTION) on its
;; window if it wants to run some special action when the placed window 
;; dies (e.g. move other windows around for alignment).
;;
;; There are a number of typical placement functions provided:
;; 'onscreen-placement' tries to make sure that the window is within the
;; visible part of the screen. 
;; 'random-placement' is not really random, but places the window with a
;; diagonal offset from the last window, wrapping around trying to make
;; the whole window visible.
;; 'user-placement' gives the user a rubber outline of the window that
;; can be dragged into position.
;; 'std-placement' is the default placement method. It tries to place the
;; window according to user or program provided hints, and otherwise uses
;; either 'random-placement' or 'user-placement' depending on the value of
;; the variable 'place-randomly'.
;;
;; The function (make-tiled-placement X1 Y1 X2 Y2 DIR [KEY VALUE]...)
;; returns a placement function that tiles the windows in some area of the
;; screen, compacting them when some window is removed. This is useful for
;; e.g. icons. (X1, Y1) is the start position for the tiling, and (X2, Y2)
;; the end. When the rectangle is filled, tiling starts from (X1, Y1)
;; again. DIR is either 'horizontal or 'vertical. Keys can be :separator,
;; which sets the distance between tiled windows, and :sort, which is a
;; function to use to compare two windows, to maintain some sorting order.
;;

(defvar default-placement (lambda (w) (std-placement w)) "Default placement method" 'procedure)
(defvar default-icon-placement (lambda (w) #f) "Default icon placement method" 'procedure)
(defvar placement-list '() "List of conditions and placement methods" 'list)
(defvar icon-placement-list '() "List of conditions and icon placement methods" 'list)
(defvar place-randomly #t "Whether windows should be placed pseudo randomly or by user" 'boolean)

(define (place-window win)
  ((cond ((deco-icon? win)
          (or (matches-cond win icon-placement-list)
              default-icon-placement))
         ((deco-window? win)
          (or (matches-cond win placement-list)
              default-placement)))
   win))

(define (set-placement cnd proc)
  (let ((pair (assoc cnd placement-list)))
    (if proc
        (if pair
            (set-cdr! pair proc)
            (set! placement-list (cons (cons cnd proc)
                                       placement-list)))
        (if pair (assoc-remove cnd placement-list)))))

(define (add-conditional-placement cnd proc)
  (set! placement-list (cons (cons (cond ((eq? cnd #t)
                                          proc)
                                         ((procedure? cnd)
                                          (lambda (w)
                                            (and (cnd w) (proc w))))
                                         (#t
                                          (lambda (w)
                                            (and (matches-token w cnd)
                                                 (proc w)))))
                                   (lambda (w) #f))
                             placement-list)))

(define (set-icon-placement cnd proc)
  (let ((pair (assoc cnd icon-placement-list)))
    (if proc
        (if pair
            (set-cdr! pair proc)
            (set! icon-placement-list (cons (cons cnd proc)
                                            icon-placement-list)))
        (if pair (assoc-remove cnd icon-placement-list)))))

(define (add-conditional-icon-placement cnd proc)
  (set! icon-placement-list (cons (cons (cond ((eq? cnd #t)
                                               proc)
                                              ((procedure? cnd)
                                               (lambda (w)
                                                 (and (cnd w) (proc w))))
                                              (#t
                                               (lambda (w)
                                                 (and (matches-token w cnd)
                                                      (proc w)))))
                                        (lambda (w) #f))
                                  icon-placement-list)))

(define (set-unplacement win proc)
  (set-property! win 'unplacement proc))

(define (unplace-window w)
  (let ((proc (get-property w 'unplacement)))
    (if proc (proc w))))

(add-to-hook! window-closing unplace-window)

(add-to-hook! icon-closing unplace-window)

(add-to-hook! window-opening place-window)

(add-to-hook! icon-opening place-window)

;;-----------------------------------------------------------------

(define (onscreen-placement win)
  (let ((x (window-x win))
        (y (window-y win)))
    (if (> (+ x (window-width win)) (screen-width))
        (set! x (- (screen-width) (window-width win))))
    (if (< x 0)
        (set! x 0))
    (if (> (+ y (window-height win)) (screen-height))
        (set! y (- (screen-height) (window-height win))))
    (if (< y 0)
        (set! y 0))
    (if (not (and (= x (window-x win)) (= y (window-y win))))
        (move-window win x y))))

(define random-placement
  (let ((place-x-offset 23)
        (place-y-offset 19)
        (place-x-wrap (+ (screen-width) 7))
        (place-y-wrap (+ (screen-height) 7))
        (place-last-x 0)  
        (place-last-y 100))
    (lambda (win)
      (if (and (not (window-was-on-screen win))
               (deco-window? win))
          (let* ((left (+ place-last-x place-x-offset))
                 (right (+ left (window-width win)))
                 (top (+ place-last-y place-y-offset))
                 (bottom (+ top (window-height win))))
            (if (> right (screen-width))
                (set! place-last-x (modulo (- left place-x-wrap) place-x-offset))
                (set! place-last-x left))
            (if (> bottom (screen-height))
                (set! place-last-y (modulo (- top place-y-wrap) place-y-offset))
                (set! place-last-y top))
            (move-window win place-last-x place-last-y))))))
                   
(define (user-placement win)
  (if (and (not (window-was-on-screen win))
           (deco-window? win))
      (let ((pos (pointer-position))
            (cur (make-cursor 130)))
        (move-window win (car pos) (cadr pos))
        (user-move-window win #f :cursor cur :opaque #f)
        (set! pos (pointer-position))
        (if (> (caddr pos) 0)
            (begin
              (warp-pointer (- (window-width win) 2)
                            (- (window-height win) 2)
                            win)
              (user-resize-window win #f :cursor cur :opaque #f))))))

(define (std-placement win)
  (cond ((or (window-was-on-screen win)
             (string=? (window-client-class win) "Gwm")
             (not (deco-window? win)))
         #f)
        ((not (or (window-program-set-position? win)
                  (window-user-set-position? win)))
;             (and (= (window-x win) 0)
;                  (= (window-y win) 0)))
         (if place-randomly
             (random-placement win)
             (user-placement win)))
;        ((window-user-set-position? win)
;         (virtual-placement win))
        ((window-program-set-position? win)
         (let* ((left (window-x win))
                (right (+ left (window-width win)))
                (top (window-y win))
                (bottom (+ top (window-height win))))
           (if (not (and (< left (screen-width)) 
                         (> right -1)
                         (< top (screen-height))
                         (> bottom -1)))
               (if place-randomly
                   (random-placement win)
                   (user-placement win)))))))

(define (make-tiled-placement-internal x1 y1 x2 y2 dir . args)
  (let* ((xstart (if (< x1 0) (+ (screen-width) x1) x1))
         (ystart (if (< y1 0) (+ (screen-height) y1) y1))
         (xend (if (< x2 0) (+ (screen-width) x2) x2))
         (yend (if (< y2 0) (+ (screen-height) y2) y2))
         (dx (if (> xstart xend) -1 (if (< xstart xend) 1 0)))
         (dy (if (> ystart yend) -1 (if (< ystart yend) 1 0)))
         (dirh (eq? dir 'horizontal))
         (sep (get-keyword :separator args 5))
         (sortf (get-keyword :sort args #f))
         (currx xstart)
         (curry ystart)
         (rowbrd 0)
         (winlist '())
         (update-function #f))
    (set! update-function 
          (lambda (w)
            (if (deco-valid? w)
                (let ((winx #f)
                      (winy #f)
                      (wdt (window-width w))
                      (hgt (window-height w))
                      (flush #f))
                  (if dirh
                      (begin
                        (cond ((= dx 1)
                               (if (> xend (+ currx wdt -1))
                                   (begin
                                     (set! winx currx)
                                     (set! currx (+ currx wdt sep)))
                                   (begin
                                     (set! winx xstart)
                                     (set! currx (+ xstart wdt sep))
                                     (set! flush #t))))
                              ((= dx -1)
                               (if (< xend (- currx wdt -1))
                                   (begin
                                     (set! winx (- currx wdt -1))
                                     (set! currx (- currx wdt sep)))
                                   (begin
                                     (set! winx (- xstart wdt -1))
                                     (set! currx (- xstart wdt sep))
                                     (set! flush #t))))
                              ((= dx 0)
                               (set! winx (- currx (quotient wdt 2)))))
                        (cond ((= dy 1)
                               (if flush
                                   (begin
                                     (set! curry (+ curry rowbrd sep))
                                     (set! rowbrd 0))
                                   (set! rowbrd (max rowbrd hgt)))
                               (if (> curry yend)
                                   (set! winy ystart)
                                   (set! winy curry)))
                              ((= dy -1)
                               (if flush
                                   (begin
                                     (set! curry (- curry rowbrd sep))
                                     (set! rowbrd 0))
                                   (set! rowbrd (max rowbrd hgt)))
                               (if (< curry yend)
                                   (set! winy (- ystart hgt -1))
                                   (set! winy (- curry hgt -1))))
                              ((= dy 0)
                               (set! winy (- curry (quotient hgt 2))))))
                      (begin
                        (cond ((= dy 1)
                               (if (> yend (+ curry hgt -1))
                                   (begin
                                     (set! winy curry)
                                     (set! curry (+ curry hgt sep)))
                                   (begin
                                     (set! winy ystart)
                                     (set! curry (+ ystart hgt sep))
                                     (set! flush #t))))
                              ((= dy -1)
                               (if (< yend (- curry hgt -1))
                                   (begin
                                     (set! winy (- curry hgt -1))
                                     (set! curry (- curry hgt sep)))
                                   (begin
                                     (set! winy (- ystart hgt -1))
                                     (set! curry (- ystart hgt sep))
                                     (set! flush #t))))
                              ((= dy 0)
                               (set! winy (- curry (quotient hgt 2)))))
                        (cond ((= dx 1)
                               (if flush
                                   (begin
                                     (set! currx (+ currx rowbrd sep))
                                     (set! rowbrd 0))
                                   (set! rowbrd (max rowbrd wdt)))
                               (if (> currx xend)
                                   (set! winx xstart)
                                   (set! winx currx)))
                              ((= dx -1)
                               (if flush
                                   (begin
                                     (set! currx (- currx rowbrd sep))
                                     (set! rowbrd 0))
                                   (set! rowbrd (max rowbrd wdt)))
                               (if (< currx xend)
                                   (set! winx (- xstart wdt -1))
                                   (set! winx (- currx wdt -1))))
                              ((= dx 0)
                               (set! winx (- currx (quotient wdt 2)))))))
                  (if (or (not (= (window-x w) winx))
                          (not (= (window-y w) winy)))
                      (move-window w winx winy))))))
    (lambda (wind)
      (if sortf
          (set! winlist (sort (cons wind winlist) sortf))
          (set! winlist (append winlist (list wind))))
      (set-unplacement wind (lambda (w)
                              (if (not (gwm-is-ending))
                                  (begin
                                   (set! winlist (delete w winlist))
                                   (set! currx xstart)
                                   (set! curry ystart)
                                   (set! rowbrd 0)
                                   (map update-function winlist)))))
      (set! currx xstart)
      (set! curry ystart)
      (set! rowbrd 0)
      (map update-function winlist))))

(define (make-tiled-placement . args)
  (if (> (screen-count) 1)
      (let ((scr (screen))
            (sym (gensym)))
        (map (lambda (s)
               (set-screen! s)
               (set-property! s sym 
                              (apply make-tiled-placement-internal args)))
             (list-of-screens))
        (set-screen! scr)
        (lambda (wind)
          ((get-property (root-window) sym) wind)))
      (apply make-tiled-placement-internal args)))
