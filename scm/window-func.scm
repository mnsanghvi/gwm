;; window-func.scm --- Useful standard window functions
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; A set of useful window functions that should always be loaded, and 
;; standard window behaviors that all normal windows should include.
;; 


(defvar move-opaque #t "If non-nil, move windows opaquely" 'boolean)
(defvar resize-opaque #t "If non-nil, resize windows opaquely" 'boolean)
(defvar move-meter #f "If non-nil, use meter during move" 'boolean)
(defvar resize-meter #f "If non-nil, use meter during resize" 'boolean)
(defvar raise-on-move #f "Raise windows when moved" 'boolean)
(defvar raise-on-resize #f "Raise windows when resized" 'boolean)
(defvar raise-on-iconify #f "Raise icons/windows when iconified/deiconified" 'boolean)
(defvar screen-background #f "Screen background color" 'paint)
(defvar screen-cursor #f "Screen cursor" 'cursor)
(defvar autocolormap #t "If non-nil, change colormap with focus" 'boolean)
(defvar modifiers (logior control-mask alt-mask) "Default modifier mask for events grabbed by Gwm" 'number)


(define (default-move-feedback-function win args)
  (let ((opaque (get-keyword :opaque args move-opaque))
        (meter (get-keyword :meter args move-meter)))
    (if opaque
        (lambda (win x y)
          (move-window win x y)
          (if meter
              (draw-rubber-text (root-deco win) x (- y 3)
                                (format #f "(~A, ~A)" x y))))
        (let ((ix (- (deco-x (inner-deco win)) (window-x win)))
              (iy (- (deco-y (inner-deco win)) (window-y win)))
              (iw (+ (deco-width (inner-deco win)) (* 2 (deco-borderwidth (inner-deco win)))))
              (ih (+ (deco-height (inner-deco win)) (* 2 (deco-borderwidth (inner-deco win)))))
              (ow (window-width win))
              (oh (window-height win)))
          (lambda (win x y) 
            (draw-rubber-rectangle (root-deco win)
                                   (+ 1 x) (+ 1 y) (- ow 2) (- oh 2))
            (draw-rubber-rectangle (root-deco win)
                                   (+ ix x) (+ iy y) iw ih)
            (if meter
                (draw-rubber-text (root-deco win) x (- y 3)
                                  (format #f "(~A, ~A)" x y))))))))

(define (user-move-window win ev . rest)
  (let ((xoff (- (window-x win) (if ev (event-x ev) (car (pointer-position)))))
        (yoff (- (window-y win) (if ev (event-y ev) (cadr (pointer-position)))))
        (cursor (get-keyword :cursor rest #f))
        (drawfunc (move-feedback-function win rest))
        (res #f))
    (if raise-on-move
        (raise-window win))
    (set! res (with-user-feedback 
               (lambda (e)
                 (if e
                     (drawfunc win (+ xoff (event-x e)) (+ yoff (event-y e)))
                     (drawfunc win (window-x win) (window-y win))))
               (if ev ev (lambda (e) (if (event-code e) e #f)))
               :no-freeze #t
               :cursor cursor))
    (if (or (not ev) (= (event-code res) (event-code ev)))
        (move-window win (+ xoff (event-x res)) (+ yoff (event-y res))))))

(define (default-resize-feedback-function win args)
  (let ((opaque (get-keyword :opaque args resize-opaque))
        (meter (get-keyword :meter args resize-meter)))
    (if opaque
        (lambda (win x y w h)
          (move-resize-window win x y w h)
          (if meter
              (draw-rubber-text (root-deco win) x (- y 3)
                                (format #f "~A x ~A" w h))))
        (let ((ix (- (deco-x (inner-deco win)) (window-x win)))
              (iy (- (deco-y (inner-deco win)) (window-y win)))
              (idw (- (window-width win) (deco-width (inner-deco win)) (* 2 (deco-borderwidth (inner-deco win)))))
              (idh (- (window-height win) (deco-height (inner-deco win)) (* 2 (deco-borderwidth (inner-deco win))))))
          (lambda (win x y w h) 
            (draw-rubber-rectangle (root-deco win)
                                   (+ 1 x) (+ 1 y) (- w 2) (- h 2))
            (draw-rubber-rectangle (root-deco win)
                                   (+ ix x) (+ iy y) (- w idw) (- h idh))
            (if meter
                (draw-rubber-text (root-deco win) x (- y 3)
                                  (format #f "~A x ~A" w h))))))))

(define (user-resize-window win ev . rest)
  (let ((cursor (or (get-keyword :cursor rest #f) (make-cursor 52)))
        (corner (get-keyword :corner-size rest 1))
        (drawfunc (resize-feedback-function win rest))
        (xstart (if ev (event-x ev) (car (pointer-position))))
        (ystart (if ev (event-y ev) (cadr (pointer-position))))
        (xfix (window-x win))
        (xmove (+ (window-x win) (window-width win)))
        (xcaught #f)
        (yfix (window-y win))
        (ymove (+ (window-y win) (window-height win)))
        (ycaught #f)
        (tmp #f)
        (res #f))
    (cond ((>= xstart (- xmove corner 1))
           (set! xcaught (max 1 (- xmove xstart)))
           (set! xmove (+ xstart xcaught)))
          ((<= xstart (+ xfix corner))
           (set! xcaught (min 0 (- xfix xstart)))
           (set! xfix xmove)
           (set! xmove (+ xstart xcaught))))
    (cond ((>= ystart (- ymove corner 1))
           (set! ycaught (max 1 (- ymove ystart)))
           (set! ymove (+ ystart ycaught)))
          ((<= ystart (+ yfix corner))
           (set! ycaught (min 0 (- yfix ystart)))
           (set! yfix ymove)
           (set! ymove (+ ystart ycaught))))
    (set! corner (min 1 corner))
    (let ((cfunc (lambda (x y)
                   (cond ((and (not xcaught) (>= x (- xmove corner 1)))
                          (set! xcaught (max 1 (- xmove x)))
                          (set! xmove (+ x xcaught)))
                         ((and (not xcaught) (<= x (+ xfix corner)))
                          (set! xcaught (min 0 (- xfix x)))
                          (set! xfix xmove)
                          (set! xmove (+ x xcaught)))
                         (xcaught
                          (set! xmove (+ x xcaught))))
                   (cond ((and (not ycaught) (>= y (- ymove corner 1)))
                          (set! ycaught (max 1 (- ymove y)))
                          (set! ymove (+ y ycaught)))
                         ((and (not ycaught) (<= y (+ yfix corner)))
                          (set! ycaught (min 0 (- yfix y)))
                          (set! yfix ymove)
                          (set! ymove (+ y ycaught)))
                         (ycaught
                          (set! ymove (+ y ycaught))))
                   (set! tmp (conform-window-size
                              win 
                              (max 0 (if (or (not xcaught) (> xcaught 0))
                                         (- xmove xfix) (- xfix xmove)))
                              (max 0 (if (or (not ycaught) (> ycaught 0))
                                         (- ymove yfix) (- yfix ymove)))))
                   (if (or (not xcaught) (> xcaught 0))
                       (set! xmove (+ xfix (car tmp)))
                       (set! xmove (- xfix (car tmp))))
                   (if (or (not ycaught) (> ycaught 0))
                       (set! ymove (+ yfix (cadr tmp)))
                       (set! ymove (- yfix (cadr tmp)))))))
      (if raise-on-resize
          (raise-window win))
      (set! res (with-user-feedback 
                 (lambda (e)
                   (if e
                       (cfunc (event-x e) (event-y e))
                       (cfunc xstart ystart))
                   (drawfunc win
                             (min xfix xmove)
                             (min yfix ymove)
                             (abs (- xfix xmove))
                             (abs (- yfix ymove))))
                 (if ev ev (lambda (e) (if (event-code e) e #f)))
                 :no-freeze #t
                 :cursor cursor))
      (cfunc (event-x res) (event-y res))
      (if (or (not ev) (= (event-code res) (event-code ev)))
          (move-resize-window win
                              (min xfix xmove)
                              (min yfix ymove)
                              (abs (- xfix xmove))
                              (abs (- yfix ymove)))))))

(defvar move-feedback-function default-move-feedback-function "Function that returns a drawing function for user-move-window" 'procedure)
(defvar resize-feedback-function default-resize-feedback-function "Function that returns a drawing function for user-rfesize-window" 'procedure)

(advice (iconify-window w) 'std 'after
  (if raise-on-iconify
      (if (window-valid? w)
          (raise-window (icon-deco w)))))

(advice (deiconify-window w) 'std 'after
  (if raise-on-iconify
      (if (window-valid? w)
          (raise-window (window-deco w)))))

(define (toggle-iconify-window win)
  (if (window-iconified? win)
      (deiconify-window win)
      (iconify-window win)))

(define (windows-overlap w1 w2)
  (let* ((w1l (window-x w1))
         (w1t (window-y w1))
         (w1r (+ (window-width w1) w1l))
         (w1b (+ (window-height w1) w1t))
         (w2l (window-x w2))
         (w2t (window-y w2))
         (w2r (+ (window-width w2) w2l))
         (w2b (+ (window-height w2) w2t)))
    (and (< w2l w1r)
         (< w2t w1b)
         (> w2b w1t)
         (> w2r w1l))))

(define (window-obscured win)
  (let ((unobscured #t)
        (might-obscure #f)
        (pri (stack-get-priority win)))
    (for-each (lambda (w)
                (if (and might-obscure
                         (not (> (stack-get-priority w) pri))
                         (windows-overlap win w))
                    (set! unobscured #f))
                (if (eq? win w)
                    (set! might-obscure #t)))
              (list-of-windows 'stacking-order 'mapped))
    (not unobscured)))

(define deltabutton-delta 4)

(define (deltabutton ev)
  (car (with-user-feedback (lambda (e) #f)
                           (lambda (e)
                             (if (event-code e)
                                 (list #f)
                                 (if (or (> (abs (- (event-x ev) (event-x e)))
                                            deltabutton-delta)
                                         (> (abs (- (event-y ev) (event-y e)))
                                            deltabutton-delta))
                                     (list #t)
                                     #f)))
                           :no-freeze #t)))

(define (releasebutton ev)
  (let ((win (window-at-position (event-x ev) (event-y ev))))
    (car (with-user-feedback (lambda (e) #f)
                             (lambda (e)
                               (if (event-code e)
                                   (list (eq? (window-at-position (event-x e)
                                                                  (event-y e))
                                              win))
                                   #f))
                           :no-freeze #t))))
  
(define (raise-lower-window win ev)
  (if (window-obscured (top-deco win))
      (raise-window win)
      (lower-window win)))

(define (raise-lower-move-window win ev)
  (if (deltabutton ev)
      (user-move-window win ev)
      (raise-lower-window win ev)))

(define (iconify-move-window win ev)
  (if (deltabutton ev)
      (user-move-window win ev)
      (toggle-iconify-window win)))

(define (user-iconify-window win ev)
  (if (releasebutton ev)
      (toggle-iconify-window win)))

(define pop-to-window-hook (make-hook 1))
(add-hook! pop-to-window-hook 
           (lambda (win)
             (deiconify-window win)
             (raise-window win)))

(define (pop-to-window win)
  (if (and (window-valid? win)
           (not (eq? win (root-window))))
      (run-hook pop-to-window-hook (window-deco win))))

(define (hide-window win sym)
  (let* ((win (top-deco win))
         (hlist (or (get-property win 'hidden) '())))
    (if (not (memq sym hlist))
        (set-property! win 'hidden (cons sym hlist)))
    (if (not (deco-hidden? win))
        (hide-deco win))))

(define (unhide-window win sym)
 (let* ((win (top-deco win))
         (hlist (or (get-property win 'hidden) '())))
   (if (memq sym hlist)
       (begin
         (set! hlist (delq sym hlist))
         (set-property! win 'hidden hlist)
         (if (and (null? hlist)
                  (deco-hidden? win))
             (show-deco win))))))

(define (force-unhide-window win)
  (let ((win (top-deco win)))
    (set-property! win 'hidden '())
    (show-deco win)))

(define (window-hidden? win)
  (let* ((win (top-deco win))
         (hlist (get-property win 'hidden)))
    (if (null? hlist) #f hlist)))

(define (shade-window win)
  (let ((sh (assq 'shaded (deco-properties (top-deco win)))))
    (if (and sh (not (cdr sh)))
        (begin
          (send-user-event 'shade (top-deco win))
          (set-cdr! sh #t)))))

(define (unshade-window win)
  (let ((sh (assq 'shaded (deco-properties (top-deco win)))))
    (if (and sh (cdr sh))
        (begin
          (send-user-event 'unshade (top-deco win))
          (set-cdr! sh #f)
          (reconsider-focus)))))

(define (toggle-shade-window win)
  (let ((sh (assq 'shaded (deco-properties (top-deco win)))))
    (if sh
        (if (cdr sh)
            (begin
              (send-user-event 'unshade (top-deco win))
              (set-cdr! sh #f)
              (reconsider-focus))
            (begin
              (send-user-event 'shade (top-deco win))
              (set-cdr! sh #t))))))

(define (deiconify-all)
  (for-each (lambda (w)
              (deiconify-window w))
            (list-of-windows 'window)))

(define (redecorate-all)
  (for-each (lambda (w)
              (redecorate-window w))
            (list-of-windows 'window)))

;; Standard-window

(defaults-to focus-in-hook (make-hook 1))
(defaults-to focus-out-hook (make-hook 1))
(defaults-to enter-window-hook (make-hook 1))
(defaults-to leave-window-hook (make-hook 1))
(defaults-to map-window-hook (make-hook 1))
(defaults-to unmap-window-hook (make-hook 1))
(defaults-to name-change-hook (make-hook 1))
(defaults-to screen-resize-hook (make-hook 0))

(define std-window-behavior
  (make-behavior (on (focus-out)
                     (begin
                       (send-user-event 'focus-out deco)
                       (run-hook focus-out-hook deco)
                       (if autocolormap
                           (set-colormap-focus! (root-window)))))
                 (on (focus-in)
                     (begin
                       (send-user-event 'focus-in deco)
                       (run-hook focus-in-hook deco)
                       (if autocolormap
                           (set-colormap-focus! deco))))
                 (on (leave)
                     (run-hook leave-window-hook deco))
                 (on (enter)
                     (run-hook enter-window-hook deco))
                 (on (map-event)
                     (run-hook map-window-hook deco))
                 (on (unmap-event)
                     (run-hook unmap-window-hook deco))
                 (on (name-change)
                     (begin
                       (send-user-event 'name-change deco)
                       (if (icon-decorated? deco)
                           (send-user-event 'name-change (icon-deco deco)))
                       (run-hook name-change-hook deco)))
                 (on (property-change 'WM_ICON_NAME)
                     (begin
                       (if (icon-decorated? deco)
                           (send-user-event 'name-change (icon-deco deco)))
                       (run-hook name-change-hook deco)))
                 (on (icon-pixmap-change)
                     (if (icon-decorated? deco)
                         (send-user-event 'icon-pixmap-change (icon-deco deco))))
                 (on (opening)
                     (run-hook window-opening deco))
                 (on (closing)
                     (run-hook window-closing deco))
                 ))

(define std-icon-behavior
  (make-behavior (on (opening)
                     (run-hook icon-opening deco))
                 (on (closing)
                     (run-hook icon-closing deco))
                 ))

(define std-screen-behavior
  (make-behavior (on (resize-event)
                     (run-hook screen-resize-hook))
                 (on (opening)
                     (run-hook screen-opening deco))
                 (on (closing)
                     (run-hook screen-closing deco))
                 ))

;; Default behaviors

(define (check-modifiers mod)
  (lambda (w e)
    (eq? (logand 255 (event-modifier e)) mod)))

(define window-behavior 
  (make-behavior
   (on-event (button 1 any)
             raise-lower-move-window
             :steal (check-modifiers modifiers))
   (on-event (button 2 any)
             user-move-window
             :steal (check-modifiers modifiers))
   (on-event (button 3 any)
             user-iconify-window
             :steal (check-modifiers modifiers))
   ))

(define icon-behavior 
  (make-behavior
   (on-event (button 1 any)
             iconify-move-window)
   (on-event (button 2 any)
             user-move-window)
   (on-event (button 3 any)
             user-iconify-window)
   ))

(define screen-behavior
  (make-behavior
   (on-event (button 3 modifiers)
             (lambda (w e) (wait-for-release e) (end)))
   ))

;; Simple window and icon

(define (simple-window win)
  (make-deco win
             :behavior (make-behavior window-behavior std-window-behavior)))

(define (simple-icon win)
  (make-deco (make-label (window-icon-name win))
             :behavior (make-behavior icon-behavior std-icon-behavior)))

