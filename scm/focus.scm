;; focus.scm --- Implementation of different focus modes 
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file implements a number of focus modes for Gwm.
;;
;; When the focus-mode is 'enter, a window receives focus when the mouse
;; enters it. There are variables to make it "sloppy" (it won't loose
;; focus just because the window is left), to give it a delay (so that
;; a short stay in the window won't give it the focus), and to autoraise
;; a focused findow. 
;; 
;; When the focus mode is 'click, a window must be clicked to receive
;; focus. There is a variable to regulate whether the click is resent
;; to the client or not. 
;;
;; When the focus mode is 'top, the top-most window will have focus. To
;; avoid floating windows (with high stacking priority) to have to focus
;; all the time, there are variables to set which stacking priorities to
;; consider (or alternatively to ignore).
;; 
;; There is also a function '(focus-window WINDOW)' which sets focus to
;; a window, overriding the normal stacking mode. The window does not
;; loose focus until it dies, or until the function is called again,
;; either with another window or with false.
;; 

(defvar focus-mode 'enter "Current focus mode: 'enter, 'click, or 'top." '(enter click top none))
(defvar focus-auto-raise #f "If true, focused windows are raised." 'boolean)
(defvar focus-enter-delay #f "Number of seconds to delay focus, when focus-mode is 'enter." 'number)
(defvar focus-enter-sloppy #f "If true, windows don't lose focus when they are left, when focus-mode is 'enter." 'boolean)
(defvar focus-click-resend #f "If true, clicks to set focus are resent to the window, when focus-mode is 'click." 'boolean)
(defvar focus-click-raises #f "If true, clicking an obscured window raises it, regardless on focus-mode." 'boolean)
(defvar focus-stack-priorities '() "List of stacking priorities to consider, when focus-mode is 'top." 'list)
(defvar focus-ignore-stack-priorities '(10) "List of stacking priorities to ignore, when focus-mode is 'top." 'list)
(defvar focus-enter-window-list '() "List of window types to treat with focus-mode 'enter." 'list)
(defvar focus-click-window-list '() "List of window types to treat with focus-mode 'click." 'list)
(defvar focus-ignore-window-list '() "List of window types to ignore when setting focus." 'list)

(define focus-locked-target #f)
(define focus-entered-window #f)


(define (focus-window-on-enter? w)
  (cond (focus-locked-target
         #f)
        ((or (not (window-focusable? w))
             (deco-icon? w))
         #f)
        ((eq? focus-mode 'enter)
         (and (not (matches-list w focus-click-window-list))
              (not (matches-list w focus-ignore-window-list))))
        (#t
         (matches-list w focus-enter-window-list))))

(define (focus-window-on-click? w)
  (cond (focus-locked-target
         #f)
        ((or (not (window-focusable? w))
             (deco-icon? w))
         #f)
        ((eq? focus-mode 'click)
         (and (not (matches-list w focus-enter-window-list))
              (not (matches-list w focus-ignore-window-list))))
        (#t
         (matches-list w focus-click-window-list))))

(define (focus-window-on-top? w)
  (cond (focus-locked-target
         #f)
        ((or (not (window-focusable? w))
             (deco-icon? w))
         #f)
        ((not (eq? focus-mode 'top))
         #f)
        ((if (null? focus-stack-priorities)
             (not (member (stack-get-priority w) focus-ignore-stack-priorities))
             (member (stack-get-priority w) focus-stack-priorities))
         (and (not (matches-list w focus-enter-window-list))
              (not (matches-list w focus-click-window-list))
              (not (matches-list w focus-ignore-window-list))))
        (#t
         #f)))

(define (focus-click-action r e)
  (let ((w (window-at-position (event-x e) (event-y e)))
        (foc (get-focus)))
    (cond ((not w)
           #f)
          ((and (if (deco? foc) (not (eq? w (top-deco foc))) #t)
                (focus-window-on-click? w))
           (set-focus! (if (deco-icon? w) (top-deco w) (inner-deco w)))
           (if (and focus-click-raises
                    (not focus-auto-raise)
                    (window-obscured w))
               (begin
                 (raise-window w)
                 #t)
               (not focus-click-resend)))
          ((and focus-click-raises
                (window-obscured w))
           (raise-window w)
           #t)
          (#t
           #f))))

(define focus-root-behavior
  (make-behavior
   (on (user-event 'focus)
       (let ((win (cdr (event-data event))))
         (if (and (focus-window-on-enter? win)
                  (eq? focus-entered-window win))
             (set-focus! (inner-deco win)))))
   (on (button any any)
       '()
       :steal focus-click-action
       :resend #f)
))

(define (focus-enter-action w)
  (set! focus-entered-window w)
  (if (focus-window-on-enter? w)
      (if (and focus-enter-delay (> focus-enter-delay 0))
          (send-timer-event (cons 'focus w) (screen) focus-enter-delay)
          (set-focus! (inner-deco w)))))

(define (focus-leave-action w)
  (if (eq? focus-entered-window w)
      (set! focus-entered-window #f))
  (if (focus-window-on-enter? w)
      (if (not focus-enter-sloppy)
          (set-focus! #t))))

(define (focus-focusin-action w)
  (if focus-auto-raise
      (raise-window w)))

(define (focus-get-top-window)
  (let ((lst (reverse (list-of-windows 'window 'mapped 'stacking-order))))
    (while (and (pair? lst)
                (or (not (window-focusable? (car lst)))
                    (if (null? focus-stack-priorities)
                        (member (stack-get-priority (car lst)) focus-ignore-stack-priorities)
                        (not (member (stack-get-priority (car lst)) focus-stack-priorities)))
                    (matches-list (car lst) focus-enter-window-list)
                    (matches-list (car lst) focus-click-window-list)
                    (matches-list (car lst) focus-ignore-window-list)))
      (set! lst (cdr lst)))
    (if (pair? lst)
        (car lst)
        #f)))

(define (focus-stack-action w)
  (let ((win (focus-get-top-window))
        (foc (get-focus)))
    (if (and win
             (eq? focus-mode 'top)
             (not focus-locked-target)
             (not (and (deco? foc) (eq? (top-deco foc) win))))
        (set-focus! (inner-deco win)))))

(define (focus-map-action w)
  (cond ((focus-window-on-click? w)
         (set-focus! (inner-deco w)))
        ((eq? focus-mode 'top)
         (focus-stack-action w))))

(define (focus-unmap-action w)
  (cond ((eq? focus-locked-target w)
         (set! focus-locked-target #f)
         (reconsider-focus))
        ((eq? focus-mode 'top)
         (focus-stack-action w))
        (#t
         (reconsider-focus))))

(define (focus-popto-action w)
  (if (focus-window-on-click? w)
      (set-focus! (inner-deco w))))

(define (reconsider-focus)
  (let* ((pos (pointer-position))
         (wind (window-at-position (car pos) (cadr pos)))
         (top (focus-get-top-window))
         (foc (get-focus)))
    (cond (focus-locked-target
           (if (and (deco-valid? focus-locked-target)
                    (not (and (deco? foc) (eq? (top-deco foc) focus-locked-target))))
               (set-focus! (inner-deco win))))
          ((and wind (focus-window-on-enter? wind))
           (if (not (and (deco? foc) (eq? (top-deco foc) wind)))
               (set-focus! (inner-deco wind))))
          ((and top (or (eq? focus-mode 'click) (eq? focus-mode 'top)))
           (if (not (and (deco? foc) (eq? (top-deco foc) top)))
               (set-focus! (inner-deco top))))
          ((not foc)
           (set-focus! #t)))))

(define (focus-window win)
  (if (or (not win)
          (eq? win (root-window)))
      (begin
        (set! focus-locked-target #f)
        (reconsider-focus))
      (begin
        (set! focus-locked-target (top-deco win))
        (set-focus! (inner-deco win)))))

(add-hook! enter-window-hook focus-enter-action)
(add-hook! leave-window-hook focus-leave-action)
(add-hook! focus-in-hook focus-focusin-action)

(add-hook! map-window-hook focus-map-action)
(add-hook! unmap-window-hook focus-unmap-action)
(add-hook! pop-to-window-hook focus-popto-action)

(modify-screen-behavior focus-root-behavior)

(advice raise-window 'focus 'after
  (focus-stack-action #f))
(advice lower-window 'focus 'after
  (focus-stack-action #f))

