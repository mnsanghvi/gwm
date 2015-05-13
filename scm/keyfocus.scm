;; keyfocus.scm --- Switch focus via the keyboard
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file implements four functions: setting focus to the next or
;; previous window of all mapped and focusable windows, or setting focus
;; to the next or previous window of all windows. It then binds those
;; functions to keys: M-C-next, M-C-prev, M-C-S-next, and M-C-S-prev.
;;
;; (Which keys to use ought of course be easily customizable.)
;;

(define (keyfocus-window-list)
  (let ((wins (list-of-windows 'window 'mapped))
        (res '()))
    (for-each (lambda (w) 
                (if (and (not (deco-menu? (inner-deco w)))
                         (window-focusable? w))
                    (set! res (cons w res))))
              wins)
    (reverse res)))

(define (keyfocus-set win)
  (pop-to-window win)
  (set-focus! (inner-deco win)))

(define (focus-next)
  (let* ((wins (keyfocus-window-list))
         (foc (get-focus))
         (ind (if (deco? foc) (list-index wins (top-deco foc)) #f))
         (len (length wins)))
    (if (> len 0)
        (if ind
            (keyfocus-set (list-ref wins (remainder (+ 1 ind) len)))
            (keyfocus-set (car wins))))))

(define (focus-prev)
  (let* ((wins (keyfocus-window-list))
         (foc (get-focus))
         (ind (if (deco? foc) (list-index wins (top-deco foc)) #f))
         (len (length wins)))
    (if (> len 0)
        (if ind
            (keyfocus-set (list-ref wins (remainder (+ len ind -1) len)))
            (keyfocus-set (car (reverse wins)))))))

(define (focus-next-all)
  (let* ((wins (list-of-windows 'window))
         (foc (get-focus))
         (ind (if (deco? foc) (list-index wins (top-deco foc)) #f))
         (len (length wins)))
    (if (> len 0)
        (if ind
            (keyfocus-set (list-ref wins (remainder (+ 1 ind) len)))
            (keyfocus-set (car wins))))))

(define (focus-prev-all)
  (let* ((wins (list-of-windows 'window))
         (foc (get-focus))
         (ind (if (deco? foc) (list-index wins (top-deco foc)) #f))
         (len (length wins)))
    (if (> len 0)
        (if ind
            (keyfocus-set (list-ref wins (remainder (+ len ind -1) len)))
            (keyfocus-set (car (reverse wins)))))))

(define keyfocus-behavior
  (make-behavior
   (on (key "Prior" (logior control-mask alt-mask))
       (focus-prev)
       :steal #t)
   (on (key "Next" (logior control-mask alt-mask))
       (focus-next)
       :steal #t)
   (on (key "Prior" (logior control-mask alt-mask shift-mask))
       (focus-prev-all)
       :steal #t)
   (on (key "Next" (logior control-mask alt-mask shift-mask))
       (focus-next-all)
       :steal #t)))

(modify-screen-behavior keyfocus-behavior)

(define keyfocus #t)
