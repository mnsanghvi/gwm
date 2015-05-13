;; stack-mgr.scm --- Handle windows with different stacking levels
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; With this package you can specify a "stacking priority" for a window,
;; making it appear under or over other windows. 
;; 
;; Default priority is 0. Negative values make the windows go below, and
;; positive values above. As a convention, use no values above 10 or
;; below -10, and the two extremes should be reserved for gwm packages
;; to implement special features (pan lists e.g.), rather than ordinary
;; windows.
;;

(defvar stack-auto-change #f "Whether relative raise or lower are allowed over priority boundaries." 'boolean)
(defvar stack-priority-list '() "List of (condition level) lists to set the initial priority for windows." 'list)

(define (stack-set-priority win num)
  (set-property! win 'stack-priority num))

(define (stack-get-priority win)
  (or (get-property win 'stack-priority) 0))

(define (stack-find-bottom num win)
  (let ((lst (list-of-windows 'stacking-order)))
    (while (and (pair? lst)
                (or (eq? (window-deco (car lst)) (window-deco win))
                    (< (stack-get-priority (car lst)) num)))
      (set! lst (cdr lst)))
    (if (pair? lst)
        (car lst)
        #f)))

(define (stack-find-top num win)
  (let ((lst (reverse (list-of-windows 'stacking-order))))
    (while (and (pair? lst)
                (or (eq? (window-deco (car lst)) (window-deco win))
                    (> (stack-get-priority (car lst)) num)))
      (set! lst (cdr lst)))
    (if (pair? lst)
        (car lst)
        #f)))

(define (stack-raise-window win other)
  (if other 
      (if (not (eq? win other))
          (let ((op (stack-get-priority other))
                (tp (stack-get-priority win)))
            (cond ((= op tp)
                   ((advice-original raise-window) win other))
                  (stack-auto-change
                   ((advice-original raise-window) win other)
                   (stack-set-priority win op))
                  ((< op tp)
                   (let ((bot (stack-find-bottom tp win)))
                     (if bot
                         ((advice-original lower-window) win bot)
                         ((advice-original raise-window) win))))
                  (#t
                   (let ((top (stack-find-top tp win)))
                     (if top
                         ((advice-original raise-window) win top)
                         ((advice-original lower-window) win)))))))
    (let* ((tp (stack-get-priority win))
           (top (stack-find-top tp win)))
      (if top
          ((advice-original raise-window) win top)
          ((advice-original lower-window) win)))))

(define (stack-lower-window win other)
  (if other 
      (if (not (eq? win other))
          (let ((op (stack-get-priority other))
                (tp (stack-get-priority win)))
            (cond ((= op tp)
                   ((advice-original lower-window) win other))
                  (stack-auto-change
                   ((advice-original lower-window) win other)
                   (stack-set-priority win op))
                  ((> op tp)
                   (let ((top (stack-find-top tp win)))
                     (if top
                         ((advice-original raise-window) win top)
                         ((advice-original lower-window) win))))
                  (#t
                   (let ((bot (stack-find-bottom tp win)))
                     (if bot
                         ((advice-original lower-window) win bot)
                         ((advice-original raise-window) win)))))))
    (let* ((tp (stack-get-priority win))
           (bot (stack-find-bottom tp win)))
      (if bot
          ((advice-original lower-window) win bot)
          ((advice-original raise-window) win)))))

(define (stack-change-priority win num)
  (let ((op (stack-get-priority win)))
    (if (not (= num op))
        (if (< num 0)
            (let ((pos (stack-find-bottom num win)))
              (if pos
                  ((advice-original lower-window) win pos)
                  ((advice-original raise-window) win))
              (stack-set-priority win num))
            (let ((pos (stack-find-top num win)))
              (if pos
                  ((advice-original raise-window) win pos)
                  ((advice-original lower-window) win))
              (stack-set-priority win num))))))

(define (stack-add-window win)
  (let* ((tmp (matches-cond win stack-priority-list))
         (num (if tmp (car tmp) 0))
         (low (if (and tmp (pair? (cdr tmp))) (eq? (cadr tmp) 'lower) #f)))
    (if low
        (let ((pos (stack-find-bottom num win)))
          (if pos
              ((advice-original lower-window) win pos)
              ((advice-original raise-window) win))
          (stack-set-priority win num))
        (let ((pos (stack-find-top num win)))
          (if pos
              ((advice-original raise-window) win pos)
              ((advice-original lower-window) win))
          (stack-set-priority win num)))))

(advice raise-window 'stack 'around
  (stack-raise-window (car args) (if (pair? (cdr args)) (cadr args) #f)))

(advice lower-window 'stack 'around
  (stack-lower-window (car args) (if (pair? (cdr args)) (cadr args) #f)))

(add-to-hook! window-opening stack-add-window)

(add-to-hook! icon-opening stack-add-window)



