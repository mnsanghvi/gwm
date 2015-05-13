;; pick.scm --- Pick a window with the mouse
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; The function (pick-window) lets you pick a window with the mouse, and
;; then returns that window. (kill-picked) lets you pick a window that is
;; killed.
;;

(define (pick-window . args)
  (let ((cursor (get-keyword :cursor args (make-cursor 38)))
        (action (get-keyword :action args (lambda (w) w)))
        (default (get-keyword :default args #f)))
    (car (with-user-feedback (lambda (e) #f)
                             (lambda (e)
                               (if (event-code e)
                                   (if (or (eq? (event-type e) 'ButtonRelease)
                                           (= (event-code e) 1))
                                       (list (action (window-at-position
                                                      (event-x e)
                                                      (event-y e)
                                                      (event-deco e))))
                                       (list default))
                                   #f))
                             :cursor cursor))))

(defmacro with-picked expr
  `(pick-window :action (lambda (window) (if window (begin ,@expr)))))

(define (kill-picked)
  (pick-window :cursor (make-cursor 88)
               :action (lambda (w)
                         (if w
                             (or (delete-window w)
                                 (kill-window w))))))
