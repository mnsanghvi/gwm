;; bind-key.scm --- Bind keys or buttons to actions dynamically
;;
;; Author: Anders Holst  (aho@sics.se)  
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; The function 'bind-key' can be used to globally (ie in all windows
;; and in the root) bind a key to some action.
;; The binding takes effect immediately after the call.
;;


(define (bind-key-make-arc k thunk take-arg)
  (let ((epat (if (list? k) (key (car k) (apply logior (cdr k))) (key k))))
    (if thunk
        (on-event epat 
                  (if take-arg (lambda (d e) (thunk d)) (lambda (d e) (thunk)))
                  :steal #t)
        (on-event epat))))

(define (bind-key k thunk)
  (let ((arc (bind-key-make-arc k thunk #f)))
    (map (lambda (scr)
           (if (deco-behavior scr)
               (modify-behavior (deco-behavior scr) arc)
               (set-deco-behavior! scr (make-behavior arc))))
         (list-of-screens))
    (if (defined? 'screen-behavior)
        (modify-behavior screen-behavior arc))))

(define (bind-window-key k thunk)
  (let ((arc (bind-key-make-arc k thunk #t)))
    (map (lambda (win)
           (if (deco-behavior win)
               (modify-behavior (deco-behavior win) arc)
               (set-deco-behavior! win (make-behavior arc))))
         (list-of-windows 'window))
    (if (defined? 'window-behavior)
        (modify-behavior window-behavior arc))))

(define (bind-icon-key k thunk)
  (let ((arc (bind-key-make-arc k thunk #t)))
    (map (lambda (win)
           (if (deco-behavior win)
               (modify-behavior (deco-behavior win) arc)
               (set-deco-behavior! win (make-behavior arc))))
         (list-of-windows 'icon))
    (if (defined? 'icon-behavior)
        (modify-behavior icon-behavior arc))))

(defmacro bindkey args
  (let ((range #f)
        (k #f)
        (body #f))
    (if (or (eq? (car args) 'window) (eq? (car args) 'icon))
        (begin
          (set! range (car args))
          (set! args (cdr args))))
    (if (list? (car args))
        (set! k (list 'list (caar args) (cons 'logior (cdar args))))
        (set! k (car args)))
    (if (null? (cdr args))
        (set! body #f)
        (set! body (cons 'lambda (cons (if range (list range) '()) (cdr args)))))
    (cond ((eq? range 'window)
           `(bind-window-key ,k ,body))
          ((eq? range 'icon)
           `(bind-icon-key ,k ,body))
          (#t
           `(bind-key ,k ,body)))))


