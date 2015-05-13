;; std-func.scm --- Useful standard functions
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; A set of useful standard functions that should always be loaded.
;; 

(define (inexact->integer num)
  (inexact->exact (round num)))

(define (matches-token win token)
  (cond ((not token) #f)
        ((eq? token #t) #t)
        ((symbol? token) (string=? (window-client-class win) (symbol->string token)))
        ((string? token) (string-match token (window-name win)))
        ((procedure? token) (token win))
        (else #f)))

(define (matches-list win lst)
  (cond ((null? lst) #f)
        ((matches-token win (car lst)) #t)
        (else (matches-list win (cdr lst)))))

(define (matches-cond win lst)
  (cond ((null? lst) #f)
        ((matches-token win (caar lst)) (cdar lst))
        (else (matches-cond win (cdr lst)))))

(define (matches-cond-all win condlst)
  (apply append (map (lambda (ele)
                       (if (matches-token win (car ele)) (cdr ele) '()))
                     condlst)))

(defmacro defaults-to (var val)
  (if (not (defined? var))
      `(define ,var ,val)))

(defmacro defvar (var val doc prop)
  `(begin
     (if (not (defined? ',var))
         (begin (define ,var #f) (set! ,var ,val))) ; Avoid naming of proc
     (set-symbol-property! (quote ,var) 'doc ,doc)
     (set-symbol-property! (quote ,var) 'type ,prop)))

(defmacro make-screen-var (var val)
  (if (> (screen-count) 1)
      `(begin
         (let ((scr (screen)))
           (map (lambda (s)
                  (set-screen! s)
                  (set-property! s (quote ,var) ,val))
                (list-of-screens))
           (set-screen! scr))
         (define ,var
           (lambda arg
             (if (null? arg)
                 (get-property (root-window) (quote ,var))
                 (set-property! (root-window) (quote ,var) (car arg))))))
      `(define ,var
         (let ((val ,val))
           (lambda arg
             (if (null? arg)
                 val
                 (set! val (car arg))))))))

(define (modify-screen-behavior beh)
  (let ((func (lambda (s)
                (if (deco-behavior s)
                    (modify-behavior (deco-behavior s) beh)
                    (set-deco-behavior! s beh)))))
    (if (not (gwm-is-starting))
        (map func (list-of-screens))
        (add-hook! screen-opening func))))

(define (require sym . rest)
  (if (not (defined? sym))
      (if (not (null? rest))
          (primitive-load-path (car rest))
          (primitive-load-path (symbol->string sym)))))

(defmacro autoload (func . rest)
  (if (not (defined? func))
      `(define ,func (lambda args
         (let ((old ,func))
           (primitive-load-path ,(car rest))
           (if (not (eq? old ,func))
               (apply ,func args)
               (error "Failed to autoload" (quote ,func))))))))

(define (add-to-hook! hook proc)
  (if (not (member proc (hook->list hook)))
      (add-hook! hook proc)))

(define (add-to-hook-first! hook proc)
  (if (not (memq proc (hook->list hook)))
      (add-hook! hook proc)
      (let ((lst (hook->list hook)))
        (reset-hook! hook)
        (for-each (lambda (f) (add-hook! hook f))
                  (reverse (cons proc lst))))))

(define (add-to-hook-last! hook proc)
  (let ((lst (hook->list hook)))
    (reset-hook! hook)
    (for-each (lambda (f) (add-hook! hook f))
              (cons proc (reverse (delete proc lst))))))

(define (start-screensaver)
  (set-screen-saver! 1 0 #t #t)
  (sleep 1.5)
  (set-screen-saver! -1 -1 #t #t))

(define (coord->gpos x y)
  (let ((grav #f))
    (cond ((and (< x 0) (< y 0))
           (set! grav 'southeast)
           (set! x (+ (screen-width) x 1))
           (set! y (+ (screen-height) y 1)))
          ((< x 0)
           (set! grav 'northeast)
           (set! x (+ (screen-width) x 1)))
          ((< y 0)
           (set! grav 'southwest)
           (set! y (+ (screen-height) y 1)))
          (#t
           (set! grav 'northwest)))
    (list x y grav)))

(define (get-menu-gpos menu)
  (let ((grav (window-gravity (top-deco menu)))
        (dim (dimensions (top-deco menu)))
        (x #f)
        (y #f))
    (cond ((or (eq? grav 'northwest)
               (eq? grav 'west)
               (eq? grav 'southwest))
           (set! x (car dim)))
          ((or (eq? grav 'northeast)
               (eq? grav 'east)
               (eq? grav 'southeast))
           (set! x (+ (car dim) (caddr dim))))
          ((or (eq? grav 'north)
               (eq? grav 'center)
               (eq? grav 'south))
           (set! x (+ (car dim) (quotient (caddr dim) 2))))
          (#t
           (set! x (+ (deco-x (inner-deco menu)) (deco-borderwidth (inner-deco menu))))
           (set! y (+ (deco-y (inner-deco menu)) (deco-borderwidth (inner-deco menu))))))
    (cond ((or (eq? grav 'northwest)
               (eq? grav 'north)
               (eq? grav 'northeast))
           (set! y (cadr dim)))
          ((or (eq? grav 'southwest)
               (eq? grav 'south)
               (eq? grav 'southeast))
           (set! y (+ (cadr dim) (cadddr dim))))
          ((or (eq? grav 'west)
               (eq? grav 'center)
               (eq? grav 'east))
           (set! y (+ (cadr dim) (quotient (cadddr dim) 2)))))
    (list x y grav)))

(define (gpos->menupos menu gpos)
  (let ((grav (caddr gpos))
        (dim (dimensions (top-deco menu))))
    (cond ((eq? grav 'northwest)
           (cons (car gpos) (cadr gpos)))
          ((eq? grav 'north)
           (cons (- (car gpos) (quotient (caddr dim) 2)) (cadr gpos)))
          ((eq? grav 'northeast)
           (cons (- (car gpos) (caddr dim)) (cadr gpos)))
          ((eq? grav 'west)
           (cons (car gpos) (- (cadr gpos) (quotient (cadddr dim) 2))))
          ((eq? grav 'center)
           (cons (- (car gpos) (quotient (caddr dim) 2)) (- (cadr gpos) (quotient (cadddr dim) 2))))
          ((eq? grav 'east)
           (cons (- (car gpos) (caddr dim)) (- (cadr gpos) (quotient (cadddr dim) 2))))
          ((eq? grav 'southwest)
           (cons (car gpos) (- (cadr gpos) (cadddr dim))))
          ((eq? grav 'south)
           (cons (- (car gpos) (quotient (caddr dim) 2)) (- (cadr gpos) (cadddr dim))))
          ((eq? grav 'southeast)
           (cons (- (car gpos) (caddr dim)) (- (cadr gpos) (cadddr dim))))
          (#t
           (cons (- (+ (car gpos) (car dim)) (deco-x (inner-deco menu)) (deco-borderwidth (inner-deco menu)))
                 (- (+ (cadr gpos) (cadr dim)) (deco-y (inner-deco menu)) (deco-borderwidth (inner-deco menu))))))))

(define (place-menu-gpos menu name gpos . args)
  (let ((pos (gpos->menupos menu gpos)))
    (apply place-menu (append (list menu (root-window) (car pos) (cdr pos)
                                    :name name :gravity (caddr gpos))
                              args))))

