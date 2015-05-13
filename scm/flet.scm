;; flet.scm --- A simple "fluid let" implementation
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; 'flet' is like 'let' but with dynamic binding, i.e. the global
;; bindings of some symbols are redefined during the call.
;;

(defmacro flset (sym val)
  `(local-define (quote ,(list sym)) ,val))

(defmacro flget (sym)
  `(local-ref (quote ,(list sym))))

(define (fl-sym sym)
  (string->symbol (string-append "_" (symbol->string sym))))

(define (fl-init vars)
  (map (lambda (l)
         (list (fl-sym (car l)) (list 'flget (car l))))
       vars))

(define (fl-start vars)
  (map (lambda (l)
         (list 'flset (car l) (cadr l)))
       vars))

(define (fl-end vars)
  (map (lambda (l)
         (list 'flset (car l) (fl-sym (car l))))
       vars))

(define flet
  (procedure->memoizing-macro
    (lambda (exp env)
      (let ((vars (cadr exp))
            (body (cddr exp))
            (rbody #f)
            (lbody #f))
        (set! rbody (reverse body))
        (set! lbody (list 'set! 'res (car rbody)))
        (set! rbody (reverse (cdr body)))
        (append (list 'let (cons '(res #f) (fl-init vars)))
                (fl-start vars)
                rbody
                (list lbody)
                (fl-end vars)
                (list 'res))))))

