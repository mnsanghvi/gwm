;; advice.scm --- A standard way of redefining procedures
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 1999  Anders Holst
;; Last change: 6/4 1999
;;
;; --------------------------------------------------------------------- 
;;
;; This package implements a way to redefine functions in a controlled
;; and uniform way. It is modeled mainly after the "advice" package in
;; Emacs. If everyone use this package when they redefine functions,
;; this will minimize the risk of collisions between different
;; packages redefining the same function. It will also prevent the
;; case that a function gets ruined because some file is (accidentally)
;; loaded twice, and then tries to redefine the same function twice.
;;
;; The main function 'advice' is used like this:
;;   (advice <FUNCTION> <TAG> <TYPE> <BODY ...>)
;; The <TAG> is a symbol which is used as a label for this piece of
;; advice. If the same function is adviced twice with the same label
;; (and with the same <TYPE>), the old advice will be replaced. Also it
;; makes it possible to remove a particular advice.
;; There are three possible values of <TYPE>: 'before', 'after', and
;; 'around', depending on whether the advice is to be run before, after,
;; or around the call to the original function.
;; <FUNCTION> can actually be either the function name, or a list with
;; the function name followed by formal arguments. These arguments will
;; then be bound to the corresponding actual arguments during execution
;; of the body. If the form without formal arguments is used, the symbol
;; 'args' will be bound to the whole argument list.
;; During execution of the body of an 'around' advice, the function
;; 'advice-inner' should be called where the actions of the original
;; function should go. It should have the same arguments as the original
;; function. It is bad practice not to call the inner function, since
;; this may ruin other around advices.
;;
;; There is also a function 'unadvice' which is used like this:
;;   (unadvice <FUNCTION> [<TAG>])
;; It removes the advice with tag <TAG> from the function (or it
;; removes advices with that tag of all three advice types). If no
;; tag is given, all advices are removed, and the function is set to
;; its original again.
;;
;; If some package would like to be sure to use the original,
;; un-redefined version of some function by some reason, it can always
;; use '(advice-original <FUNCTION>)' to retrieve it. But NOTE, this
;; should *not* normally be used in the body of an 'around' advice, since
;; this would ruin other packages 'around' advices. Use 'advice-inner'
;; there instead. 
;;

;; Mapping procedures to advice info

(define (advice-info proc)
  (procedure-property proc 'advice-info))

(define (set-advice-info! proc info)
  (set-procedure-property! proc 'advice-info info))

(define advice? advice-info)

;; The advice macro

(define (advice-name exp)
  (if (pair? (cadr exp))
      (caadr exp)
      (cadr exp)))

(define (advice-formals exp)
  (if (pair? (cadr exp))
      (cdadr exp)
      'args))

(define (advice-tag exp)
  (if (null? (cddr exp))
      #f
      (caddr exp)))

(define advice-type cadddr)
(define advice-body cddddr)

;; Bugfix below: both set! and define is used, set! to make the advice
;; have effect everywhere (strangely define doesn't do this, which must
;; be a bug), and define to give the new function a name (which can be
;; retrieved with procedure-name).

(define advice
  (procedure->memoizing-macro
    (lambda (exp env)
      `(begin
         (set! ,(advice-name exp)
               (let ((proc (if (advice? ,(advice-name exp))
                               ,(advice-name exp)
                               (make-advice-procedure ,(advice-name exp)))))
                 (add-advice! proc
                              ,(advice-tag exp)
                              ,(advice-type exp)
                              (if (eq? ,(advice-type exp) 'around)
                                  (let ((advice-inner #f))
                                    (cons (lambda ,(advice-formals exp)
                                            ,@(advice-body exp))
                                          (lambda (p)
                                            (set! advice-inner p))))
                                  (lambda ,(advice-formals exp)
                                    ,@(advice-body exp))))
                 proc))
         (define ,(advice-name exp) ,(advice-name exp))))))

(define unadvice
  (procedure->memoizing-macro
    (lambda (exp env)
      `(set! ,(advice-name exp)
	 (let ((proc (if ,(advice-tag exp)
			 ,(advice-name exp)
			 (advice-original ,(advice-name exp)))))
           (remove-advice! proc
                           ,(advice-tag exp))
	   proc)))))

(define (compose-inner around orig)
  (if (null? around)
      orig
      (begin
        ((cddar around) (compose-inner (cdr around) orig))
        (cadar around))))

(define (make-advice-procedure orig)
  (let ((before '())
        (after '())
        (around '())
        (inner #f))
    (let ((proc (lambda args
		  (let ((ans #f))
		    (for-each (lambda (ele)
				(apply (cdr ele) args))
			      before)
                    (set! ans (apply inner args))
		    (for-each (lambda (ele)
				(apply (cdr ele) args))
			      after)
		    ans)))
	  (add-before (lambda (tag proc)
                        (set! before (assq-set! before tag proc))))
	  (add-after (lambda (tag proc)
                       (set! after (reverse! (assq-set! (reverse! after) tag proc)))))
	  (add-around (lambda (tag proc)
                        (set! around (assq-set! around tag proc))
                        (set! inner (compose-inner around orig))))
	  (remove-all (lambda (tag)
                        (set! before (assq-remove! before tag))
                        (set! after (assq-remove! after tag))
                        (set! around (assq-remove! around tag))
                        (set! inner (compose-inner around orig)))))
      (set! inner (compose-inner around orig))
      (set-advice-info! proc (list add-before add-after add-around remove-all orig))
      proc)))

(define (add-advice! proc tag type advice)
  (cond ((eq? type 'before)
         ((car (advice-info proc)) tag advice))
        ((eq? type 'after)
         ((cadr (advice-info proc)) tag advice))
        ((eq? type 'around)
         ((caddr (advice-info proc)) tag advice))
        (#t
         (error "Unknown type tag: " type))))

(define (remove-advice! proc tag)
  (if tag
      ((cadddr (advice-info proc)) tag)))

(define (advice-original proc)
  (let ((info (advice-info proc)))
    (if info
        (car (cddddr info))
        proc)))



