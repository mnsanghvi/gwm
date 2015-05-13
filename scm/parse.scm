;; parse.scm --- Functions to parse other window managers configuration files
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file implements some functions intended for use when parsing 
;; configuration files for other window managers. Currently it is
;; specialized for reading twm and fvwm type configuration files (but
;; not trying to interpret them).
;;
;; Unfortunately this way of reading files tends to be quite slow.
;; Perhaps some general file reader could be implemented on the C level.
;;

(use-modules (ice-9 rdelim))

(define (parse-read-token f)
  (let ((buf (make-string 2000))
        (ch (read-char f))
        (len 0))
    (while (memq ch '(#\space #\tab))
      (set! ch (read-char f)))
    (cond ((eq? ch the-eof-object)
           #f)
          ((eq? ch #\newline)
           #f)
          ((eq? ch #\#)
           (read-line f)
           #f)
          ((memq ch '(#\! #\{ #\} #\( #\) #\:))
           (string->symbol (list->string (list ch))))
          ((eq? ch #\")
           (set! len (cdr (%read-delimited! "\"" buf #t f)))
           (substring buf 0 len))
          (#t
           (unread-char ch f)
           (set! len (cdr (%read-delimited! " \t\n" buf #f f)))
           (if (or (char-numeric? ch)
                   (eq? ch #\-)
                   (eq? ch #\+))
               (string->number (substring/shared buf 0 len))
               (string->symbol (substring/shared buf 0 len)))))))

(define (parse-read-line f)
  (let ((res '())
        (ele (parse-read-token f)))
    (while ele
      (set! res (cons ele res))
      (set! ele (parse-read-token f)))
    (reverse res)))

(define (parse-read-file f)
  (let ((res '())
        (ele (parse-read-line f)))
    (while (not (eq? (peek-char f) the-eof-object))
      (set! res (cons ele res))
      (set! ele (parse-read-line f)))
    (if (not (null? ele))
        (set! res (cons ele res)))
    (reverse res)))

(define (parse-organize-list lst stsym endsym)
  (let ((res '())
        (ele #f)
        (done #f))
    (while (not (or (null? lst)
                    done))
      (set! ele (car lst))
      (set! lst (cdr lst))
      (cond ((null? ele)
             #f)
            ((eq? (car ele) endsym)
             (set! done #t)
             (if (not (null? (cdr ele)))
                 (set! lst (cons (cdr ele) lst))))
            ((eq? (car ele) stsym)
             (let ((ret (parse-organize-list (cons (cdr ele) lst) stsym endsym)))
               (set! res (cons (append (car res) (list (car ret))) (cdr res)))
               (set! lst (cdr ret))))
            ((or (memq stsym ele)
                 (memq endsym ele))
             (let* ((sind (list-index ele stsym))
                    (eind (list-index ele endsym))
                    (ind (if (and sind (or (not eind) (< sind eind)))
                             sind
                             eind)))
               (set! lst (cons (list-head ele ind)
                               (cons (list-tail ele ind)
                                     lst)))))
            (#t
             (set! res (cons ele res)))))
    (cons (reverse res) lst)))

(define (parse-file file)
  (let* ((f (open-file file "r"))
         (lst (parse-read-file f)))
    (close-port f)
    (car (parse-organize-list lst '{ '}))))

