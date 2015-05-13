;; multimenu.scm --- A multiple menus popup
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Thise file implements a multiple menus popup, where several menus are
;; poped up at once, and one selection has to be made in each of them
;; before any command is executed. 
;;
;; The only example of practical use so far is when you want to run a
;; command on a remote machine - in one menu you tell which machine, and
;; in another which command.
;;
;; To create a multimenu popup, use the function (construct-multimenu
;; CONTEXT ACTION MENUDESCR1 ...). It takes a number of menu descriptions,
;; each a list of line descriptionsc of the same form as 'construct-menu
;; expects, with the exception that already constructed decorations should
;; not be used, since construct-multimenu needs to make them itself. This
;; means that each menu description is a list of strings (giving labels),
;; the value #t (giving a separator), or lists of a string and a function
;; (giving an item that runs the function). Submenus are also allowed,
;; i.e. the function can be replaced by a new menu description list.
;; The functions in these descriptions takes no argument and should not
;; themselves perform any action, but only return some value. After a
;; selection has been made in each menu, the ACTION argument of 
;; 'construct-multimenu is called with the values returned by each menu as
;; its arguments.
;;

(require 'flet)

(define multimenu-top top-deco)

(define (multimenu-root m)
  (or (get-property (multimenu-top m) 'mroot)
      (multimenu-top m)))

(define (multimenu-selp m)
  (get-property (multimenu-top m) 'selected))

(define (multimenu-getsel m)
  (get-property (multimenu-top m) 'selection))

(define (multimenu-setsel m val)
  (let ((m (multimenu-top m)))
    (set-property! m 'selection val)
    (set-property! m 'selected #t)))

(define (multimenu-donep m)
  (and-map multimenu-selp (get-property (multimenu-root m) 'mlist)))

(define (multimenu-getnextnonsel deco)
  (let* ((mroot (multimenu-root deco))
         (mlist (get-property mroot 'mlist))
         (mcur (get-property mroot 'mcurrent))
         (bef #f)
         (aft #f))
    (map (lambda (x)
           (cond ((eq? x mcur)
                  (set! bef aft)
                  (set! aft #f))
                 ((and (not aft) (not (get-property x 'selected)))
                  (set! aft x))))
         mlist)
    (or aft bef)))
                 
(define multimenu-item-behavior
  (make-behavior
   (on (buttonrelease any any)
       (send-user-event 'doit deco))
   (on (enter) 
       (let ((cur (get-property (menu-father deco) 'current)))
         (if (not (multimenu-selp deco))
             (begin
               (menu-unpop-old-cascade deco)
               (if cur
                   (send-user-event 'inactive cur))
               (send-user-event 'active deco)
               (set-property! (menu-father deco) 'current deco)
               (if (and (not (get-property (menu-father deco) 'cascademenu))
                        (or (get-property deco 'menu)
                            (get-property deco 'menu-expr)))
                   (menu-pop-cascade deco))))))
   (on (leave) 
       (if (not (multimenu-selp deco))
           (if (not (or (get-property deco 'menu)
                        (get-property deco 'menu-expr)))
               (let ((cur (get-property (menu-father deco) 'current)))
                 (send-user-event 'inactive deco)
                 (if (and cur (eq? cur deco))
                     (set-property! (menu-father deco) 'current #f))))))
   (on (user-event 'doit)
       (let ((action (get-property deco 'action))
             (maction #f))
         (multimenu-setsel (multimenu-top deco) action)
         (send-user-event 'inactive (multimenu-top deco))
         (if (multimenu-donep deco)
             (begin
               (set! maction (map multimenu-getsel (get-property (multimenu-root deco) 'mlist)))
               (multimenu-unpop deco)
               (apply (get-property (multimenu-root deco) 'maction) maction))
             (begin
               (set-property! (multimenu-root deco) 'mcurrent (multimenu-getnextnonsel deco))
               (menu-unpop-old-cascade deco)
               (send-user-event 'active deco)
               (if (and (not (get-property (menu-father deco) 'cascademenu))
                        (or (get-property deco 'menu)
                            (get-property deco 'menu-expr)))
                   (menu-pop-cascade deco))))))
   (on (user-event 'item-respond)
       (let ((f (cdr (event-data event))))
         (f deco))) 
   ))

(define multimenu-label-behavior
  (make-behavior 
   (on (buttonrelease any any) 
       (let ((r (multimenu-root deco)))
         (if (and menu-stay-popped
                  (not (get-property r 'nailed)))
             (set-property! r 'nailed #t)
             (multimenu-unpop deco))))
   (on (enter)
       (if (not (multimenu-selp deco))
           (menu-unpop-old-cascade deco)))
   ))

(define multimenu-behavior
  (make-behavior
   (on (buttonpress any any)
       #f)
   (on (buttonrelease any any) 
       (multimenu-unpop deco))
   (on (key "Return")
       (set! deco (or (get-property (multimenu-root deco) 'mcurrent) deco))
       (while (get-property deco 'cascademenu)
         (set! deco (get-property deco 'cascademenu)))
       (let ((cur (get-property deco 'current)))
         (if cur
             (if (or (get-property cur 'menu)
                     (get-property cur 'menu-expr))
                 (menu-pop-cascade cur)
                 (send-user-event 'doit cur)))))
   (on (key "Escape")
       (multimenu-unpop deco))
   (on (key "Down")
       (set! deco (or (get-property (multimenu-root deco) 'mcurrent) deco))
       (while (get-property deco 'cascademenu)
         (set! deco (get-property deco 'cascademenu)))
       (let ((cur (get-property deco 'current))
             (parts (menu-get-item-list deco)))
         (if cur
             (let ((pos (+ (list-index parts cur) 1)))
               (if (>= pos (length parts))
                   (set! pos 0))
               (send-user-event 'inactive cur)
               (set! cur (list-ref parts pos)))
             (set! cur (car parts)))
         (send-user-event 'active cur)
         (set-property! deco 'current cur)))
   (on (key "Up")
       (set! deco (or (get-property (multimenu-root deco) 'mcurrent) deco))
       (while (get-property deco 'cascademenu)
         (set! deco (get-property deco 'cascademenu)))
       (let ((cur (get-property deco 'current))
             (parts (menu-get-item-list deco)))
         (if cur
             (let ((pos (- (list-index parts cur) 1)))
               (if (< pos 0)
                   (set! pos (- (length parts) 1)))
               (send-user-event 'inactive cur)
               (set! cur (list-ref parts pos)))
             (set! cur (car (reverse parts))))
         (send-user-event 'active cur)
         (set-property! deco 'current cur)))
   (on (key "Left")
       (set! deco (or (get-property (multimenu-root deco) 'mcurrent) deco))
       (while (get-property deco 'cascademenu)
         (set! deco (get-property deco 'cascademenu)))
       (let ((par (menu-father deco)))
         (if par
             (menu-unpop-cascade par))))
   (on (key "Right")
       (set! deco (or (get-property (multimenu-root deco) 'mcurrent) deco))
       (while (get-property deco 'cascademenu)
         (set! deco (get-property deco 'cascademenu)))
       (let ((cur (get-property deco 'current)))
         (if (and cur
                  (or (get-property cur 'menu)
                      (get-property cur 'menu-expr)))
             (menu-pop-cascade cur))))
   (on (key "Next")
       (let* ((mroot (multimenu-root deco))
              (mlist (get-property mroot 'mlist))
              (mcur (get-property mroot 'mcurrent))
              (cur (if mcur (get-property mcur 'current) #f))
              (ind (if mlist (list-index mlist mcur) #f)))
         (if cur
             (multimenu-setsel mcur (get-property cur 'action)))
         (if mlist
             (if (and ind (< (+ 1 ind) (length mlist)))
                 (set-property! mroot 'mcurrent (list-ref mlist (+ 1 ind)))
                 (set-property! mroot 'mcurrent (car mlist))))))
   (on (key "Prior")
       (let* ((mroot (multimenu-root deco))
              (mlist (get-property mroot 'mlist))
              (mcur (get-property mroot 'mcurrent))
              (cur (if mcur (get-property mcur 'current) #f))
              (ind (if mlist (list-index mlist mcur) #f)))
         (if cur
             (multimenu-setsel mcur (get-property cur 'action)))
         (if mlist
             (if (and ind (> ind 0))
                 (set-property! mroot 'mcurrent (list-ref mlist (- ind 1)))
                 (set-property! mroot 'mcurrent (list-ref mlist (- (length mlist) 1)))))))
   ))

(define (multimenu-unpop m)
  (let ((mroot (multimenu-root m)))
    (map (lambda (m)
           (set-property! m 'selected #f)
           (set-property! m 'selection #f)
           (send-user-event 'inactive m)
           (menu-unpop-all-cascade m))
         (get-property mroot 'mlist))
    (set-property! mroot 'mcurrent mroot)))

(define (multimenu-popall m x y)
  (map (lambda (mn)
         (if (not (eq? m mn))
             (menu-pop mn (root-window) #f :pos (list x y) :parent m))
         (set! x (+ x (deco-width mn) (* 2 (deco-borderwidth mn)))))
       (get-property m 'mlist)))

(define (construct-multimenu ctx action . args)
  (flet ((std-menu-item-behavior multimenu-item-behavior)
         (std-menu-label-behavior multimenu-label-behavior)
         (std-menu-behavior multimenu-behavior))
    (let* ((reslist (map (lambda (x) (apply construct-menu (cons ctx x))) args))
           (mroot (car reslist)))
      (map (lambda (x) (set-property! x 'mroot mroot)) reslist)
      (set-property! mroot 'mlist reslist)
      (set-property! mroot 'mcurrent mroot)
      (set-property! mroot 'maction 
                     (lambda args
                       (apply action (map (lambda (arg) (apply arg '())) args))))
      (let ((h (get-property mroot 'pop-hook)))
        (if (not h)
            (begin
              (set! h (make-hook 3))
              (add-hook! h multimenu-popall)
              (set-property! mroot 'pop-hook h))
            (add-hook! h multimenu-popall)))
      mroot)))
