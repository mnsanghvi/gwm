;; dialogue.scm --- Provide simple dialogues with the user.
;;
;; Author: Anders Holst  (aho@sics.se)  
;; Copyright (C) 2002  Anders Holst
;;
;; ---------------------------------------------------------------------
;;
;; The two main functions in this file are 'simple-dialogue' and
;; 'complex-dialogue'.
;;
;; (simple-dialogue MESSAGE DEFAULT CONTEXT-ARGS...) prompts with MESSAGE
;; for a text reply with initial value DEFAULT. The string written by the
;; user is returned, or false if the user aborted (by pressing Ctrl-C).
;;
;; (complex-dialogue DIALOGUE-DESCR CONTEXT-ARGS...) constructs a more
;; advanced dialogue based on DIALOGUE-DESCR, and returns a list of the
;; values of all editable fields in the dialoge (or false if the user
;; aborted by presssing "Cancel" instead of "OK"). The dialogue description
;; is a list of any of: a list of the symbol 'header followed by a string,
;; giving a centered header text; a list of 'label followed by a string,
;; giving a left adjusted text label; a list of 'text followed by a default
;; string, giving an editable text field; a list of 'check followed by a
;; boolean, giving a check box; a list of 'radio followed by two symbols,
;; giving a radio button where the first symbol identifies the radio button
;; group and the second the value of the radio button; a list of 'ok
;; optionally followed by a label, giving an OK-button; a list of 'cancel
;; optionally followed by a label, giving a Cancel-button; an empty list
;; representing extendable space; or another dialogue description list.
;; Recursive description lists switch between horizontal and vertical layout.
;;

(require 'edit-plug)

(define (construct-complex-dialogue-intern descr focuslist radiolist beh bctx ictx)
  (cond ((not (list? descr))
         (error "Bad dialogue description: ~S\n" descr))
        ((null? descr)
         (make-deco '() :context bctx))
        ((list? (car descr))
         (apply make-deco (append (map (lambda (x)
                                         (construct-complex-dialogue-intern x focuslist radiolist beh bctx ictx))
                                       descr)
                                  (list :context bctx))))
        ((eq? (car descr) 'header)
         (make-deco '() (make-label (cadr descr) :font (get-keyword :bigfont bctx #f) :horizontal-margin 4 :vertical-margin 2 :context bctx) '()
                    :direction 'horizontal :context bctx))
        ((eq? (car descr) 'label)
         (make-deco (make-label (cadr descr) :horizontal-margin 4 :vertical-margin 2 :context bctx) '()
                    :direction 'horizontal :context bctx))
        ((eq? (car descr) 'text)
         (let ((ep (make-edit-plug (cadr descr) :horizontal-margin 3 :vertical-margin 1 :behavior beh :context ictx)))
           (variable-set! focuslist (append! (variable-ref focuslist) (list ep)))
           (make-deco ep '() :direction 'horizontal :context bctx)))
        ((eq? (car descr) 'alternative)
         (let ((ep (make-alt-plug (cadr descr) (cdr descr) :horizontal-margin 3 :vertical-margin 1 :behavior beh :context ictx)))
           (variable-set! focuslist (append! (variable-ref focuslist) (list ep)))
           (make-deco ep '() :direction 'horizontal :context bctx)))
        ((eq? (car descr) 'check)
         (let ((ep (make-check-plug (cadr descr) :behavior beh :context ictx)))
           (variable-set! focuslist (append! (variable-ref focuslist) (list ep)))
           (make-deco ep :margin 12 :direction 'horizontal :context bctx)))
        ((eq? (car descr) 'radio)
         (let ((ep #f)
               (lst (assq (cadr descr) (variable-ref radiolist))))
           (if (not lst)
               (begin
                 (set! lst (list (cadr descr) (caddr descr)))
                 (variable-set! radiolist (cons lst (variable-ref radiolist)))))
           (set! ep (make-radio-plug (caddr descr) (cdr lst) :behavior beh :context ictx))
           (variable-set! focuslist (append! (variable-ref focuslist) (list ep)))
           (make-deco ep :margin 12 :direction 'horizontal :context bctx)))
;        ((eq? (car descr) 'button)
;         (make-button-plug (cadr descr)
;                             (dialogue-make-recursive-action (cddr descr) bctx)
;                             bctx))
        ((eq? (car descr) 'ok)
         (make-button-plug (if (null? (cdr descr)) " OK " (cadr descr))
                             dialogue-done-action
                             bctx))
        ((eq? (car descr) 'cancel)
             (make-button-plug (if (null? (cdr descr)) "Cancel" (cadr descr))
                                 dialogue-cancel-action
                                 bctx))
        ))

(define (construct-complex-dialogue descr . ctx)
  (let ((focuslist (make-variable '()))
        (radiolist (make-variable '()))
        (bctx (append ctx
                      (list :bigfont (make-font "6x13bold")
                            :font (make-font "6x13")
                            :button-background (make-color "wheat1")
                            :active-button-background (make-color "white")
                            :background (make-color "burlywood1")
                            :foreground (make-color "black"))))
        (ictx (append (let ((vf (get-keyword :value-font ctx #f)))
                        (if vf (list :font vf) '()))
                      ctx
                      (list :value-background (make-color "wheat1")
                            :active-background (make-color "white")
                            :background (make-color "burlywood1")
                            :foreground (make-color "black"))))
        (beh  (make-behavior (on (user-event 'start)
                                 (dialogue-set-focus deco))
                             (on (user-event 'done)
                                 (dialogue-unset-focus deco))
                             (on (user-event 'cancel)
                                 (dialogue-unset-focus deco))
                             (on (keypress (car (keysym->keycode "Tab")) alone)
                                 (dialogue-move-focus-forward deco))
                             (on (keypress (car (keysym->keycode "Tab")) shift-mask)
                                 (dialogue-move-focus-backward deco))))

        (dia #f))
    (set! dia (construct-complex-dialogue-intern descr focuslist radiolist beh
                                                 bctx ictx))
    (modify-deco dia 
                 :behavior (make-behavior
                            (on (focus-in)
                                (dialogue-refresh-focus deco))
                            (on (keypress (car (keysym->keycode "Tab")) alone)
                                (dialogue-move-focus-forward deco))
                            (on (keypress (car (keysym->keycode "Tab")) shift-mask)
                                (dialogue-move-focus-backward deco))
                            (on (keypress "Escape" alone)
                                (dialogue-cancel-action deco event))
                            (on (keypress "Return" alone)
                                (dialogue-done-action deco event)))
                 :property (list (cons 'focus #f) 
                                 (cons 'focuslist (variable-ref focuslist)))
                 :separator 5
                 :margin 5)
    dia))

(define (complex-dialogue dia . args)
  (let ((dia (if (deco? dia)
                 dia
                 (apply construct-complex-dialogue dia args)))
        (dfunc (get-keyword :decoration args #f)))
    (set-property! dia 'done #f)
    (place-menu dia (screen)
                (quotient (- (screen-width (screen))
                             (deco-width dia)) 2)
                (quotient (- (screen-height (screen))
                             (deco-height dia)) 2)
                :gravity 'center
                :context (if dfunc (list :decoration dfunc) '()))
    (focus-window dia)
    (while (not (get-property dia 'done))
      (sleep 0.2))
    (if (list? (get-property dia 'done))
        (get-property dia 'done)
        #f)))

(define (dialogue-done-action d e)
  (let ((res '()))
    (send-user-event (cons 'intern_get (lambda (v) (set! res (cons v res))))
                     (inner-deco d))
    (set-property! (inner-deco d) 'done (reverse res))
    (delete-window (inner-deco d))))

(define (dialogue-cancel-action d e)
  (set-property! (inner-deco d) 'done #t)
  (delete-window (inner-deco d)))

(define (dialogue-set-focus plug)
  (let ((focus (get-property (inner-deco plug) 'focus)))
    (if (and focus (not (eq? focus plug)))
        (edit-plug-done focus))
    (set-property! (inner-deco plug) 'focus plug))
    (set-focus! plug))

(define (dialogue-unset-focus plug)
  (let ((focus (get-property (inner-deco plug) 'focus)))
    (if (and focus (eq? focus plug))
        (begin
          (set-property! (inner-deco plug) 'focus #f)
          (set-focus! (inner-deco plug))))))

(define (dialogue-move-focus-forward plug)
  (let* ((top (inner-deco plug))
         (focus (get-property top 'focus))
         (focuslist (get-property top 'focuslist))
         (ind (list-index focuslist focus)))
    (if ind 
        (set! ind (if (= ind (- (length focuslist) 1))
                      #f
                      (+ ind 1)))
        (if (not (null? focuslist))
            (set! ind 0)))
    (if focus
        (edit-plug-done focus))
    (if ind
        (edit-plug-start (list-ref focuslist ind)))))

(define (dialogue-move-focus-backward plug)
  (let* ((top (inner-deco plug))
         (focus (get-property top 'focus))
         (focuslist (get-property top 'focuslist))
         (ind (list-index focuslist focus)))
    (if ind 
        (set! ind (if (= ind 0)
                      #f
                      (- ind 1)))
        (if (not (null? focuslist))
            (set! ind (- (length focuslist) 1))))
    (if focus
        (edit-plug-done focus))
    (if ind
        (edit-plug-start (list-ref focuslist ind)))))

(define (dialogue-refresh-focus plug)
  (let ((focus (get-property (inner-deco plug) 'focus)))
    (if focus
        (set-focus! focus))))

(define (simple-dialogue str def . args)
  (let* ((ret #f)
         (done #f)
         (beh (make-behavior (on-event (user-event 'done)
                                       (lambda (p e)
                                         (set! ret (ep-get-string p))
                                         (set! done #t)))
                             (on-event (user-event 'cancel)
                                       (lambda (p e)
                                         (set! ret #f)
                                         (set! done #t)))))
         (d (make-deco (make-label str :context args)
                       (make-edit-plug (or def "") :behavior beh :context args)
                       :behavior (make-behavior (on (focus-in) (set-focus! (deco-part deco 2))))
                       :separator 5
                       :margin 5
                       :context args))
         (dfunc (get-keyword :decoration args #f)))
    (place-menu d (screen)
                (quotient (- (screen-width (screen))
                             (deco-width d)) 2)
                (quotient (- (screen-height (screen))
                             (deco-height d)) 2)
                :gravity 'center
                :context (if dfunc (list :decoration dfunc) '()))
    (edit-plug-start (deco-part d 2))
    (focus-window d)
    (while (not done)
      (sleep 0.2))
    (delete-window d)
    ret))

(define dialogue #t)

