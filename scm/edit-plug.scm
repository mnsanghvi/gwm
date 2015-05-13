;; edit-plug.scm --- An editable text widget, plus some relatives.
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; (make-edit-plug INITIAL-STRING CONTEXT-ARGS...) constructs
;; an editable text plug.
;; 
;; (make-multi-edit-plug INITIAL-STRING-LIST CONTEXT-ARGS...) constructs
;; an editable multiline text plug.
;; 
;; (make-alt-plug INITIAL-VALUE VALUE-LIST CONTEXT-ARGS...) constructs
;; a plug with values selected from a menu.
;;
;; (make-alt-edit-plug INITIAL-STRING STRING-LIST CONTEXT-ARGS...)
;; constructs an editable text plug, which can also be set by selecting
;; values from a menu (caching last values).
;;
;; (make-check-plug INITIAL-STATE CONTEXT-ARGS...) constructs
;; an interactive check box plug.
;; 
;; (make-radio-plug IDENTITY LIST-OF-STATE CONTEXT-ARGS...) constructs
;; an interactive radio button plug. IDENTITY is a symbol representing
;; the value of this radio plug, and LIST-OF-STATE should be a pair
;; with the identity of the initially active radio plug as its first
;; element, and the same list should be used for all radio plugs in the
;; same group. 
;; 
;; With (edit-plug-get PLUG) and (edit-plug-set PLUG VALUE) you can
;; get and set the current contents of the plugs. For an edit-plug or
;; alt-edit-plug the value is a string, for a multi-edit-plug it is a
;; list of strings, for an alt-plug it is whatever is in its value list,
;; for a check-plug it is a boolean, and for a group of radio plugs a
;; symbol. (When using radio plugs, these functions should be called on
;; a deco containing all radio plugs in the same group, or alternatively
;; once for each plug in the group.)
;;
;; With (edit-plug-start PLUG [EVENT]), (edit-plug-done PLUG [EVENT])
;; and (edit-plug-cancel PLUG [EVENT]) you can make the plug switch
;; between editable and passive state. By binding actions to the
;; user-events 'start, 'done and 'cancel you can execute some code
;; whenever the plug switches state.
;;


(define (edit-plug-start plug . event)
  (send-user-event 'intern_start plug)
  (send-user-event 'start plug))

(define (edit-plug-done plug . event)
  (send-user-event 'intern_done plug)
  (send-user-event 'done plug))

(define (edit-plug-cancel plug . event)
  (send-user-event 'intern_cancel plug)
  (send-user-event 'cancel plug))

(define (edit-plug-set plug val)
  (send-user-event (cons 'intern_set val) plug))

(define (edit-plug-get plug)
  (let ((res #f))
    (send-user-event (cons 'intern_get (lambda (val) (set! res val))) plug)
    res))

(define (make-edit-plug str . args)
  (let ((bg (or (get-keyword :value-background args #f)
                (get-keyword :background args #f)
                (make-color "white")))
        (abg (get-keyword :active-background args #f))
        (fg (or (get-keyword :foreground args #f) (make-color "black")))
        (afg (get-keyword :active-foreground args #f))
        (bw (get-keyword :borderwidth args 1))
        (bc (or (get-keyword :bordercolor args #f)
                (get-keyword :background args #f)
                (make-color "white")))
        (abc (or (get-keyword :active-bordercolor args #f)
                 (get-keyword :bordercolor args #f)
                 (get-keyword :foreground args #f)
                 (make-color "black")))
        (font (or (get-keyword :font args #f) (make-font "fixed")))
        (hmarg (get-keyword :horizontal-margin args 2))
        (vmarg (get-keyword :vertical-margin args 2))
        (prop (get-keyword :property args '()))
        (beh (get-keyword :behavior args #f))
        (plug #f)
        (tmpbeh #f))
    (set! tmpbeh (if beh (make-behavior beh ep-behavior1) ep-behavior1))
    (set! plug (make-deco '()
                          :behavior tmpbeh
                          :property (append
                                     `((string) (pos . 0)
                                       (orig . #f) (active . #f)
                                       (ctx1 :background ,bg :foreground ,fg
                                             :horizontal-margin ,hmarg
                                             :vertical-margin ,vmarg
                                             :font ,font)
                                       (ctx2 :background ,(or abg bg)
                                             :foreground ,(or afg fg)
                                             :horizontal-margin ,hmarg
                                             :vertical-margin ,vmarg
                                             :font ,font)
                                       (beh1 . ,tmpbeh)
                                       (beh2 . ,(if beh
                                                    (make-behavior beh ep-behavior2)
                                                    ep-behavior2))
                                       (slant . ,(caddr (font-dimensions font)))
                                       (bc . ,bc) (abc . ,abc))
                                     prop)
                          :borderwidth bw
                          :bordercolor bc))
    (ep-set-string plug str 0)
    plug))

(define (ep-set-string plug str pos)
  (let* ((active (get-property plug 'active))
         (ctx (if active
                  (get-property plug 'ctx2)
                  (get-property plug 'ctx1)))
         (pix (make-label str :context ctx))
         (font (get-keyword :font ctx #f))
         (vmarg (get-keyword :vertical-margin ctx #f))
         (hmarg (get-keyword :horizontal-margin ctx #f))
         (pos (max 0 (min (string-length str) pos)))
         (size #f)
         (slant #f))
    (if active
        (begin
          (set! size (string-dimensions (substring str 0 pos) font))
          (set! slant (get-property plug 'slant))
          (draw-line pix (+ (car size) slant hmarg -1) vmarg
                         (+ (car size) hmarg -1) (+ vmarg (cadr size))
                         :color (get-keyword :foreground ctx #f))))
    (set-deco-part! plug 1 pix)
    (set-property! plug 'pos pos)
    (set-property! plug 'string str)))

(define (ep-get-string plug)
  (get-property plug 'string))

(define (ep-get-pos plug)
  (get-property plug 'pos))

(define (ep-set-pos plug npos)
  (let ((active (get-property plug 'active))
        (str (get-property plug 'string))
        (opos (get-property plug 'pos)))
    (set! npos (max 0 (min (string-length str) npos)))
    (if active
        (if (not (= npos opos))
            (ep-set-string plug str npos))
        (set-property! plug 'pos npos))))

(define (ep-forward plug . event)
  (ep-set-pos plug (+ (ep-get-pos plug) 1)))

(define (ep-backward plug . event)
  (ep-set-pos plug (- (ep-get-pos plug) 1)))

(define (ep-bol plug . event)
  (ep-set-pos plug 0))

(define (ep-eol plug . event)
  (ep-set-pos plug (string-length (ep-get-string plug))))

(define (ep-insert-char plug event)
  (let ((str (ep-get-string plug))
        (pos (ep-get-pos plug))
        (ch (if (xevent? event) (event-key event) (if (char? event) (char->string event) #f))))
    (if ch
        (ep-set-string plug
                       (string-append (substring str 0 pos)
                                      ch
                                      (substring str pos (string-length str)))
                       (+ pos 1)))))

(define (ep-delete plug . event)
  (let ((str (ep-get-string plug))
        (pos (ep-get-pos plug)))
    (ep-set-string plug
                   (string-append (substring str 0 pos)
                                  (substring str (min (string-length str) (+ pos 1)) (string-length str)))
                   pos)))

(define (ep-backspace plug . event)
  (let ((str (ep-get-string plug))
        (pos (ep-get-pos plug)))
    (ep-set-string plug
                   (string-append (substring str 0 (max 0 (- pos 1)))
                                  (substring str pos (string-length str)))
                   (max 0 (- pos 1)))))

(define (ep-delete-rest plug . event)
  (let ((str (ep-get-string plug))
        (pos (ep-get-pos plug)))
    (ep-set-string plug
                   (substring str 0 pos)
                   pos)))

(define (ep-clear plug . event)
  (ep-set-string plug "" 0))

(define (ep-undo plug . event)
  (let ((orig (get-property plug 'orig)))
    (if orig
        (ep-set-string plug orig 0))))

(define (ep-get-mouse-pos plug event)
  (let* ((ctx (get-property plug 'ctx2))
         (font (get-keyword :font ctx #f))
         (hmarg (get-keyword :horizontal-margin ctx #f))
         (x (- (event-relative-x event) hmarg -2))
         (str (ep-get-string plug))
         (len (string-length str))
         (size (car (string-dimensions str font)))
         (ind (quotient (* len x) (max size 1))))
    (cond ((<= ind 0)
           0)
          ((>= ind len)
           len)
          ((< x (car (string-dimensions (substring str 0 ind) font)))
           (set! ind (- ind 1))
           (while (and (> ind 0) (< x (car (string-dimensions (substring str 0 ind) font))))
             (set! ind (- ind 1)))
           ind)
          (#t
           (set! ind (+ ind 1))
           (while (and (< ind len) (> x (car (string-dimensions (substring str 0 ind) font))))
             (set! ind (+ ind 1)))
           (- ind 1)))))

(define (ep-mouse-position plug event)
  (let ((ind (ep-get-mouse-pos plug event)))
    (ep-set-pos plug ind)))

(define (ep-mouse-select plug event)
  (let ((ind (ep-get-mouse-pos plug event))
        (pos (ep-get-pos plug))
        (str (ep-get-string plug)))
    (if (not (= ind pos))
        (set-cut-buffer! (substring str (min ind pos) (max ind pos))))))

(define (ep-mouse-yank plug event)
  (let ((ind (ep-get-mouse-pos plug event))
        (str (ep-get-string plug))
        (cutstr (cut-buffer)))
    (ep-set-string plug
                   (string-append (substring str 0 ind)
                                  cutstr
                                  (substring str ind (string-length str)))
                   (+ ind (string-length cutstr)))))

(define (ep-activate plug)
  (let ((active (get-property plug 'active))
        (str (get-property plug 'string))
        (pos (get-property plug 'pos)))
    (if (not active)
        (begin
          (set-deco-behavior! plug (get-property plug 'beh2))
          (set-property! plug 'orig str)
          (set-property! plug 'active #t)
          (set-deco-bordercolor! plug (get-property plug 'abc))
          (ep-set-string plug str pos)))))

(define (ep-deactivate plug)
  (let ((active (get-property plug 'active))
        (str (get-property plug 'string))
        (pos (get-property plug 'pos)))
    (if active
        (begin
          (set-deco-behavior! plug (get-property plug 'beh1))
          (set-property! plug 'orig #f)
          (set-property! plug 'active #f)
          (set-deco-bordercolor! plug (get-property plug 'bc))
          (ep-set-string plug str pos)))))

(define (ep-intern-set plug event)
  (let ((val (cdr (event-data event))))
    (if (string? val)
        (ep-set-string plug val 0))))

(define (ep-intern-get plug event)
  (let ((func (cdr (event-data event))))
    (func (ep-get-string plug))))

(define ep-behavior1
  (make-behavior
   (on-event (user-event 'intern_start) (lambda (p e) (ep-activate p)))
   (on-event (user-event 'intern_set) ep-intern-set)
   (on-event (user-event 'intern_get) ep-intern-get)
   (on-event (button 1 any)
             (lambda (p e) (edit-plug-start p) (ep-mouse-position p e))
             (lambda (p e) (ep-mouse-select p e)))
   (on-event (button 2 any)
             (lambda (p e) (edit-plug-start p) (ep-mouse-yank p e)))
   (on-event (key "Return") edit-plug-start)))

(define ep-behavior2
  (make-behavior
   (on-event (user-event 'intern_done) (lambda (p e) (ep-deactivate p)))
   (on-event (user-event 'intern_cancel) (lambda (p e) (ep-undo p) (ep-deactivate p)))
   (on-event (user-event 'intern_set) ep-intern-set)
   (on-event (user-event 'intern_get) ep-intern-get)
   (on-event (button 1 any)
             (lambda (p e) (ep-mouse-position p e))
             (lambda (p e) (ep-mouse-select p e)))
   (on-event (button 2 any)
             (lambda (p e) (ep-mouse-yank p e)))
   (on-event (keypress "Return") edit-plug-done)
   (on-event (keypress "Escape") edit-plug-cancel)
   (on-event (keypress "c" control-mask) edit-plug-cancel)
   (on-event (keypress "a" control-mask) ep-bol)
   (on-event (keypress "Home") ep-bol)
   (on-event (keypress "e" control-mask) ep-eol)
   (on-event (keypress "End") ep-eol)
   (on-event (keypress "f" control-mask) ep-forward)
   (on-event (keypress "Right") ep-forward)
   (on-event (keypress "b" control-mask) ep-backward)
   (on-event (keypress "Left") ep-backward)
   (on-event (keypress "d" control-mask) ep-delete)
   (on-event (keypress "Delete") ep-delete)
   (on-event (keypress "BackSpace") ep-backspace)
   (on-event (keypress "k" control-mask) ep-delete-rest)
   (on-event (keypress "w" control-mask) ep-clear)
   (on-event (keypress "u" control-mask) ep-undo)
   (on-event (keypress any any) ep-insert-char)))


;;-------------------------------------------------------------

(define (make-multi-edit-plug strl . args)
  (let ((bg (or (get-keyword :value-background args #f)
                (get-keyword :background args #f)
                (make-color "white")))
        (abg (get-keyword :active-background args #f))
        (fg (or (get-keyword :foreground args #f) (make-color "black")))
        (afg (get-keyword :active-foreground args #f))
        (bw (get-keyword :borderwidth args 1))
        (bc (or (get-keyword :bordercolor args #f)
                (get-keyword :background args #f)
                (make-color "white")))
        (abc (or (get-keyword :active-bordercolor args #f)
                 (get-keyword :bordercolor args #f)
                 (get-keyword :foreground args #f)
                 (make-color "black")))
        (font (or (get-keyword :font args #f) (make-font "fixed")))
        (hmarg (get-keyword :horizontal-margin args 2))
        (vmarg (get-keyword :vertical-margin args 2))
        (prop (get-keyword :property args '()))
        (beh (get-keyword :behavior args #f))
        (plug #f)
        (tmpbeh #f))
    (set! tmpbeh (if beh (make-behavior beh mep-behavior1) mep-behavior1))
    (set! plug (make-deco '()
                          :behavior tmpbeh
                          :property (append
                                     `((string) (pos . (0 . 0))
                                       (orig . #f) (active . #f)
                                       (ctx1 :background ,bg :foreground ,fg
                                             :horizontal-margin ,hmarg
                                             :vertical-margin ,vmarg
                                             :font ,font)
                                       (ctx2 :background ,(or abg bg)
                                             :foreground ,(or afg fg)
                                             :horizontal-margin ,hmarg
                                             :vertical-margin ,vmarg
                                             :font ,font)
                                       (beh1 . ,tmpbeh)
                                       (beh2 . ,(if beh
                                                    (make-behavior beh mep-behavior2)
                                                    mep-behavior2))
                                       (fdim . ,(font-dimensions font))
                                       (bc . ,bc) (abc . ,abc))
                                     prop)
                          :borderwidth bw
                          :bordercolor bc))
    (mep-set-string plug strl '(0 . 0))
    plug))

(define (mep-set-string plug strl pos)
  (let* ((active (get-property plug 'active))
         (ctx (if active
                  (get-property plug 'ctx2)
                  (get-property plug 'ctx1)))
         (font (get-keyword :font ctx #f))
         (vmarg (get-keyword :vertical-margin ctx #f))
         (hmarg (get-keyword :horizontal-margin ctx #f))
         (fg (get-keyword :foreground ctx #f))
         (dims (map (lambda (s) (car (string-dimensions s font))) strl))
         (fdim (get-property plug 'fdim))
         (pix (make-pixmap (+ (* 2 hmarg) (apply max dims))
                           (+ vmarg (* (length strl) (+ vmarg (cadr fdim))))
                           :background (get-keyword :background ctx #f)))
         (pos (cond ((< (car pos) 0)
                     '(0 . 0))
                    ((>= (car pos) (length strl))
                     (cons (- (length strl) 1) (string-length (car (reverse strl)))))
                    (#t
                     (cons (car pos)
                           (max 0 (min (string-length (list-ref strl (car pos))) (cdr pos)))))))
         (size #f)
         (ind 0))
    (for-each (lambda (s)
                (draw-text pix (- hmarg (caddr fdim)) (+ vmarg (* ind (+ vmarg (cadr fdim))) (cadddr fdim))
                           s :font font :foreground fg)
                (set! ind (+ ind 1)))
              strl)
    (if active
        (begin
          (set! size (string-dimensions (substring (list-ref strl (car pos)) 0 (cdr pos)) font))
          (draw-line pix (+ (car size) (caddr fdim) hmarg -1) (+ vmarg (* (car pos) (+ vmarg (cadr fdim))))
                         (+ (car size) hmarg -1) (* (+ (car pos) 1) (+ vmarg (cadr fdim)))
                         :color fg)))
    (set-deco-part! plug 1 pix)
    (set-property! plug 'pos pos)
    (set-property! plug 'string strl)))

(define (mep-get-string plug)
  (get-property plug 'string))

(define (mep-get-pos plug)
  (get-property plug 'pos))

(define (mep-set-pos plug npos)
  (let ((active (get-property plug 'active))
        (strl (get-property plug 'string))
        (opos (get-property plug 'pos)))
    (set! npos (cond ((< (car npos) 0)
                      '(0 . 0))
                     ((>= (car npos) (length strl))
                      (cons (- (length strl) 1) (string-length (car (reverse strl)))))
                     (#t
                      (cons (car npos)
                            (max 0 (min (string-length (list-ref strl (car npos))) (cdr npos)))))))
    (if active
        (if (not (equal? npos opos))
            (mep-set-string plug strl npos))
        (set-property! plug 'pos npos))))

(define (mep-forward plug . event)
  (let ((str (mep-get-string plug))
        (pos (mep-get-pos plug)))
    (if (= (cdr pos) (string-length (list-ref str (car pos))))
        (mep-set-pos plug (cons (+ (car pos) 1) 0))
        (mep-set-pos plug (cons (car pos) (+ (cdr pos) 1))))))

(define (mep-backward plug . event)
  (let ((str (mep-get-string plug))
        (pos (mep-get-pos plug)))
    (if (and (= (cdr pos) 0) (> (car pos) 0))
        (mep-set-pos plug (cons (- (car pos) 1) (string-length (list-ref str (- (car pos) 1)))))
        (mep-set-pos plug (cons (car pos) (- (cdr pos) 1))))))

(define (mep-bol plug . event)
  (let ((pos (mep-get-pos plug)))
    (mep-set-pos plug (cons (car pos) 0))))

(define (mep-eol plug . event)
  (let ((str (mep-get-string plug))
        (pos (mep-get-pos plug)))
    (mep-set-pos plug (cons (car pos) (string-length (list-ref str (car pos)))))))

(define (mep-bob plug . event)
  (mep-set-pos plug '(0 . 0)))

(define (mep-eob plug . event)
  (let ((str (mep-get-string plug))
        (pos (mep-get-pos plug)))
    (mep-set-pos plug (cons (- (length str) 1) (string-length (car (reverse str)))))))

(define (mep-up plug . event)
  (let ((pos (mep-get-pos plug)))
    (mep-set-pos plug (cons (- (car pos) 1) (cdr pos)))))

(define (mep-down plug . event)
  (let ((pos (mep-get-pos plug)))
    (mep-set-pos plug (cons (+ (car pos) 1) (cdr pos)))))

(define (mep-insert-char plug event)
  (let ((str (mep-get-string plug))
        (pos (mep-get-pos plug))
        (ch (if (xevent? event) (event-key event) (if (char? event) (char->string event) #f))))
    (if ch
        (begin
          (list-set! str (car pos) 
                     (string-append (substring (list-ref str (car pos)) 0 (cdr pos))
                                    ch
                                    (substring (list-ref str (car pos)) (cdr pos))))
          (mep-set-string plug
                          str
                          (cons (car pos) (+ (cdr pos) 1)))))))

(define (mep-newline plug . event)
  (let* ((str (mep-get-string plug))
         (pos (mep-get-pos plug))
         (os (list-ref str (car pos))))
    (set! str (append (list-head str (car pos))
                      (list (substring os 0 (cdr pos))
                            (substring os (cdr pos)))
                      (list-tail str (+ (car pos) 1))))
    (mep-set-string plug str (cons (+ (car pos) 1) 0))))

(define (mep-open-line plug . event)
  (let* ((str (mep-get-string plug))
         (pos (mep-get-pos plug))
         (os (list-ref str (car pos))))
    (if (and (> (cdr pos) 0) (= (cdr pos) (string-length (list-ref str (car pos)))))
        (set! pos (cons (+ (car pos) 1) (cdr pos))))
    (set! str (append (list-head str (car pos))
                      (list "")
                      (list-tail str (car pos))))
    (mep-set-string plug str (cons (car pos) 0))))

(define (mep-delete plug . event)
  (let* ((str (mep-get-string plug))
         (pos (mep-get-pos plug))
         (os (list-ref str (car pos))))
    (if (= (cdr pos) (string-length os))
        (if (< (car pos) (- (length str) 1))
            (begin
              (set! str (append (list-head str (car pos))
                                (list (string-append os (list-ref str (+ (car pos) 1))))
                                (list-tail str (+ (car pos) 2))))
              (mep-set-string plug str pos)))
        (begin
          (list-set! str (car pos)
                     (string-append (substring os 0 (cdr pos))
                                    (substring os (min (string-length os)
                                                       (+ (cdr pos) 1)))))
          (mep-set-string plug str pos)))))

(define (mep-backspace plug . event)
  (let* ((str (mep-get-string plug))
         (pos (mep-get-pos plug))
         (os (list-ref str (car pos))))
    (if (= (cdr pos) 0)
        (if (> (car pos) 0)
            (let ((os1 (list-ref str (- (car pos) 1))))
              (set! str (append (list-head str (- (car pos) 1))
                                (list (string-append os1 os))
                                (list-tail str (+ (car pos) 1))))
              (mep-set-string plug str (cons (- (car pos) 1) (string-length os1)))))
        (begin
          (list-set! str (car pos)
                     (string-append (substring os 0 (max 0 (- (cdr pos) 1)))
                                    (substring os (cdr pos))))
          (mep-set-string plug str (cons (car pos) (max 0 (- (cdr pos) 1))))))))

(define (mep-delete-rest plug . event)
  (let ((str (mep-get-string plug))
        (pos (mep-get-pos plug)))
    (list-set! str (car pos)
               (substring (list-ref str (car pos)) 0 (cdr pos)))
    (mep-set-string plug str pos)))

(define (mep-clear plug . event)
  (mep-set-string plug (list "") '(0 . 0)))

(define (mep-undo plug . event)
  (let ((orig (get-property plug 'orig)))
    (if orig
        (mep-set-string plug orig '(0 . 0)))))

(define (mep-get-mouse-pos plug event)
  (let* ((ctx (get-property plug 'ctx2))
         (font (get-keyword :font ctx #f))
         (vmarg (get-keyword :vertical-margin ctx #f))
         (hmarg (get-keyword :horizontal-margin ctx #f))
         (strl (mep-get-string plug))
         (dims (map (lambda (s) (car (string-dimensions s font))) strl))
         (fdim (get-property plug 'fdim))
         (x (- (event-relative-x event) hmarg -2))
         (y (- (event-relative-y event) (quotient vmarg 2)))
         (indy (max 0 (min (- (length strl) 1) (quotient y (+ vmarg (cadr fdim))))))
         (os (list-ref strl indy))
         (len (string-length os))
         (indx (quotient (* len x) (max (car (string-dimensions os font)) 1))))
    (cond ((<= indx 0)
           (set! indx 0))
          ((>= indx len)
           (set! indx len))
          ((< x (car (string-dimensions (substring os 0 indx) font)))
           (set! indx (- indx 1))
           (while (and (> indx 0) (< x (car (string-dimensions (substring os 0 indx) font))))
             (set! indx (- indx 1))))
          (#t
           (set! indx (+ indx 1))
           (while (and (< indx len) (> x (car (string-dimensions (substring os 0 indx) font))))
             (set! indx (+ indx 1)))
           (set! indx (- indx 1))))
    (cons indy indx)))

(define (mep-string->list str)
  (let ((res '())
        (ind1 0)
        (ind2 #f))
    (while ind1
      (set! ind2 (string-index str #\newline ind1))
      (if ind2
          (set! res (cons (substring str ind1 ind2) res))
          (set! res (cons (substring str ind1) res)))
      (set! ind1 (if ind2 (+ ind2 1) #f)))
    (reverse res)))

(define (mep-list->string lst)
  (let ((tmp (list (car lst))))
    (for-each (lambda (s)
                (set! tmp (cons s (cons "\n" tmp))))
              (cdr lst))
    (apply string-append (reverse tmp))))

(define (mep-mouse-position plug event)
  (let ((ind (mep-get-mouse-pos plug event)))
    (mep-set-pos plug ind)))

(define (mep-mouse-select plug event)
  (let ((ind (mep-get-mouse-pos plug event))
        (pos (mep-get-pos plug))
        (str (mep-get-string plug)))
    (if (not (equal? ind pos))
        (if (= (car ind) (car pos))
            (set-cut-buffer! (substring (list-ref str (car pos))
                                        (min (cdr ind) (cdr pos))
                                        (max (cdr ind) (cdr pos))))
            (let ((p1 (if (< (car ind) (car pos)) ind pos))
                  (p2 (if (< (car ind) (car pos)) pos ind)))
              (set-cut-buffer! (mep-list->string (append (list (substring (list-ref str (car p1)) (cdr p1)))
                                                         (list-tail (list-head str (car p2)) (+ (car p1) 1))
                                                         (list (substring (list-ref str (car p2)) 0 (cdr p2))))))))))) 

(define (mep-mouse-yank plug event)
  (let ((ind (mep-get-mouse-pos plug event))
        (str (mep-get-string plug))
        (cutlst (mep-string->list (cut-buffer)))
        (len #f))
    (set-car! cutlst
              (string-append (substring (list-ref str (car ind)) 0 (cdr ind))
                             (car cutlst)))
    (set! len (string-length (list-ref cutlst (- (length cutlst) 1))))
    (list-set! cutlst (- (length cutlst) 1) 
               (string-append (list-ref cutlst (- (length cutlst) 1))
                              (substring (list-ref str (car ind)) (cdr ind))))
    (mep-set-string plug
                    (append (list-head str (car ind))
                            cutlst
                            (list-tail str (+ (car ind) 1)))
                    (cons (+ (car ind) (length cutlst) -1) len))))

(define (mep-activate plug)
  (let ((active (get-property plug 'active))
        (str (get-property plug 'string))
        (pos (get-property plug 'pos)))
    (if (not active)
        (begin
          (set-deco-behavior! plug (get-property plug 'beh2))
          (set-property! plug 'orig str)
          (set-property! plug 'active #t)
          (set-deco-bordercolor! plug (get-property plug 'abc))
          (mep-set-string plug (list-copy str) pos)))))

(define (mep-deactivate plug)
  (let ((active (get-property plug 'active))
        (str (get-property plug 'string))
        (pos (get-property plug 'pos)))
    (if active
        (begin
          (set-deco-behavior! plug (get-property plug 'beh1))
          (set-property! plug 'orig #f)
          (set-property! plug 'active #f)
          (set-deco-bordercolor! plug (get-property plug 'bc))
          (mep-set-string plug str pos)))))

(define (mep-intern-set plug event)
  (let ((val (cdr (event-data event))))
    (if (and (list? val) (and-map string? val))
        (mep-set-string plug val '(0 . 0))
        (if (string? val)
            (mep-set-string plug (mep-string->list val) '(0 . 0))))))

(define (mep-intern-get plug event)
  (let ((func (cdr (event-data event))))
    (func (mep-get-string plug))))

(define mep-behavior1
  (make-behavior
   (on-event (user-event 'intern_start) (lambda (p e) (mep-activate p)))
   (on-event (user-event 'intern_set) mep-intern-set)
   (on-event (user-event 'intern_get) mep-intern-get)
   (on-event (button 1 any)
             (lambda (p e) (edit-plug-start p) (mep-mouse-position p e))
             (lambda (p e) (mep-mouse-select p e)))
   (on-event (button 2 any)
             (lambda (p e) (edit-plug-start p) (mep-mouse-yank p e)))
   (on-event (key "Return") edit-plug-start)))

(define mep-behavior2
  (make-behavior
   (on-event (user-event 'intern_done) (lambda (p e) (mep-deactivate p)))
   (on-event (user-event 'intern_cancel) (lambda (p e) (mep-undo p) (mep-deactivate p)))
   (on-event (user-event 'intern_set) mep-intern-set)
   (on-event (user-event 'intern_get) mep-intern-get)
   (on-event (button 1 any)
             (lambda (p e) (mep-mouse-position p e))
             (lambda (p e) (mep-mouse-select p e)))
   (on-event (button 2 any)
             (lambda (p e) (mep-mouse-yank p e)))
   (on-event (keypress "Return") edit-plug-done)
   (on-event (keypress "Escape") edit-plug-cancel)
   (on-event (keypress "c" control-mask) edit-plug-cancel)
   (on-event (keypress "a" control-mask) mep-bol)
   (on-event (keypress "Home") mep-bob)
   (on-event (keypress "e" control-mask) mep-eol)
   (on-event (keypress "End") mep-eob)
   (on-event (keypress "f" control-mask) mep-forward)
   (on-event (keypress "Right") mep-forward)
   (on-event (keypress "b" control-mask) mep-backward)
   (on-event (keypress "Left") mep-backward)
   (on-event (keypress "p" control-mask) mep-up)
   (on-event (keypress "Up") mep-up)
   (on-event (keypress "n" control-mask) mep-down)
   (on-event (keypress "Down") mep-down)
   (on-event (keypress "d" control-mask) mep-delete)
   (on-event (keypress "Delete") mep-delete)
   (on-event (keypress "BackSpace") mep-backspace)
   (on-event (keypress "j" control-mask) mep-newline)
   (on-event (keypress "o" control-mask) mep-open-line)
   (on-event (keypress "k" control-mask) mep-delete-rest)
   (on-event (keypress "w" control-mask) mep-clear)
   (on-event (keypress "u" control-mask) mep-undo)
   (on-event (keypress any any) mep-insert-char)))



;;-------------------------------------------------------------

(define (make-alt-edit-plug str alts . args)
  (let ((bg (or (get-keyword :value-background args #f)
                (get-keyword :background args #f)
                (make-color "white")))
        (abg (get-keyword :active-background args #f))
        (fg (or (get-keyword :foreground args #f) (make-color "black")))
        (afg (get-keyword :active-foreground args #f))
        (bw (get-keyword :borderwidth args 1))
        (bc (or (get-keyword :bordercolor args #f)
                (get-keyword :background args #f)
                (make-color "white")))
        (abc (or (get-keyword :active-bordercolor args #f)
                 (get-keyword :bordercolor args #f)
                 (get-keyword :foreground args #f)
                 (make-color "black")))
        (font (or (get-keyword :font args #f) (make-font "fixed")))
        (hmarg (get-keyword :horizontal-margin args 2))
        (vmarg (get-keyword :vertical-margin args 2))
        (asort (get-keyword :sort args #f))
        (amax (get-keyword :max args #f))
        (prop (get-keyword :property args '()))
        (beh (get-keyword :behavior args #f))
        (plug #f)
        (tmpbeh #f))
    (set! tmpbeh (if beh (make-behavior beh aep-behavior1) aep-behavior1))
    (set! plug (make-deco '()
                          :behavior tmpbeh
                          :property (append
                                     `((string) (pos . 0)
                                       (orig . #f) (active . #f)
                                       ,(cons 'alts alts) (amenu . #f)
                                       (asort . ,asort) (amax . ,amax)
                                       (ctx1 :background ,bg :foreground ,fg
                                             :borderwidth ,bw
                                             :bordercolor ,bc
                                             :horizontal-margin ,hmarg
                                             :vertical-margin ,vmarg
                                             :font ,font)
                                       (ctx2 :background ,(or abg bg)
                                             :foreground ,(or afg fg)
                                             :borderwidth ,bw
                                             :bordercolor ,abc
                                             :horizontal-margin ,hmarg
                                             :vertical-margin ,vmarg
                                             :font ,font)
                                       (beh1 . ,tmpbeh)
                                       (beh2 . ,(if beh
                                                    (make-behavior beh aep-behavior2)
                                                    aep-behavior2))
                                       (slant . ,(caddr (font-dimensions font)))
                                       (bc . ,bc) (abc . ,abc))
                                     prop)
                          :borderwidth bw
                          :bordercolor bc))
    (ep-set-string plug str 0)
    plug))

(define (aep-add-alt plug val)
  (let ((alts (get-property plug 'alts))
        (asort (get-property plug 'asort))
        (amax (get-property plug 'amax)))
    (if (member val alts)
        (begin
          (if (and (not asort)
                   (not (string=? val (car alts))))
              (set-property! plug 'amenu #f))
          (set! alts (cons val (delete val alts))))
        (begin
          (set-property! plug 'amenu #f)
          (set! alts (cons val alts))
          (if (and amax (> (length alts) amax))
              (set! alts (list-head alts amax)))))
    (set-property! plug 'alts alts)))

(define (aep-intern-set plug event)
  (let ((val (cdr (event-data event))))
    (if (string? val)
        (begin
          (ep-set-string plug val 0)
          (aep-add-alt plug val)))))

(define (aep-intern-get plug event)
  (let ((func (cdr (event-data event))))
    (func (ep-get-string plug))))

(define (aep-pop-menu deco)
  (let ((mn (get-property deco 'amenu)))
    (if (not mn)
        (let ((alts (get-property deco 'alts))
              (asort (get-property deco 'asort))
              (ctx1 (get-property deco 'ctx1))
              (ctx2 (get-property deco 'ctx2)))
          (set! mn (ap-menu-make (if asort
                                     (sort alts asort)
                                     alts)
                                 ctx1 ctx2))
          (set-property! deco 'amenu mn)))
    (let ((hgt (deco-height mn))
          (xpos (- (deco-x deco) (deco-borderwidth mn)))
          (ypos (+ (deco-y deco) (deco-height deco))))
      (if (< ypos 0)
          (set! ypos 0))
      (if (> (+ ypos hgt) (screen-height))
          (set! ypos (max 0 (- (screen-height) hgt))))
      (set-property! mn 'caller deco)
      (pop-menu mn (root-window) xpos ypos)
      )))

(define aep-behavior1
  (make-behavior
   (on-event (user-event 'intern_start) (lambda (p e) (ep-activate p)))
   (on-event (user-event 'intern_set) aep-intern-set)
   (on-event (user-event 'intern_get) aep-intern-get)
   (on-event (button 1 any)
             (lambda (p e) (edit-plug-start p) (ep-mouse-position p e))
             (lambda (p e) (ep-mouse-select p e)))
   (on-event (button 2 any)
             (lambda (p e) (edit-plug-start p) (ep-mouse-yank p e)))
   (on-event (button 3 any)
             (lambda (p e) (aep-pop-menu p)))
   (on-event (key "Return") edit-plug-start)))

(define aep-behavior2
  (make-behavior
   (on-event (user-event 'intern_done)
             (lambda (p e) (ep-deactivate p) (aep-add-alt p (ep-get-string p))))
   (on-event (user-event 'intern_cancel)
             (lambda (p e) (ep-undo p) (ep-deactivate p)))
   (on-event (user-event 'intern_set) aep-intern-set)
   (on-event (user-event 'intern_get) aep-intern-get)
   (on-event (button 1 any)
             (lambda (p e) (ep-mouse-position p e))
             (lambda (p e) (ep-mouse-select p e)))
   (on-event (button 2 any)
             (lambda (p e) (ep-mouse-yank p e)))
   (on-event (button 3 any)
             (lambda (p e) (aep-pop-menu p)))
   (on-event (keypress "Down")
             (lambda (p e) (aep-pop-menu p)))
   (on-event (keypress "Return") edit-plug-done)
   (on-event (keypress "Escape") edit-plug-cancel)
   (on-event (keypress "c" control-mask) edit-plug-cancel)
   (on-event (keypress "a" control-mask) ep-bol)
   (on-event (keypress "Home") ep-bol)
   (on-event (keypress "e" control-mask) ep-eol)
   (on-event (keypress "End") ep-eol)
   (on-event (keypress "f" control-mask) ep-forward)
   (on-event (keypress "Right") ep-forward)
   (on-event (keypress "b" control-mask) ep-backward)
   (on-event (keypress "Left") ep-backward)
   (on-event (keypress "d" control-mask) ep-delete)
   (on-event (keypress "Delete") ep-delete)
   (on-event (keypress "BackSpace") ep-backspace)
   (on-event (keypress "k" control-mask) ep-delete-rest)
   (on-event (keypress "w" control-mask) ep-clear)
   (on-event (keypress "u" control-mask) ep-undo)
   (on-event (keypress any any) ep-insert-char)))

;;-------------------------------------------------------------

(define (make-alt-plug val alts . args)
  (let ((bg (or (get-keyword :value-background args #f)
                (get-keyword :background args #f)
                (make-color "white")))
        (abg (get-keyword :active-background args #f))
        (fg (or (get-keyword :foreground args #f) (make-color "black")))
        (afg (get-keyword :active-foreground args #f))
        (bw (get-keyword :borderwidth args 1))
        (bc (or (get-keyword :bordercolor args #f)
                (get-keyword :background args #f)
                (make-color "white")))
        (abc (or (get-keyword :active-bordercolor args #f)
                 (get-keyword :bordercolor args #f)
                 (get-keyword :foreground args #f)
                 (make-color "black")))
        (font (or (get-keyword :font args #f) (make-font "fixed")))
        (hmarg (get-keyword :horizontal-margin args 2))
        (vmarg (get-keyword :vertical-margin args 2))
        (prop (get-keyword :property args '()))
        (beh (get-keyword :behavior args #f))
        (plug #f)
        (tmpbeh #f))
    (set! tmpbeh (if beh (make-behavior beh ap-behavior1) ap-behavior1))
    (set! plug (make-deco '()
                          :behavior tmpbeh
                          :property (append
                                     `((value . #f)
                                       (active . #f)
                                       ,(cons 'alts alts) (amenu . #f)
                                       (ctx1 :background ,bg :foreground ,fg
                                             :borderwidth ,bw
                                             :bordercolor ,bg  ;; Not bc
                                             :horizontal-margin ,hmarg
                                             :vertical-margin ,vmarg
                                             :font ,font)
                                       (ctx2 :background ,(or abg bg)
                                             :foreground ,(or afg fg)
                                             :borderwidth ,bw
                                             :bordercolor ,abc
                                             :horizontal-margin ,hmarg
                                             :vertical-margin ,vmarg
                                             :font ,font)
                                       (slant . ,(caddr (font-dimensions font)))
                                       (bc . ,bc) (abc . ,abc))
                                     prop)
                          :borderwidth bw
                          :bordercolor bc))
    (ap-set-value plug val)
    plug))

(define (ap-set-value plug val)
  (let* ((active (get-property plug 'active))
         (ctx (if active
                  (get-property plug 'ctx2)
                  (get-property plug 'ctx1)))
         (str (if (string? val) val (simple-format #f "~A" val)))
         (pix (make-label str :context ctx)))
    (set-deco-part! plug 1 pix)
    (set-property! plug 'value val)))

(define (ap-get-value plug)
  (get-property plug 'value))

(define (ap-activate plug)
  (let ((active (get-property plug 'active))
        (val (get-property plug 'value)))
    (if (not active)
        (begin
          (set-property! plug 'active #t)
          (set-deco-bordercolor! plug (get-property plug 'abc))
          (ap-set-value plug val)))))

(define (ap-deactivate plug)
  (let ((active (get-property plug 'active))
        (val (get-property plug 'value)))
    (if active
        (begin
          (set-property! plug 'active #f)
          (set-deco-bordercolor! plug (get-property plug 'bc))
          (ap-set-value plug val)))))

(define (ap-intern-set plug event)
  (let ((val (cdr (event-data event))))
    (ap-set-value plug val)))

(define (ap-intern-get plug event)
  (let ((func (cdr (event-data event))))
    (func (ap-get-value plug))))

(define (ap-pop-menu deco ev ind)
  (let ((mn (get-property deco 'amenu))
        (alts (get-property deco 'alts))
        (val (get-property deco 'value)))
    (if (not mn)
        (let ((ctx1 (get-property deco 'ctx1))
              (ctx2 (get-property deco 'ctx2)))
          (set! mn (ap-menu-make alts ctx1 ctx2))
          (set-property! deco 'amenu mn)))
    (let ((hgt (deco-height mn))
          (pos (list-index alts val))
          (xpos (- (deco-x deco) (deco-borderwidth mn)))
          (ypos (- (deco-y deco) (deco-borderwidth mn))))
      (set-property! mn 'poptime (if ev (event-time ev) #f))
      (if pos
          (set! ypos (- ypos (cadr (dimensions (deco-part mn (+ pos 1))))))
          (set! ypos (+ ypos (deco-height deco))))
      (if (< ypos 0)
          (set! ypos 0))
      (if (> (+ ypos hgt) (screen-height))
          (set! ypos (max 0 (- (screen-height) hgt))))
      (if (and ind (not (= ind 0)))
          (if pos
              (set! pos (remainder (+ pos ind (length alts)) (length alts)))
              (set! pos (remainder (+ ind (length alts)) (+ (length alts) 1)))))
      (if pos
          (let ((cur (deco-part mn (+ pos 1))))
            (set-property! mn 'current cur)))
      (set-property! mn 'caller deco)
      (pop-menu mn (root-window) xpos ypos)
      )))

(define ap-behavior1
  (make-behavior
   (on-event (user-event 'intern_start)
             (lambda (p e) (ap-activate p)))
   (on-event (user-event 'intern_done)
             (lambda (p e) (ap-deactivate p)))
   (on-event (user-event 'intern_cancel)
             (lambda (p e) (ap-deactivate p)))
   (on-event (user-event 'intern_set) ap-intern-set)
   (on-event (user-event 'intern_get) ap-intern-get)
   (on-event (button any any)
             (lambda (p e)
               (if (not (get-property p 'active))
                   (edit-plug-start p))
               (ap-pop-menu p e 0)))
   (on-event (key "Return")
             (lambda (p e)
               (if (not (get-property p 'active))
                   (edit-plug-start p))
               (ap-pop-menu p #f 0)))
   (on-event (key "Down")
             (lambda (p e)
               (if (not (get-property p 'active))
                   (edit-plug-start p))
               (ap-pop-menu p #f 1)))
   (on-event (key "Up")
             (lambda (p e)
               (if (not (get-property p 'active))
                   (edit-plug-start p))
               (ap-pop-menu p #f -1)))
   (on-event (key "Escape") edit-plug-cancel)))

(define (ap-menu-item-behavior)
  (make-behavior
   (on (buttonpress any any)
       #f)
   (on (buttonrelease any any)
       (let ((caller (get-property (top-deco deco) 'caller))
             (value (get-property deco 'value))
             (cur (get-property (deco-parent deco) 'current))
             (pt (get-property (top-deco deco) 'poptime)))
         (if (or (not pt) (> (- (event-time event) pt) double-click-delay))
             (begin
               (if cur
                   (begin
                     (send-user-event 'inactive cur)
                     (set-property! (deco-parent deco) 'current #f)))
               (unpop-menu (top-deco deco))
               (edit-plug-set caller value)
               (edit-plug-done caller)))))
   (on (user-event 'active)
       (set-deco-part! deco 1 (get-property deco 'alab))
       (set-deco-background! deco (get-property deco 'abg))
       (set-deco-bordercolor! deco (get-property deco 'abc)))
   (on (user-event 'inactive)
       (set-deco-part! deco 1 (get-property deco 'nlab))
       (set-deco-background! deco (get-property deco 'bg))
       (set-deco-bordercolor! deco (get-property deco 'bc)))
   (on (enter)
       (let ((cur (get-property (deco-parent deco) 'current)))
         (if cur
             (send-user-event 'inactive cur))
         (send-user-event 'active deco)
         (set-property! (deco-parent deco) 'current deco)))
   ))

(define (ap-menu-behavior)
  (make-behavior
   (on (buttonpress any any)
       #f)
   (on (buttonrelease any any) 
       (let ((cur (get-property deco 'current))
             (caller (get-property deco 'caller))
             (pt (get-property deco 'poptime)))
         (if (or (not pt) (> (- (event-time event) pt) double-click-delay))
             (begin
               (if cur
                   (begin
                     (send-user-event 'inactive cur)
                     (set-property! deco 'current #f)))
               (unpop-menu (top-deco deco))
               (edit-plug-cancel caller)))))
   (on (key "Escape")
       (let ((cur (get-property deco 'current))
             (caller (get-property deco 'caller)))
         (if cur
             (begin
               (send-user-event 'inactive cur)
               (set-property! deco 'current #f)))
         (unpop-menu (top-deco deco))
         (edit-plug-cancel caller)))
   (on (key "Return")
       (let ((cur (get-property deco 'current))
             (caller (get-property deco 'caller)))
         (if cur
             (begin
               (edit-plug-set (get-property deco 'caller)
                              (get-property cur 'value))
               (send-user-event 'inactive cur)
               (set-property! deco 'current #f)))
         (unpop-menu (top-deco deco))
         (edit-plug-done caller)))
   (on (key "Down")
       (let ((cur (get-property deco 'current))
             (parts (deco-parts deco)))
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
       (let ((cur (get-property deco 'current))
             (parts (deco-parts deco)))
         (if cur
             (let ((pos (- (list-index parts cur) 1)))
               (if (< pos 0)
                   (set! pos (- (length parts) 1)))
               (send-user-event 'inactive cur)
               (set! cur (list-ref parts pos)))
             (set! cur (car (reverse parts))))
         (send-user-event 'active cur)
         (set-property! deco 'current cur)))
   (on (leave) 
       (let ((cur (get-property deco 'current)))
         (if cur
             (begin
               (send-user-event 'inactive cur)
               (set-property! deco 'current #f)))))
   (on (opening)
       (let ((cur (get-property deco 'current)))
         (if cur
             (send-user-event 'active cur))))
   (on (closing)
       (let ((cur (get-property deco 'current)))
         (if cur
             (send-user-event 'inactive cur))))
   ))

(define (ap-menu-item-make value ctx1 ctx2)
  (let ((str (if (string? value) value (simple-format #f "~A" value)))
        (bg (get-keyword :background ctx1 #f))
        (abg (get-keyword :background ctx2 #f))
        (bc (get-keyword :bordercolor ctx1 #f))
        (abc (get-keyword :bordercolor ctx2 #f))
        (nlab #f)
        (alab #f))
    (set! nlab (make-label str :context ctx1))
    (set! alab (make-label str :context ctx2))
    (make-deco nlab '()
               :borderwidth (get-keyword :borderwidth ctx1 1)
               :bordercolor bc
               :background bg
               :behavior (ap-menu-item-behavior)
               :property `((value . ,value) (nlab . ,nlab) (alab . ,alab)
                           (bg . ,bg) (abg . ,abg) (bc . ,bc) (abc . ,abc)))))

(define (ap-menu-make vals ctx1 ctx2)
  (apply make-deco
         (append (map (lambda (x) (ap-menu-item-make x ctx1 ctx2)) vals)
                 (list :borderwidth 1
                       :borderpixel (get-keyword :bordercolor ctx2 #f)
                       :behavior (ap-menu-behavior)))))

;;-------------------------------------------------------------


(define (make-check-plug val . args)
  (let ((bg (or (get-keyword :background args #f) (make-color "white")))
        (vbg (get-keyword :value-background args #f))
        (abg (get-keyword :active-background args #f))
        (fg (or (get-keyword :foreground args #f) (make-color "black")))
        (bw (get-keyword :borderwidth args 1))
        (bc (or (get-keyword :bordercolor args #f)
                (get-keyword :background args #f)
                (make-color "white")))
        (abc (or (get-keyword :active-bordercolor args #f)
                 (get-keyword :bordercolor args #f)
                 (get-keyword :foreground args #f)
                 (make-color "black")))
        (size (or (get-keyword :size args #f) 17))
        (pixon1 (get-keyword :on-pixmap args #f))
        (pixoff1 (get-keyword :off-pixmap args #f))
        (pixon2 (get-keyword :active-on-pixmap args #f))
        (pixoff2 (get-keyword :active-off-pixmap args #f))
        (prop (get-keyword :property args '()))
        (beh (get-keyword :behavior args #f))
        (plug #f)
        (tmpbeh #f))
    (set! tmpbeh (if beh (make-behavior beh cp-behavior1) cp-behavior1))
    (if pixon1
        (if (not pixon2)
            (set! pixon2 pixon1))
        (begin
          (set! pixon1 (make-pixmap size size :background bg))
          (if vbg
              (draw-rectangle pixon1 3 3 (- size 6) (- size 6) :borderwidth 1 :foreground fg :background vbg)
              (draw-rectangle pixon1 3 3 (- size 6) (- size 6) :borderwidth 1 :foreground fg))
          (draw-line pixon1 4 4 (- size 5) (- size 5) :color fg)
          (draw-line pixon1 4 (- size 5) (- size 5) 4 :color fg)
          (if (not abg)
              (set! pixon2 pixon1)
              (begin
                (set! pixon2 (make-pixmap size size :background bg))
                (draw-rectangle pixon2 3 3 (- size 6) (- size 6) :borderwidth 1 :foreground fg :background abg)
                (draw-line pixon2 4 4 (- size 5) (- size 5) :color fg)
                (draw-line pixon2 4 (- size 5) (- size 5) 4 :color fg)))))
    (if pixoff1
        (if (not pixoff2)
            (set! pixoff2 pixoff1))
        (begin
          (set! pixoff1 (make-pixmap size size :background bg))
          (if vbg
              (draw-rectangle pixoff1 3 3 (- size 6) (- size 6) :borderwidth 1 :foreground fg :background vbg)
              (draw-rectangle pixoff1 3 3 (- size 6) (- size 6) :borderwidth 1 :foreground fg))
          (if (not abg)
              (set! pixoff2 pixoff1)
              (begin
                (set! pixoff2 (make-pixmap size size :background bg))
                (draw-rectangle pixoff2 3 3 (- size 6) (- size 6) :borderwidth 1 :foreground fg :background abg)))))
    (set! plug (make-deco (if val pixon1 pixoff1)
                          :behavior tmpbeh
                          :property (append
                                     `((state . ,(if val #t #f))
                                       (active . #f)
                                       (pixon1 . ,pixon1)
                                       (pixoff1 . ,pixoff1)
                                       (pixon2 . ,pixon2)
                                       (pixoff2 . ,pixoff2)
                                       (beh1 . ,tmpbeh)
                                       (beh2 . ,(if beh
                                                    (make-behavior beh cp-behavior2)
                                                    cp-behavior2))
                                       (bc . ,bc) (abc . ,abc))
                                     prop)
                          :borderwidth bw
                          :bordercolor bc))
    plug))

(define (cp-set-state plug val)
  (set-property! plug 'state val)
  (set-deco-part! plug 1 (get-property plug
                                       (if (get-property plug 'active)
                                           (if val 'pixon2 'pixoff2)
                                           (if val 'pixon1 'pixoff1)))))

(define (cp-get-state plug)
  (get-property plug 'state))

(define (cp-toggle-state plug . event)
  (cp-set-state plug (not (cp-get-state plug))))

(define (cp-set-state-on plug . event)
  (cp-set-state plug #t))

(define (cp-set-state-off plug . event)
  (cp-set-state plug #f))

(define (cp-activate plug)
  (if (not (get-property plug 'active))
      (let ((val (get-property plug 'state)))
        (set-deco-behavior! plug (get-property plug 'beh2))
        (set-property! plug 'active #t)
        (set-deco-bordercolor! plug (get-property plug 'abc))
        (set-deco-part! plug 1 (get-property plug
                                             (if val 'pixon2 'pixoff2))))))

(define (cp-deactivate plug)
  (if (get-property plug 'active)
      (let ((val (get-property plug 'state)))
        (set-deco-behavior! plug (get-property plug 'beh1))
        (set-property! plug 'active #f)
        (set-deco-bordercolor! plug (get-property plug 'bc))
        (set-deco-part! plug 1 (get-property plug
                                             (if val 'pixon1 'pixoff1))))))

(define (cp-intern-set plug event)
  (let ((val (cdr (event-data event))))
    (cp-set-state plug (if val #t #f))))

(define (cp-intern-get plug event)
  (let ((func (cdr (event-data event))))
    (func (cp-get-state plug))))

(define cp-behavior1
  (make-behavior
   (on-event (user-event 'intern_start) (lambda (p e) (cp-activate p)))
   (on-event (user-event 'intern_set) cp-intern-set)
   (on-event (user-event 'intern_get) cp-intern-get)
   (on-event (button 1 any)
             (lambda (p e) (edit-plug-start p e) (cp-toggle-state p e))
             (lambda (p e) (edit-plug-done p e)))))

(define cp-behavior2
  (make-behavior
   (on-event (user-event 'intern_done) (lambda (p e) (cp-deactivate p)))
   (on-event (user-event 'intern_cancel) (lambda (p e) (cp-deactivate p)))
   (on-event (user-event 'intern_set) cp-intern-set)
   (on-event (user-event 'intern_get) cp-intern-get)
   (on-event (button 1 any)
             (lambda (p e) (cp-toggle-state p e))
             (lambda (p e) (edit-plug-done p e)))
   (on-event (keypress "Return") 
             (lambda (p e) (cp-toggle-state p e) (edit-plug-done p e)))
   (on-event (keypress "Escape") edit-plug-done)
   (on-event (keypress "plus") cp-set-state-on)
   (on-event (keypress "minus") cp-set-state-off)
   (on-event (keypress "space") cp-toggle-state)))


;;-------------------------------------------------------------

(define (make-radio-plug id vlst . args)
  (let ((bg (or (get-keyword :background args #f) (make-color "white")))
        (vbg (get-keyword :value-background args #f))
        (abg (get-keyword :active-background args #f))
        (fg (or (get-keyword :foreground args #f) (make-color "black")))
        (bw (get-keyword :borderwidth args 1))
        (bc (or (get-keyword :bordercolor args #f)
                (get-keyword :background args #f)
                (make-color "white")))
        (abc (or (get-keyword :active-bordercolor args #f)
                 (get-keyword :bordercolor args #f)
                 (get-keyword :foreground args #f)
                 (make-color "black")))
        (size (or (get-keyword :size args #f) 17))
        (pixon1 (get-keyword :on-pixmap args #f))
        (pixoff1 (get-keyword :off-pixmap args #f))
        (pixon2 (get-keyword :active-on-pixmap args #f))
        (pixoff2 (get-keyword :active-off-pixmap args #f))
        (prop (get-keyword :property args '()))
        (beh (get-keyword :behavior args #f))
        (plug #f)
        (tmpbeh #f))
    (set! tmpbeh (if beh (make-behavior beh rp-behavior1) rp-behavior1))
    (if pixon1
        (if (not pixon2)
            (set! pixon2 pixon1))
        (begin
          (set! pixon1 (make-pixmap size size :background bg))
          (if vbg
              (draw-ellipse pixon1 3 3 (- size 6) (- size 6) :borderwidth 1 :foreground fg :background vbg)
              (draw-ellipse pixon1 3 3 (- size 6) (- size 6) :borderwidth 1 :foreground fg))
          (draw-ellipse pixon1 5 5 (- size 10) (- size 10) :background fg)
          (if (not abg)
              (set! pixon2 pixon1)
              (begin
                (set! pixon2 (make-pixmap size size :background bg))
                (draw-ellipse pixon2 3 3 (- size 6) (- size 6) :borderwidth 1 :foreground fg :background abg)
                (draw-ellipse pixon2 5 5 (- size 10) (- size 10) :background fg)))))
    (if pixoff1
        (if (not pixoff2)
            (set! pixoff2 pixoff1))
        (begin
          (set! pixoff1 (make-pixmap size size :background bg))
          (if vbg
              (draw-ellipse pixoff1 3 3 (- size 6) (- size 6) :borderwidth 1 :foreground fg :background vbg)
              (draw-ellipse pixoff1 3 3 (- size 6) (- size 6) :borderwidth 1 :foreground fg))
          (if (not abg)
              (set! pixoff2 pixoff1)
              (begin
                (set! pixoff2 (make-pixmap size size :background bg))
                (draw-ellipse pixoff2 3 3 (- size 6) (- size 6) :borderwidth 1 :foreground fg :background abg)))))
    (set! plug (make-deco (if (eq? id (car vlst)) pixon1 pixoff1)
                          :behavior tmpbeh
                          :property (append
                                     `((identity . ,id)
                                       (state . ,vlst)
                                       (active . #f)
                                       (pixon1 . ,pixon1)
                                       (pixoff1 . ,pixoff1)
                                       (pixon2 . ,pixon2)
                                       (pixoff2 . ,pixoff2)
                                       (beh1 . ,tmpbeh)
                                       (beh2 . ,(if beh
                                                    (make-behavior beh rp-behavior2)
                                                    rp-behavior2))
                                       (bc . ,bc) (abc . ,abc))
                                     prop)
                          :borderwidth bw
                          :bordercolor bc))
    (set-cdr! (list-tail vlst (- (length vlst) 1)) (list plug))
    plug))

(define (rp-update-state plug)
  (set-deco-part! plug 1 
                  (get-property plug 
                                (if (eq? (get-property plug 'identity)
                                         (car (get-property plug 'state)))
                                    (if (get-property plug 'active) 'pixon2 'pixon1)
                                    (if (get-property plug 'active) 'pixoff2 'pixoff1)))))

(define (rp-set-state plug val)
  (if (eq? val (get-property plug 'identity))
      (begin
        (set-car! (get-property plug 'state) val)
        (for-each rp-update-state (cdr (get-property plug 'state))))))

(define (rp-get-state plug)
  (let ((id (get-property plug 'identity)))
    (if (eq? id (car (get-property plug 'state)))
        id #f)))

(define (rp-set-state-on plug . event)
  (rp-set-state plug (get-property plug 'identity)))

(define (rp-next-state-on plug . event)
  (let* ((vlst (get-property plug 'state))
         (ind (list-index vlst plug)))
    (if ind
        (rp-set-state-on (list-ref vlst 
                                   (if (= ind (- (length vlst) 1))
                                       1
                                       (+ ind 1)))))))

(define (rp-prev-state-on plug . event)
  (let* ((vlst (get-property plug 'state))
         (ind (list-index vlst plug)))
    (if ind
        (rp-set-state-on (list-ref vlst 
                                   (if (= ind 1)
                                       (- (length vlst) 1)
                                       (- ind 1)))))))

(define (rp-activate plug)
  (if (not (get-property plug 'active))
      (begin
        (set-deco-behavior! plug (get-property plug 'beh2))
        (set-property! plug 'active #t)
        (set-deco-bordercolor! plug (get-property plug 'abc))
        (rp-update-state plug))))

(define (rp-deactivate plug)
  (if (get-property plug 'active)
      (begin
        (set-deco-behavior! plug (get-property plug 'beh1))
        (set-property! plug 'active #f)
        (set-deco-bordercolor! plug (get-property plug 'bc))
        (rp-update-state plug))))

(define (rp-intern-set plug event)
  (let ((val (cdr (event-data event))))
    (rp-set-state plug val)))

(define (rp-intern-get plug event)
  (let ((func (cdr (event-data event)))
        (val (rp-get-state plug)))
    (if val
        (func val))))

(define rp-behavior1
  (make-behavior
   (on-event (user-event 'intern_start) (lambda (p e) (rp-activate p)))
   (on-event (user-event 'intern_set) rp-intern-set)
   (on-event (user-event 'intern_get) rp-intern-get)
   (on-event (button 1 any)
             (lambda (p e) (edit-plug-start p e) (rp-set-state-on p e))
             (lambda (p e) (edit-plug-done p e)))))

(define rp-behavior2
  (make-behavior
   (on-event (user-event 'intern_done) (lambda (p e) (rp-deactivate p)))
   (on-event (user-event 'intern_cancel) (lambda (p e) (rp-deactivate p)))
   (on-event (user-event 'intern_set) rp-intern-set)
   (on-event (user-event 'intern_get) rp-intern-get)
   (on-event (button 1 any)
             (lambda (p e) (rp-set-state-on p e))
             (lambda (p e) (edit-plug-done p e)))
   (on-event (keypress "Return") 
             (lambda (p e) (rp-set-state-on p e) (edit-plug-done p e)))
   (on-event (keypress "Escape") edit-plug-done)
   (on-event (keypress "plus") rp-next-state-on)
   (on-event (keypress "minus") rp-prev-state-on)
   (on-event (keypress "space") rp-set-state-on)))

;;-------------------------------------------------------------

(define (button-plug-behavior action p1 p2)
  (make-behavior
   (on-event (user-event 'active)
             (lambda (p e)
               (set-deco-part! p 1 p2)))
   (on-event (user-event 'inactive)
             (lambda (p e)
               (set-deco-part! p 1 p1)))
   (on-event (key "Return")
             (lambda (p e)
               (send-user-event 'active p))
             (lambda (p e)
               (send-user-event 'inactive p)
               (action p e)))
   (on-event (button any any)
             (lambda (p e)
               (send-user-event 'active p))
             (lambda (p e)
               (send-user-event 'inactive p)
               (action p e)))))

(define (make-button-plug label action ctx)
  (let* ((fg (or (get-keyword :foreground ctx #f) (make-color "black")))
         (bc (get-keyword :bordercolor ctx fg))
         (abc (get-keyword :active-bordercolor ctx bc))
         (obg (or (get-keyword :background ctx #f) (make-color "white")))
         (ibg (get-keyword :button-background ctx obg))
         (abg (get-keyword :active-button-background ctx ibg))
         (pix1 (make-label label
                           :horizontal-margin 3 :vertical-margin 5
                           :background ibg :context ctx))
         (pix2 (make-label label
                           :horizontal-margin 3 :vertical-margin 5
                           :background abg :context ctx))
         (dim (dimensions pix1))
         (w (caddr dim))
         (h (cadddr dim)))
    (draw-rectangle pix1 2 2 (- w 4) (- h 4) 
                    :borderwidth 2 :foreground bc)
    (draw-polygon pix1 0 0 0 2 2 0 :color obg)
    (draw-polygon pix1 w 0 (- w 2) 0 w 2 :color obg)
    (draw-polygon pix1 0 h 3 h 0 (- h 3) :color obg)
    (draw-polygon pix1 w h (- w 3) h w (- h 3) :color obg)
    (draw-line pix1 2 2 2 2 :color bc)
    (draw-line pix1 (- w 3) 2 (- w 3) 2 :color bc)
    (draw-line pix1 2 (- h 3) 2 (- h 3) :color bc)
    (draw-line pix1 (- w 3) (- h 3)
               (- w 3) (- h 3) :color bc)
    (draw-rectangle pix2 2 2 (- w 4) (- h 4) 
                    :borderwidth 2 :foreground abc)
    (draw-polygon pix2 0 0 0 2 2 0 :color obg)
    (draw-polygon pix2 w 0 (- w 2) 0 w 2 :color obg)
    (draw-polygon pix2 0 h 3 h 0 (- h 3) :color obg)
    (draw-polygon pix2 w h (- w 3) h w (- h 3) :color obg)
    (draw-line pix2 2 2 2 2 :color abc)
    (draw-line pix2 (- w 3) 2 (- w 3) 2 :color abc)
    (draw-line pix2 2 (- h 3) 2 (- h 3) :color abc)
    (draw-line pix2 (- w 3) (- h 3)
               (- w 3) (- h 3) :color abc)
    (make-deco pix1 :behavior (button-plug-behavior action pix1 pix2))))


;;-------------------------------------------------------------

(define edit-plug #t)
