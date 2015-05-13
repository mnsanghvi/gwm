;; custom-menu.scm --- User-friendlier package customizations
;;
;; Author: Anders Holst  (aho@sics.se)  
;; Copyright (C) 2002  Anders Holst
;;
;; ---------------------------------------------------------------------
;;
;; This file implements a way for other packages to provide easy
;; customization via menus, without spending too much effort on it
;; themselves.
;;
;;
;; The main function is '(custom-menu <MENU-DESCRIPTION>)'.
;;
;; In the simplest case its argument is a list of strings and variables,
;; and it constructs a menu where the user can edit the values of the
;; given variables. Typically the first element in the list should be
;; the name of the menu, but any strings in the list show up as labels.
;;
;; The list can also contain other "menu descriptions" as elements.
;; They will turn up as buttons leading to these "sub-menus". Note that
;; then the first element in each sub-menu description *must* be
;; the name of it (and not a variable or a sub-description).
;;
;; Actually, the first element in each menu-description can also be a
;; list where the first element is the name of it and the rest an
;; association list. Currently the only supported field in this
;; association list is 'hook', which should be some code to run whenever
;; the value of a variable is changed in the menu.
;;
;; To start edit a value in the menu, you click in the corresponding
;; field. If the variable 'custom-menu-immediate-change' is non-nil, the
;; variable is assigned to the new value as soon you finish editing.
;; There is also a special construct, to allow more complex values. If
;; the field starts with a comma (,) the following expression is
;; evaluated before the assignment.
;; 
;; But that's all there is to it really.
;;
;; The rest of the code in this file is to provide a simple way
;; for packages to install their customizable variables in a global
;; custom-menu hierarchy, and to maintain the user's variable settings
;; between GWM sessions.
;;
;;
;; There are two functions intended to be used by the package developer:
;;   (custom-menu-install-symbols <PACKAGE-NAME> <SYMBOL-LIST>)
;;   (custom-menu-install-hook <PACKAGE-NAME> <HOOK-CODE>)
;;
;; The <PACKAGE-NAME> is either a string, or a list of strings serving
;; as a "path" through the menu hierarchy. <SYMBOL-LIST> is the list of
;; all customizable variables in the package. <HOOK-CODE> is a piece of
;; code (quoted) to run whenever any of the customizable variables
;; have changed. It is of course not necessary to provide such a hook,
;; but it is very nice to have one.
;;
;; Another thing to consider for the package developer is to make the
;; variables suitable for customization in this way. Colors and fonts
;; should *not* be stored as numbers but as the strings used when
;; creating them. The contents of variables should preferably not be
;; very long lists, since these are hard to edit. If values are cached
;; there should be some way of updating the cache when the values have
;; changed, and if values are not cached the package should not assume
;; that the contents of variables will remain the same during the whole
;; session.
;;
;;
;; There are three functions intended for the GWM user:
;;   (custom-menu)
;;   (custom-menu-load-preferences)
;;   (custom-menu-save-preferences)
;;
;; '(custom-menu)' brings you into the top of the custom-menu
;; hierarchy. It is most suitably called from the root menu.
;;
;; The function '(custom-menu-load-preferences)' can be called either when
;; loading the profile, or from the 'screen-opening' hook and 
;; '(custom-menu-save-preferences)' can be called from the 'screen-closing'
;; hook. They try to maintain a file ".gwmcustom.scm" where the
;; variables the user have changed through the custom menus are saved.
;;

(require 'panel-window)

(defvar custom-menu-font "6x13bold" "Font of headers and buttons" 'font)
(defvar custom-menu-symbol-font "6x13" "Font of symbol names" 'font)
(defvar custom-menu-value-font "6x13" "Font of symbol values" 'font)
(defvar custom-menu-docum-font "6x10" "Font of symbol descriptions" 'font)
(defvar custom-menu-foreground "black" "Text color" 'color)
(defvar custom-menu-background "lightgreen" "Background color" 'color)
(defvar custom-menu-button-background "white" "Button color" 'color)
(defvar custom-menu-value-background "white" "Edit field color" 'color)
(defvar custom-menu-value-frame #t "Whether to use a frame around the value" 'boolean)
(defvar custom-menu-immediate-change #t "Whether update should be done immediately" 'boolean)


(define custom-menu-global-preferences '())
(define custom-menu-preferences-changed #f)
(define custom-menu-global-description '((#f) "Customizations"))
(define custom-menu-preference-file #f)

(let ((tmp (getenv "GWM_CUSTOM_FILE")))
  (cond ((not tmp)
         (set! custom-menu-preference-file "./.gwmcustom.scm"))
        ((eq? (string-ref tmp 0) #\/)
         (set! custom-menu-preference-file tmp))
        (#t
         (set! custom-menu-preference-file (string-append "./" tmp)))))

(require 'edit-plug)

(define (custom-menu-global-context)
  (list :font (make-font custom-menu-font)
        :symbol-font (make-font custom-menu-symbol-font)
        :value-font (make-font custom-menu-value-font)
        :doc-font (make-font custom-menu-docum-font)
        :foreground (make-color custom-menu-foreground)
        :background (make-color custom-menu-background)
        :value-background (make-color custom-menu-value-background)
        :button-background (make-color custom-menu-button-background)
        :value-frame custom-menu-value-frame
        :immediate-change custom-menu-immediate-change))

(define (custom-menu-print-proc proc)
  (let ((src (procedure-source proc))
        (name (procedure-name proc)))
    (if name
        (simple-format #f "#<procedure ~A>" name)
        (apply string-append
               (append '("#<procedure")
                       (map (lambda (x) (simple-format #f " ~S" x)) (cdr src))
                       '(">"))))))

(define (custom-menu-print-hook hook)
  (let ((hstr (simple-format #f "~A" hook))
        (lst (hook->list hook)))
    (apply string-append
           (append (list (regexp-substitute #f (string-match "#<hook [0-9]* " hstr) 0))
                   (map custom-menu-print-proc lst)
                   '(")")))))

(define (custom-menu-print-list lst)
  (let ((res "("))
    (set! res (string-append res (custom-menu-print-to-string (car lst))))
    (while (pair? (cdr lst))
      (set! lst (cdr lst))
      (set! res (string-append res " " (custom-menu-print-to-string (car lst)))))
    (if (null? (cdr lst))
        (set! res (string-append res ")"))
        (set! res (string-append res " . " (custom-menu-print-to-string (cdr lst)) ")")))
    res))

(define (custom-menu-print-to-string val)
  (cond ((pair? val)
         (custom-menu-print-list val))
        ((procedure? val)
         (custom-menu-print-proc val))
        ((hook? val)
         (custom-menu-print-hook val))
        (#t
         (simple-format #f "~S" val))))

(define (custom-menu-check-specials val)
  (if (pair? val)
      (or (custom-menu-check-specials (car val))
          (custom-menu-check-specials (cdr val)))
      (or (procedure? val) (hook? val))))        

(define (custom-menu-transform-specials str ev)
  (let ((mh (string-match "#<hook ([^<>]*(#<[^<>]*>[^<>]*)*)>" str)))
    (while mh
      (set! str (regexp-substitute #f mh
                                   'pre (if ev "(custom-menu-make-hook " ",(custom-menu-make-hook ")
                                   (custom-menu-transform-specials (match:substring mh 1) #t)
                                   ")" 'post))
      (set! mh (string-match "#<hook ([^<>]*(#<[^<>]*>[^<>]*)*)>" str (match:end mh 0))))
    (set! str (regexp-substitute/global #f "#<procedure ([^<>() ]*)>" str 
                                        'pre (if ev "" ",") 1 " " 'post))
    (set! str (regexp-substitute/global #f "#<procedure ([^<>]*)>" str 
                                        'pre (if ev "(lambda " ",(lambda ") 1 ")" 'post))
    (if ev str (string-append "`" str))))

(define (custom-menu-make-hook n . lst)
  (let ((res (make-hook n)))
    (map (lambda (x) (add-hook! res x)) lst)
    res))

(define (custom-menu-read-from-string str)
  (let ((meval (string-match "^ *," str))
        (mspec (string-match "#<procedure|#<hook" str))
        (do-eval #f)
        (ret #f))
    (if meval
        (begin
         (set! str (substring str (cdr (array-ref meval 1))))
         (set! do-eval #t)))
    (if mspec
        (begin
         (set! str (custom-menu-transform-specials str do-eval))
         (set! do-eval #t)))
    (set! ret
          (catch #t 
            (if do-eval
                (lambda ()
                  (list (eval-string str)))
                (lambda ()
                  (list (with-input-from-string str (lambda () (read))))))
            (lambda args #f)))
    (if (or (not ret) (eof-object? (car ret)) (unspecified? (car ret)))
        #f
        ret)))

(define (custom-menu-set-plug-value deco val)
  (let ((type (get-property deco 'custom-type)))
    (cond ((eq? type 'boolean) 
           (edit-plug-set deco (if (list? val) (car val) #f)))
          ((eq? type 'list)
           (edit-plug-set deco (if (and (list? val) (pair? (car val)))
                                   (map custom-menu-print-to-string (car val))
                                   '(""))))
          ((list? type)
           (edit-plug-set deco (if (list? val) (car val) #f)))
          (#t
           (edit-plug-set deco
                          (cond ((list? val)
                                 (custom-menu-print-to-string (car val)))
                                ((string? val)
                                 val)
                                (#t "")))))))

(define (custom-menu-get-plug-value deco)
  (let ((type (get-property deco 'custom-type))
        (val (edit-plug-get deco)))
    (cond ((eq? type 'boolean)
           (list (cp-get-state deco)))
          ((eq? type 'list)
           (list (apply append (delete #f (map custom-menu-read-from-string val)))))
          ((list? type)
           (list val))
          (#t
           (if (string-match "^ *," val)
               val
               (custom-menu-read-from-string val))))))

(if (not (defined? 'primitive-eval))
    (define primitive-eval eval))

(define (custom-menu-set-var sym val)
  (if (not (defined? sym))
      (primitive-eval `(begin (define ,sym #f) (set! ,sym (quote ,val))))
      (primitive-eval `(set! ,sym (quote ,val)))))

;; Things to do when starting editing.
(define (custom-menu-start-edit deco event)
  (custom-menu-set-focus deco))

;; Things to do when editing is done.
(define (custom-menu-end-edit deco event)
  (let* ((sym (get-property deco 'descr))
         (str (custom-menu-get-plug-value deco))
         (res (if (string? str) (custom-menu-read-from-string str) str))
         (immch (get-property (inner-deco deco) 'immch))
         (hook (get-property (inner-deco deco) 'hook)))
    (if res
        (if (not (equal? (cadr sym) str))
            (begin
              (set-car! (cdr sym) str)
              (if (not (string? str))
                  (custom-menu-set-plug-value deco res))
              (if immch
                  (begin
                    (custom-menu-set-var (car sym) (car res))
                    (set-car! (cdddr sym) #f)
                    (if hook (run-hook hook)))
                  (set-car! (cdddr sym) #t))))
        (custom-menu-set-plug-value deco (cadr sym)))
    (custom-menu-unset-focus deco)))

(define (custom-menu-abort-edit deco event)
  (custom-menu-unset-focus deco))

(define (custom-menu-header txt ctx)
  (make-deco '() (make-label txt :vertical-margin 5 :context ctx) '() :context ctx))

(define (custom-menu-row sym symlen ctx)
  (let* ((val (if (defined? sym)
                  (or (custom-menu-get-preference sym)
                      (list (primitive-eval sym)))
                  #f))
         (descr (list sym val val #f))
         (beh (make-behavior (on (user-event 'start)
                                 (custom-menu-start-edit deco event))
                             (on (user-event 'done)
                                 (custom-menu-end-edit deco event))
                             (on (user-event 'cancel)
                                 (custom-menu-abort-edit deco event))
                             (on (keypress (car (keysym->keycode "Tab")) alone)
                                 (custom-menu-move-focus-forward deco))
                             (on (keypress (car (keysym->keycode "Tab")) shift-mask)
                                 (custom-menu-move-focus-backward deco))))
         (type (symbol-property sym 'type))
         (ep (cond ((eq? type 'boolean)
                    (make-check-plug (if (list? val) (car val) #f)
                                     :behavior beh
                                     :property (list (cons 'descr descr)
                                                     (cons 'custom-type type))
                                     :background (get-keyword :value-background ctx #f)
                                     :context ctx))
                   ((eq? type 'list)
                    (if (string? val)
                        (set! val (custom-menu-read-from-string val)))
                    (make-multi-edit-plug (if (and (list? val) (pair? (car val)))
                                              (map custom-menu-print-to-string (car val))
                                              '(""))
                                          :font (get-keyword :value-font ctx #f)
                                          :behavior beh
                                          :property (list (cons 'descr descr)
                                                          (cons 'custom-type type))
                                          :background (get-keyword :value-background ctx #f)
                                          :context ctx))
                   ((list? type)
                    (make-alt-plug (if (list? val) (car val) "") type 
                                   :behavior beh
                                   :property (list (cons 'descr descr)
                                                   (cons 'custom-type type))
                                   :font (get-keyword :value-font ctx #f)
                                   :background (get-keyword :value-background ctx #f)
                                   :context ctx))
                   (#t
                    (make-edit-plug (if val 
                                        (if (string? val)
                                            val
                                            (custom-menu-print-to-string (car val)))
                                        "")
                                    :font (get-keyword :value-font ctx #f)
                                    :behavior beh
                                    :property (list (cons 'descr descr)
                                                    (cons 'custom-type type))
                                    :background (get-keyword :value-background ctx #f)
                                    :context ctx)))))
    (make-deco (make-deco (make-deco (make-label (symbol->string sym)
                                                 :font (get-keyword :symbol-font ctx #f)
                                                 :context ctx)
                                     '()
                                     :direction 'horizontal
                                     :width symlen
                                     :context ctx)
                          (if (get-keyword :value-frame ctx #f)
                              (make-deco ep
                                         :borderwidth 1
                                         :context ctx)
                              ep)
                          '()
                          :direction 'horizontal
                          :margin 8
                          :separator 8
                          :context ctx)
               (let ((doc (symbol-property sym 'doc)))
                 (if doc
                     (make-deco (make-deco :height 0 :width 30)
                                (make-label doc
                                            :font (get-keyword :doc-font ctx #f)
                                            :context ctx)
                                '()
                                :context ctx)
                     '()))
               :direction 'vertical
               :property (list (cons 'descr descr)
                               (cons 'ep ep))
               :context ctx)))

(define (custom-menu-symbol-max-length lst ctx)
  (let ((font (get-keyword :symbol-font ctx #f)))
    (apply max (map (lambda (e)
                      (if (symbol? e)
                          (car (string-dimensions (symbol->string e) font))
                          0))
                    lst))))

(define (custom-menu-submenu-row descr name ctx nctx)
  (make-deco (make-button-plug (caar descr)
                               (lambda (d e)
                                 (custom-menu-pop descr
                                                  ctx
                                                  (append name (list (caar descr)))
                                                  (+ 15 (deco-x (inner-deco d)))
                                                  (+ 15 (deco-y (inner-deco d)))))
                               nctx)
             '()
             :direction 'horizontal :margin 30 :context nctx))

(define (custom-menu-button-row ctx)
  (make-deco (make-button-plug "Done" (lambda (d e) (custom-menu-done (inner-deco d))) ctx)
             (make-button-plug "Apply" (lambda (d e) (custom-menu-apply (inner-deco d))) ctx)
             (make-button-plug "Undo" (lambda (d e) (custom-menu-undo (inner-deco d))) ctx)
             (make-button-plug "Cancel" (lambda (d e) (custom-menu-cancel (inner-deco d))) ctx)
             '()
             :direction 'horizontal :margin 30 :separator 20 :context ctx))

(define (custom-menu-make descr ctx name)
  (let* ((lctx (assq 'local-ctx (cdar descr)))
         (nctx (if lctx (append (cdr lctx) ctx) ctx))
         (hook (assq 'hook (cdar descr)))
         (immch (get-keyword :immediate-change nctx #f))
         (len (custom-menu-symbol-max-length (cdr descr) nctx))
         (dlst (delq! #f (map (lambda (e) 
                                (cond ((string? e)
                                       (custom-menu-header e nctx))
                                      ((symbol? e)
                                       (custom-menu-row e len nctx))
                                      ((pair? e)
                                       (custom-menu-submenu-row e name ctx nctx))
                                      (#t #f)))
                              (cdr descr)))))
    (apply make-deco (append dlst
                             (list (custom-menu-button-row ctx)
                                   :behavior (make-behavior
                                              (on (focus-in)
                                                  (custom-menu-refresh-focus deco))
                                              (on (keypress (car (keysym->keycode "Tab")) alone)
                                                  (custom-menu-move-focus-forward deco))
                                              (on (keypress (car (keysym->keycode "Tab")) shift-mask)
                                                  (custom-menu-move-focus-backward deco)))
                                   :property `((descr . ,descr)
                                               (custom-menu-path . ,name)
                                               (focus . #f)
                                               (immch . ,immch)
                                               (hook . ,(if hook (cdr hook) #f)))
                                   :separator 10
                                   :margin 8
                                   :context ctx)))))

(define (custom-menu-set-focus plug)
  (let ((focus (get-property (inner-deco plug) 'focus)))
    (if (and focus (not (eq? focus plug)))
        (edit-plug-done focus))
    (set-property! (inner-deco plug) 'focus plug))
    (set-focus! plug))

(define (custom-menu-unset-focus plug)
  (let ((focus (get-property (inner-deco plug) 'focus)))
    (if (and focus (eq? focus plug))
        (begin
          (set-property! (inner-deco plug) 'focus #f)
          (set-focus! (inner-deco plug))))))

(define (custom-menu-move-focus-forward plug)
  (let* ((top (inner-deco plug))
         (focus (get-property top 'focus))
         (len (deco-num-parts top))
         (ind 1))
    (if focus
        (begin
          (while (and (<= ind len)
                      (not (eq? focus (get-property (deco-part top ind) 'ep))))
            (set! ind (+ ind 1)))
          (set! ind (+ ind 1))
          (if (> ind len)
              (set! ind 1))))
    (while (and (<= ind len)
                (not (get-property (deco-part top ind) 'ep)))
      (set! ind (+ ind 1)))
    (if (> ind len)
        (begin
          (set! ind 1)
          (while (and (<= ind len)
                      (not (get-property (deco-part top ind) 'ep)))
            (set! ind (+ ind 1)))))
    (if (<= ind len)
        (begin
          (if focus
              (edit-plug-done focus))
          (edit-plug-start (get-property (deco-part top ind) 'ep))))))

(define (custom-menu-move-focus-backward plug)
  (let* ((top (inner-deco plug))
         (focus (get-property top 'focus))
         (len (deco-num-parts top))
         (ind len))
    (if focus
        (begin
          (while (and (> ind 0)
                      (not (eq? focus (get-property (deco-part top ind) 'ep))))
            (set! ind (- ind 1)))
          (set! ind (- ind 1))
          (if (<= ind 0)
              (set! ind len))))
    (while (and (> ind 0)
                (not (get-property (deco-part top ind) 'ep)))
      (set! ind (- ind 1)))
    (if (= ind 0)
        (begin
          (set! ind len)
          (while (and (> ind 0)
                      (not (get-property (deco-part top ind) 'ep)))
            (set! ind (- ind 1)))))
    (if (> ind 0)
        (begin
          (if focus
              (edit-plug-done focus))
          (edit-plug-start (get-property (deco-part top ind) 'ep))))))

(define (custom-menu-refresh-focus plug)
  (let ((focus (get-property (inner-deco plug) 'focus)))
    (if focus
        (set-focus! focus))))

(define (custom-menu-done top)
  (let ((ind 1)
        (len (deco-num-parts top))
        (change #f)
        (hook (get-property top 'hook))
        (focus (get-property top 'focus))
        (descr #f)
        (res #f))
    (if focus
        (edit-plug-done focus))
    (while (<= ind len)
      (set! descr (get-property (deco-part top ind) 'descr))
      (if descr
          (begin
            (if (cadddr descr)
                (begin
                  (set! res (if (string? (cadr descr))
                                (custom-menu-read-from-string (cadr descr))
                                (cadr descr)))
                  (if res 
                      (begin
                        (custom-menu-set-var (car descr) (car res))
                        (set-car! (cdddr descr) #f)
                        (set! change #t)))))
            (if (not (eq? (cadr descr) (caddr descr)))
                (custom-menu-put-preference (car descr) (cadr descr)))))
      (set! ind (+ ind 1)))
    (kill-window top)
    (if change
        (if hook (run-hook hook)))))

(define (custom-menu-apply top)
  (let ((ind 1)
        (len (deco-num-parts top))
        (change #f)
        (hook (get-property top 'hook))
        (focus (get-property top 'focus))
        (descr #f)
        (res #f))
    (if focus
        (edit-plug-done focus))
    (while (<= ind len)
      (set! descr (get-property (deco-part top ind) 'descr))
      (if descr
          (if (cadddr descr)
              (begin
                (set! res (if (string? (cadr descr))
                              (custom-menu-read-from-string (cadr descr))
                              (cadr descr)))
                (if res 
                    (begin
                      (custom-menu-set-var (car descr) (car res))
                      (set-car! (cdddr descr) #f)
                      (set! change #t))))))
      (set! ind (+ ind 1)))
    (if change
        (if hook (run-hook hook)))))

(define (custom-menu-undo top)
  (let ((ind 1)
        (len (deco-num-parts top))
        (change #f)
        (hook (get-property top 'hook))
        (focus (get-property top 'focus))
        (descr #f)
        (res #f))
    (if focus
        (edit-plug-done focus))
    (while (<= ind len)
      (set! descr (get-property (deco-part top ind) 'descr))
      (if descr
          (if (not (eq? (cadr descr) (caddr descr)))
              (begin
                (if (cadddr descr)
                    (set-car! (cdddr descr) #f)
                    (begin
                      (set! res (if (string? (caddr descr))
                                    (custom-menu-read-from-string (caddr descr))
                                    (caddr descr)))
                      (if res 
                          (begin
                            (custom-menu-set-var (car descr) (car res))
                            (set! change #t)))))
                (set-car! (cdr descr) (caddr descr))
                (custom-menu-set-plug-value (get-property (deco-part top ind) 'ep)
                                            (cadr descr)))))
      (set! ind (+ ind 1)))
    (if change
        (if hook (run-hook hook)))))

(define (custom-menu-cancel top)
  (let ((ind 1)
        (len (deco-num-parts top))
        (change #f)
        (hook (get-property top 'hook))
        (focus (get-property top 'focus))
        (descr #f)
        (res #f))
    (if focus
        (edit-plug-done focus))
    (while (<= ind len)
      (set! descr (get-property (deco-part top ind) 'descr))
      (if descr
          (if (not (eq? (cadr descr) (caddr descr)))
              (if (cadddr descr)
                  (set-car! (cdddr descr) #f)
                  (begin
                    (set! res (if (string? (caddr descr))
                                  (custom-menu-read-from-string (caddr descr))
                                  (caddr descr)))
                    (if res 
                        (begin
                          (custom-menu-set-var (car descr) (car res))
                          (set! change #t)))))))
      (set! ind (+ ind 1)))
    (kill-window top)
    (if change
        (if hook (run-hook hook)))))

(define (custom-menu-put-preference sym val)
  (let ((ele (assq sym custom-menu-global-preferences)))
    (if ele
        (set-cdr! ele val)
        (set! custom-menu-global-preferences
              (cons (cons sym val)
                    custom-menu-global-preferences)))
    (set! custom-menu-preferences-changed #t)))

(define (custom-menu-get-preference sym)
  (let ((ele (assq sym custom-menu-global-preferences)))
    (if ele
        (cdr ele)
        #f)))

(define (custom-menu-apply-preferences prefs)
  (let ((res #f))
    (for-each (lambda (ele)
                (set! res (if (string? (cdr ele))
                              (custom-menu-read-from-string (cdr ele))
                              (cdr ele)))
                (if res 
                    (custom-menu-set-var (car ele) (car res))))
              prefs)))

(define (custom-menu-save-preferences)
  (if custom-menu-preferences-changed
      (begin
        (if (file-exists? custom-menu-preference-file)
            (rename-file custom-menu-preference-file
                         (string-append custom-menu-preference-file "~")))
        (let ((f (open-file custom-menu-preference-file "w")))
          (display "(\n" f)
          (for-each (lambda (ele)
                      (if (and (pair? (cdr ele))
                               (custom-menu-check-specials (cadr ele)))
                          (write (cons (car ele) (custom-menu-print-to-string (cadr ele))) f)
                          (write ele f))
                      (newline f))
                    custom-menu-global-preferences)
          (display ")\n" f)
          (close-port f))
        (set! custom-menu-preferences-changed #f))))

(define (custom-menu-load-preferences)
  (if (file-exists? custom-menu-preference-file)
      (let ((f (open-file custom-menu-preference-file "r")))
        (set! custom-menu-global-preferences (read f))
        (custom-menu-apply-preferences custom-menu-global-preferences)
        (set! custom-menu-preferences-changed #f)
        (close-port f))))

(define (custom-menu-insert-symbols descr syms)
  (let ((l2 descr)
        (l1 (cdr descr)))
    (while (and (not (null? l1)) (not (pair? (car l1))))
      (if (memq (car l1) syms)
          (set! syms (delq! (car l1) syms)))
      (set! l2 l1)
      (set! l1 (cdr l1)))
    (set-cdr! l2 (append syms l1))))

(define (custom-menu-insert-hook descr hook)
  (let ((hk (assq 'hook (cdar descr))))
    (if (not hk)
        (begin
          (set! hk (cons 'hook (make-hook 0)))
          (set-cdr! (car descr) (cons hk (cdar descr)))))
    (add-hook! (cdr hk) hook)))

(define (custom-menu-insert-context descr ctx)
  (let ((lc (assq 'local-ctx (cdar descr))))
    (if (not lc)
        (begin
          (set! lc (cons 'local-ctx '()))
          (set-cdr! (car descr) (cons lc (cdar descr)))))
    (set-cdr! lc (append ctx (cdr lc)))))

(define (custom-menu-find-name descr name)
  (if (null? name)
      descr
      (let ((lst (cdr descr)))
        (while (and (not (null? lst))
                    (or (not (list? (car lst)))
                        (not (string=? (car name) (caaar lst)))))
          (set! lst (cdr lst)))
        (if (null? lst)
            (begin
              (set! lst (list (list (list (car name)))))
              (set-cdr! descr (append (cdr descr) lst))))
        (custom-menu-find-name (car lst) (cdr name)))))

(define (custom-menu-install-symbols name syms)
  (custom-menu-insert-symbols (cond ((string? name)
                                     (custom-menu-find-name custom-menu-global-description (list name)))
                                    ((list? name)
                                     (custom-menu-find-name custom-menu-global-description name))
                                    ((not name)
                                     custom-menu-global-description))
                              syms))

(define (custom-menu-install-hook name hook)
  (custom-menu-insert-hook (cond ((string? name)
                                  (custom-menu-find-name custom-menu-global-description (list name)))
                                 ((list? name)
                                  (custom-menu-find-name custom-menu-global-description name))
                                 ((not name)
                                  custom-menu-global-description))
                           hook))

(define (custom-menu-install-context name ctx)
  (custom-menu-insert-context (cond ((string? name)
                                     (custom-menu-find-name custom-menu-global-description (list name)))
                                    ((list? name)
                                     (custom-menu-find-name custom-menu-global-description name))
                                    ((not name)
                                     custom-menu-global-description))
                              ctx))

(define (custom-menu-find-window name)
  (let ((lst (list-of-windows 'window)))
    (while (and (not (null? lst))
                (not (and (string=? (window-client-class (car lst)) "Gwm")
                          (equal? name (get-property (inner-deco (car lst)) 'custom-menu-path)))))
      (set! lst (cdr lst)))
    (if (null? lst)
        #f
        (car lst))))    

(define (custom-menu-pop descr ctx name xpos ypos)
  (let ((old (custom-menu-find-window name)))
    (if old
        (pop-to-window old)
        (let ((menu (custom-menu-make descr ctx name)))
          (place-menu menu (screen) xpos ypos :decoration panel-window)))))

(define (custom-menu)
  (custom-menu-pop custom-menu-global-description
                   (custom-menu-global-context)
                   '()
                   100 100))

(define (custom-menu-refresh-menus)
  (for-each (lambda (w)
              (if (and (string=? (window-client-class w) "Gwm")
                       (get-property (inner-deco w) 'custom-menu-path))
                  (let ((l (map (lambda (e)
                                  (get-property e 'descr))
                                (deco-parts (inner-deco w))))
                        (m (custom-menu-make (get-property (inner-deco w) 'descr)
                                             (custom-menu-global-context)
                                             (get-property (inner-deco w) 'custom-menu-path)))
                        (x (deco-x (inner-deco w)))
                        (y (deco-y (inner-deco w))))
                    (place-menu m (screen) x y :decoration panel-window)
                    (for-each (lambda (e)
                                (let ((d (get-property e 'descr)))
                                  (if (and d (car l))
                                      (begin
                                        (custom-menu-set-plug-value (get-property e 'ep)
                                                                    (cadr (car l)))
                                        (set-car! (cdr d) (cadr (car l)))
                                        (set-car! (cddr d) (caddr (car l)))
                                        (set-car! (cdddr d) (cadddr (car l)))))
                                  (set! l (cdr l))))
                              (deco-parts m))
                    (raise-window m w)
                    (delete-window w))))
            (list-of-windows 'window)))


;; Now, let this package install itself
(custom-menu-install-symbols "Customization menus"
  '("Customization menus"
    custom-menu-font
    custom-menu-symbol-font
    custom-menu-value-font
    custom-menu-docum-font
    custom-menu-foreground
    custom-menu-background
    custom-menu-button-background
    custom-menu-value-background
    custom-menu-value-frame
    custom-menu-immediate-change)
)

(custom-menu-install-hook "Customization menus" custom-menu-refresh-menus)

;;------------------------------------------------------------------------
;;
;; Custom menu automatic installer - automatically registers all defvar:s
;; in a loaded package in the custom menu hierarchy.
;;

(define cmi-symbol-list '())
(define cmi-package-name #f)
(define cmi-handled #f)

(define (cmi-register-var sym)
  (set! cmi-symbol-list (cons sym cmi-symbol-list)))

(advice (primitive-load-path f) 'custom-menu 'around
  (let ((old-symbol-list cmi-symbol-list)
        (old-package-name cmi-package-name)
        (old-handled cmi-handled))
    (set! cmi-symbol-list '())
    (set! cmi-package-name #f)
    (set! cmi-handled #f)
    (advice-inner f)
    (if (not cmi-package-name)
        (set! cmi-package-name
              (list (string-capitalize (regexp-substitute/global #f "[-_]"
                                                                 (basename f ".scm")
                                                                 'pre " " 'post)))))
    (if (and (not cmi-handled)
             (not (null? cmi-symbol-list)))
        (custom-menu-install-symbols cmi-package-name
                                     (if (null? (cdr (custom-menu-find-name custom-menu-global-description cmi-package-name)))
                                         (cons (car (reverse cmi-package-name))
                                               (reverse cmi-symbol-list))
                                         (reverse cmi-symbol-list))))
    (set! cmi-symbol-list old-symbol-list)
    (set! cmi-package-name old-package-name)
    (set! cmi-handled old-handled)))
    
(advice (custom-menu-install-symbols name syms) 'custom-menu 'after
  (set! cmi-handled #t))

(advice (custom-menu-install-hook name hook) 'custom-menu 'before
  (set! cmi-package-name (if (string? name) (list name) name)))

(advice (custom-menu-install-context name ctx) 'custom-menu 'before
  (set! cmi-package-name (if (string? name) (list name) name)))

(define (custom-menu-package-name name)
  (set! cmi-package-name (if (string? name) (list name) name)))

(defmacro defvar (var val doc prop)
  `(begin
     (if (not (defined? ',var))
         (begin (define ,var #f) (set! ,var ,val))) ; Avoid naming of proc
     (set-symbol-property! (quote ,var) 'doc ,doc)
     (set-symbol-property! (quote ,var) 'type ,prop)
     (cmi-register-var ',var)))

;;------------------------------------------------------------------------

;(add-to-hook! screen-opening custom-menu-load-preferences)

(add-to-hook! screen-closing (lambda (s) (custom-menu-save-preferences)))

;;And do it now too, just to make sure
;(custom-menu-load-preferences)
