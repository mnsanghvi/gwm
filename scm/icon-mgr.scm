;; icon-mgr.gwm --- Multiple Icon Managers
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file implements icon managers.
;;
;; By default the "default icon manager" handles all windows not in
;; 'icon-mgr-omit-list'. In addition, you can add icon managers with
;; '(make-icon-mgr NAME XPOS YPOS CONDITION)' which then "steals"
;; windows from the default icon manager.
;; CONDITION can be (as can the elements of 'icon-mgr-omit-list')
;; an atom representing a client class, a string representing a window
;; name regexp, a boolean function taking the window as argument, or a
;; list of one or several of the above.
;;
;; In addition to the variables below, you might want to copy the
;; setting of 'icon-mgr-behavior' to "gwmrc.scm" and change it.
;;


;;
;;    USER CUSTOMIZABLE VARIABLES
;;    ---------------------------  
;;    Adjust these in your own profile
;;
(defvar icon-mgr-name "Icon Manager" "Default name of icon manager" 'string)
(defvar icon-mgr-xpos 0 "Default x position" 'integer)
(defvar icon-mgr-ypos 0 "Default y position" 'integer)
(defvar show-default-icon-mgr #t "Default icon manager is used" 'boolean)
(defvar show-icon-mgr #t "Use icon managers" 'boolean)
(defvar icon-mgr-hide-if-empty #t "Don't show empty managers" 'boolean)
(defvar iconify-by-unmapping #t "Do not use icons by default" 'boolean)
(defvar iconify-unmanaged-by-icon #t "Use an icon if not in any manager" 'boolean)
(defvar iconify-by-icon-list '() "Always use icons for these" 'list)
(defvar iconify-on-start-list '() "List of window types to iconify on start" 'list)
(defvar icon-mgr-omit-list '() "Window types not handled in default manager" 'list)
(defvar icon-mgr-sort #f "Sort the entries in an icon manager" 'procedure)
(defvar icon-mgr-font "8x13" "Font in icon managers" 'font)
(defvar icon-mgr-width 150 "Width of icon manager" 'integer)
(defvar icon-mgr-foreground "black" "Foreground of icon managers" 'color)
(defvar icon-mgr-background "white" "Background of icon managers" 'color)
(defvar icon-mgr-title-foreground #f "Title foreground of icon managers" 'color)
(defvar icon-mgr-title-background #f "Title background of icon managers" 'color)
(defvar icon-mgr-no-title #f "Inhibits the icon manager title" 'boolean)
(defvar icon-mgr-framed-bars #f "Makes it look more like in real VTWM" 'boolean)
(defvar icon-mgr-fancy-colors '() "List of (wintype fg bg) specifications" 'list)
(defvar icon-mgr-pixmap-name (if (defined? 'iconify-pixmap-name)
                                 iconify-pixmap-name "iconify2")
  "Pixmap file for iconified symbol" 'string)

         ; In addition, the property 'icon-mgr-special-pixmap' can be set on
         ; a window to the pixmap to use in the icon manager for that window.


(define (icon-mgr-list)
  (or (get-property (root-window) 'icon-mgrs) '()))

(define (default-icon-mgr)
  (get-property (root-window) 'default-icon-mgr))

(define (icon-mgr-focusin win)
  (let ((bar (get-property win 'imgr-bar)))
    (if (and (deco? bar) (deco-valid? bar))
        (send-user-event 'imgr-focusin bar))))

(define (icon-mgr-focusout win)
  (let ((bar (get-property win 'imgr-bar)))
    (if (and (deco? bar) (deco-valid? bar))
        (send-user-event 'imgr-focusout bar))))

(define (icon-mgr-update win)
  (if (window-valid? win)
      (let ((bar (get-property (window-deco win) 'imgr-bar)))
        (if (and (deco? bar) (deco-valid? bar))
            (send-user-event 'imgr-rethink bar)))))

(define (icon-mgr-raise-all)
  (for-each (lambda (ele)
              (let ((mgr (vector-ref ele 0)))
                (if (and mgr (deco-valid? mgr))
                    (raise-window (top-deco mgr)))))
            (icon-mgr-list)))

(define (icon-mgr-lower-all)
  (for-each (lambda (ele)
              (let ((mgr (vector-ref ele 0)))
                (if (and mgr (deco-valid? mgr))
                    (lower-window (top-deco mgr)))))
            (icon-mgr-list)))

(if (not (defined? 'icon-mgr-header-behavior))
(define icon-mgr-header-behavior
  (make-behavior
   (on (button 1 alone)
       (icon-mgr-raise-all))
   (on (button 2 alone)
       (user-move-window (top-deco deco) event))
   (on (button 3 alone)
       (icon-mgr-lower-all))))
)

(defmacro icon-mgr-with-window body
  `(let ((deco (get-property deco 'window))
         (vis #f))
     (if (deco-valid? deco)
         (begin
           (set! vis (window-mapped? deco))
           ,@body
           (if (deco-valid? deco)
               (if (and vis (not (window-mapped? deco)))
                   (run-hook leave-window-hook deco)
                   (if (and (not vis) (window-mapped? deco))
                       (run-hook enter-window-hook deco))))))))

(if (not (defined? 'icon-mgr-behavior))
(define icon-mgr-behavior
  (make-behavior
   (on (button 1 any)
       (icon-mgr-with-window
        (pop-to-window deco)))
   (on (button 2 any)
       (icon-mgr-with-window
        (toggle-iconify-window deco)))
   (on (button 3 any)
       (icon-mgr-with-window
        (deiconify-window deco)
        (lower-window deco)))
   ))
)

(define imgr-bar-behavior
  (make-behavior
   (on (enter)
       (let ((d (get-property deco 'window)))
         (if (and d (deco-valid? d))
             (run-hook enter-window-hook d))))
   (on (leave)
       (let ((d (get-property deco 'window)))
         (if (and d (deco-valid? d))
             (run-hook leave-window-hook d))))
   (on (user-event 'imgr-focusin)
       (set-deco-bordercolor! deco (car (get-property deco 'cols))))
   (on (user-event 'imgr-focusout)
       (set-deco-bordercolor! deco (cadr (get-property deco 'cols))))))

(define (imgr-icon-pixmap win bg fg)
  (or (get-property win 'icon-mgr-special-pixmap)
      (if (pixmap? icon-mgr-pixmap-name)
          icon-mgr-pixmap-name
          #f)
      (make-pixmap bg
                   icon-mgr-pixmap-name
                   fg)))

(define (imgr-smart-icon-name win)
  (if (string=? (window-icon-name win) "icon") ; Means that no icon name was specified
      (window-name win)
      (window-icon-name win)))

(define imgr-icon-behavior
  (make-behavior
   (on (user-event 'imgr-rethink)
       (if (window-iconified? (get-property deco 'window))
           (show-deco deco)
           (hide-deco deco)))))

(define imgr-label-behavior
  (make-behavior
   (on (user-event 'imgr-rethink)
       (let ((cols (get-property deco 'cols))
             (font (if (string? icon-mgr-font)
                   (make-font icon-mgr-font)
                   icon-mgr-font)))
         (set-deco-part! deco 1 
                         (make-label (imgr-smart-icon-name (get-property deco 'window))
                                     :font font
                                     :horizontal-margin 4
                                     :vertical-margin 2
                                     :background (cadr cols)
                                     :foreground (car cols)))))))

(define (imgr-make-header-bar name)
  (let* ((fg (or icon-mgr-title-foreground icon-mgr-foreground))
         (bg (or icon-mgr-title-background icon-mgr-background))
         (foreground (if (string? fg) (make-color fg) fg))
         (background (if (string? bg) (make-color bg) bg))
         (font (if (string? icon-mgr-font)
                   (make-font icon-mgr-font)
                   icon-mgr-font))
         (label (make-label name 
                            :font font
                            :horizontal-margin 4
                            :vertical-margin 2
                            :background background
                            :foreground foreground)))
    (if icon-mgr-framed-bars
        (make-deco (make-deco '() label '()
                              :direction 'horizontal
                              :background background
                              :bordercolor foreground
                              :borderwidth 2)
                   :background background
                   :bordercolor background
                   :borderwidth 1
                   :behavior icon-mgr-header-behavior)
        (make-deco '() label '()
                   :background background
                   :bordercolor foreground
                   :borderwidth 1
                   :behavior icon-mgr-header-behavior))))

(define (imgr-make-bar wind)
  (let* ((cols (matches-cond wind icon-mgr-fancy-colors))
         (fg (or (and cols (car cols)) icon-mgr-foreground))
         (bg (or (and cols (cadr cols)) icon-mgr-background))
         (foreground (if (string? fg) (make-color fg) fg))
         (background (if (string? bg) (make-color bg) bg))
         (font (if (string? icon-mgr-font)
                   (make-font icon-mgr-font)
                   icon-mgr-font))
         (property `((window . , wind)
                     (cols . ,(list foreground background)))))
    (let ((icon (make-deco (imgr-icon-pixmap wind background foreground)
                           :behavior imgr-icon-behavior
                           :background background
                           :property property))
          (label (make-deco (make-label (imgr-smart-icon-name wind)
                                        :font font
                                        :horizontal-margin 4
                                        :vertical-margin 2
                                        :background background
                                        :foreground foreground)
                            :behavior imgr-label-behavior
                            :property property))
          (res #f))
      (if (not (window-iconified? wind))
          (hide-deco icon))
      (set! res
            (if icon-mgr-framed-bars
                (make-deco (make-deco (make-deco '() :width 3 :background background)
                                      icon
                                      (make-deco '() :width 3 :background background)
                                      label
                                      '()
                                      :direction 'horizontal
                                      :background background
                                      :bordercolor foreground
                                      :borderwidth 1)
                           :background background
                           :bordercolor background
                           :borderwidth 2
                           :behavior (make-behavior icon-mgr-behavior imgr-bar-behavior)
                           :property property)
                (make-deco icon label '()
                           :borderwidth 1
                           :background background
                           :bordercolor background
                           :behavior (make-behavior icon-mgr-behavior imgr-bar-behavior)
                           :property property)))
      (set-property! wind 'imgr-bar res)
      res)))
  
(define (imgr-window-list imcond)
  (let ((lst '()))
    (for-each (lambda (w)
                (if (imcond w)
                    (set! lst (cons w lst))))
              (list-of-windows 'window))
    (if icon-mgr-sort
        (sort lst
              (if (procedure? icon-mgr-sort)
                  icon-mgr-sort
                  (lambda (w1 w2)
                    (string-ci<? (imgr-smart-icon-name w1)
                                 (imgr-smart-icon-name w2)))))
        lst)))

(define (imgr-create-menu name winlist)
  (let ((header (if (not icon-mgr-no-title)
                    (imgr-make-header-bar name)
                    #f))
        (items (map imgr-make-bar winlist)))
    (apply make-deco (append (if header (list header) '())
                             items
                             (list :width icon-mgr-width
                                   :property `((header . ,header)
                                               (winlist . ,winlist)))))))

(define (imgr-menu-add menu wind)
  (let ((item (imgr-make-bar wind))
        (winlist (get-property menu 'winlist))
        (ind 0))
    (if icon-mgr-sort
        (let ((method (if (procedure? icon-mgr-sort)
                          icon-mgr-sort
                          (lambda (w1 w2)
                            (string-ci<? (imgr-smart-icon-name w1)
                                         (imgr-smart-icon-name w2)))))
              (tmp winlist))
          (while (and (not (null? tmp)) (not (method wind (car tmp))))
            (set! tmp (cdr tmp))
            (set! ind (+ ind 1))))
        (set! ind (length winlist)))
    (set-property! menu 'winlist 
                   (append (list-head winlist ind)
                           (list wind)
                           (list-tail winlist ind)))
    (deco-add-part! menu (if (get-property menu 'header)
                             (+ 2 ind)
                             (+ 1 ind))
                    item)))
            
(define (imgr-menu-remove menu wind)
  (let* ((winlist (get-property menu 'winlist))
         (ind (list-index winlist wind)))
    (if ind
        (begin
          (set-property! menu 'winlist (delq wind winlist))
          (set-property! wind 'imgr-bar #f)
          (deco-remove-part! menu (if (get-property menu 'header)
                                      (+ 2 ind)
                                      (+ 1 ind)))))))
    
(define (imgr-decouple-all)
  (for-each (lambda (w)
              (if (get-property w 'imgr-bar)
                  (set-property! w 'imgr-bar #f)))
            (list-of-windows 'window)))
  
(define (imgr-consider-icon win)
  (if (icon-decorated? win)
      (if (or (not iconify-by-unmapping)
              (matches-list (window-deco win) iconify-by-icon-list)
              (and iconify-unmanaged-by-icon
                   (not (get-property (window-deco win) 'imgr-bar))))
          (unhide-window (icon-deco win) 'icon-mgr)
          (hide-window (icon-deco win) 'icon-mgr))))

(define (imgr-delete-menu ele)
  (let ((menu (vector-ref ele 0)))
    (if (and menu (deco-valid? menu))
        (begin
          (vector-set! ele 2 (get-menu-gpos menu))
          (delete-window (top-deco menu))
          (vector-set! ele 0 #f)))))

(define (imgr-show-menu ele)
  (let ((menu (vector-ref ele 0))
        (name (or (vector-ref ele 1) icon-mgr-name))
        (gpos (or (vector-ref ele 2) (coord->gpos icon-mgr-xpos icon-mgr-ypos)))
        (cond (lambda (w) 
                (and (not (string=? (window-client-class w) "Gwm"))
                     (not (get-property w 'imgr-bar))
                     (matches-token w (vector-ref ele 3))))))
    (if (not (and menu (deco-valid? menu) (deco-valid? (top-deco menu))))
        (begin
          (set! menu (imgr-create-menu name (imgr-window-list cond)))
          (vector-set! ele 0 menu)
          (place-menu-gpos menu name gpos)))
    (if (or (not icon-mgr-hide-if-empty)
            (not (null? (get-property menu 'winlist))))
        (unhide-window (top-deco menu) 'icon-mgr-menu)
        (hide-window (top-deco menu) 'icon-mgr-menu))))

(define (icon-mgr-add win)
  (if (and show-icon-mgr
           (not (deco-icon? win))
           (not (string=? (window-client-class win) "Gwm")))
      (begin
        (if (not (or-map (lambda (ele)
                           (if (and (vector-ref ele 0)
                                    (matches-token win (vector-ref ele 3))
                                    (or show-default-icon-mgr
                                        (not (eq? ele (default-icon-mgr)))))
                               (begin
                                 (imgr-menu-add (vector-ref ele 0) win)
                                 (imgr-show-menu ele)
                                 #t)
                               #f))
                         (icon-mgr-list)))
            (if (and (not iconify-unmanaged-by-icon)
                     (icon-decorated? win))
                (hide-window (icon-deco win) 'icon-mgr)))
        (if (and iconify-by-unmapping
                 (icon-decorated? win)
                 (not (matches-list win iconify-by-icon-list)))
            (hide-window (icon-deco win) 'icon-mgr))
        (if (matches-list win iconify-on-start-list)
            (iconify-window win)))))

(define (icon-mgr-remove win)
  (let ((bar (get-property win 'imgr-bar)))
    (if (and show-icon-mgr
             bar (deco-valid? bar))
        (or-map (lambda (ele)
                  (if (and (vector-ref ele 0)
                           (eq? (top-deco (vector-ref ele 0))
                                (top-deco bar))
                           (or show-default-icon-mgr
                               (not (eq? ele (default-icon-mgr)))))
                      (begin
                        (imgr-menu-remove (deco-parent bar) win)
                        (imgr-show-menu ele)
                        #t)
                      #f))
                (icon-mgr-list)))))

(define (icon-mgr-show)
  (imgr-decouple-all)
  (if show-icon-mgr
      (begin
        (for-each (lambda (ele)
                    (imgr-delete-menu ele)
                    (if (or show-default-icon-mgr
                            (not (eq? ele (default-icon-mgr))))
                        (imgr-show-menu ele)))
                  (icon-mgr-list))
        (if (deco? (get-focus))
            (icon-mgr-focusin (top-deco (get-focus)))))
      (for-each (lambda (ele)
                  (imgr-delete-menu ele))
                (icon-mgr-list)))
  (for-each imgr-consider-icon (list-of-windows 'window)))

(define (icon-mgr-toggle)
  (set! show-icon-mgr (not show-icon-mgr))
  (icon-mgr-show))

(define (make-icon-mgr name xpos ypos imcond)
  (let ((a (make-vector 4 #f)))
    (vector-set! a 1 name)
    (vector-set! a 2 (coord->gpos xpos ypos))
    (vector-set! a 3 imcond)
    (set-property! (root-window) 'icon-mgrs
                   (cons a (icon-mgr-list))))
  (icon-mgr-show))

(define (remove-icon-mgr name)
  (or-map (lambda (ele)
            (if (equal? name (vector-ref ele 1))
                (begin
                  (imgr-delete-menu ele)
                  (set-property! (root-window) 'icon-mgrs
                                 (delete ele (icon-mgr-list)))
                  (icon-mgr-show)
                  #t)
                #f))
          (icon-mgr-list)))

;; Install icon-mgr

(define (imgr-init-screen s)
  (let ((a (make-vector 4 #f))
        (f (lambda (w)
             (not (matches-list w icon-mgr-omit-list)))))
    (set-screen! s)
    (vector-set! a 3 f)
    (set-property! (root-window) 'default-icon-mgr a)
    (set-property! (root-window) 'icon-mgrs (list a))
    (icon-mgr-show)))

(add-to-hook! window-opening icon-mgr-add)

(add-to-hook! window-closing icon-mgr-remove)

(add-to-hook! icon-opening imgr-consider-icon)

(add-to-hook! name-change-hook icon-mgr-update)

(add-to-hook! focus-in-hook icon-mgr-focusin)

(add-to-hook! focus-out-hook icon-mgr-focusout)

(advice iconify-window 'icon-mgr 'after (icon-mgr-update (car args)))

(advice deiconify-window 'icon-mgr 'after (icon-mgr-update (car args)))

(custom-menu-install-hook "Icon Manager" icon-mgr-show)

(if (gwm-is-starting)
    (add-to-hook! screen-opening imgr-init-screen)
    (map imgr-init-screen (list-of-screens)))


(define icon-mgr #t)

