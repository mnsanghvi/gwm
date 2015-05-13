;; menu-func.scm --- General popup menu functionality
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; There are (at least) three separate aspects of popup-menus: their
;; behavior, their appearance, and their contents. Each of these things
;; should be possible to specify independently (almost) of the others. 
;; This file provides basic functionality concerning all three aspects.
;; 
;; First in implements a basic menu behavior: give feedback when entering
;; or leaving an item, pop and unpop submenus as appropriate, and execute
;; the selected action. This results in three behaviors to use with the
;; next aspect.
;;
;; The next aspect is the appearance: What should menu items and labels 
;; look like, and how should feedback be given. This should be specified
;; in a "menu context", a context list containing values for the tags
;; :menu-func, :item-func, :label-func, :separator-func, :foreground,
;; :background, and :font. The last three are not absolutely necessary,
;; they are provided as hints for other widgets that may want to have a
;; similar colorization as the menus.
;; To continue backwards, the tag :separator-func should have a function
;; that produces a decoration for a menu separator. It takes no argument,
;; and need not have any behavior.
;; The tag :label-func should have a function that produces a decoration
;; for a menu label. It takes the label string as argument, and should
;; use std-menu-label-behavior as (or as part of) its top level behavior.
;; The tag :item-func should have a function that produces a decoration
;; for a menu item. It takes the name of the item, and an action as
;; argument. The action is either a function to run (with one argument -
;; the decoration of the caller of the menu) when the item is selected,
;; or another menu to be used as a sub-menu. The item decoration should
;; use the std-menu-item-behavior as part of its top behavior. Any part
;; of it could listen to the user events 'active and 'inactive, which
;; means that the item is entered or left, and give the appropriate
;; feedback. The item decoration should also on its top level have either
;; a property 'action with the function to run on selection, or a
;; property 'menu with the sub-menu to use.
;; Finally, the tag :menu-func should have a function that produces the 
;; entire menu decoration given as arguments the decorations produced by
;; the other functions. It should use std-menu-behavior as part of its 
;; top behavior.
;; Most window styles have a corresponding menu style. If this simple 
;; protocoll is followed, this should be fairly simple to implement, and
;; all user interaction is taken care of. This file implements the most
;; simple (and boring looking black text on white background) menu style,
;; simple-menu-context, as an example and to be used if nothing else is
;; specified.
;;
;; The last aspect is the menu contents. This file implements a set of
;; functions that helps creating a menu of any menu style given a simple 
;; specification of the contents.
;; The main function is (construct-menu CONTEXT LINESPEC ...) which takes
;; as arguments the menu context to use, and then specifications of each
;; line in the menu. The line specifications can be: a string, resulting
;; in a label; the value #t, resulting in a separator; a list with a string
;; and a function, resulting in an item running the function when selected;
;; a list with a string and another list of more line specifications,
;; resulting in an item with a submenu; or a decoration or list of
;; decorations, inserted directly into the menu.
;; The other functions produce specific menu item decorations that can be 
;; sent directly to 'construct-menu'. (construct-machine-menu-item CONTEXT
;; HOST FUNCTION) produces a menu item to log in to a specific host. The
;; name of the item will be produced from the hostname, and if selected,
;; the function will be called with the host as argument, and is supposed
;; to perform the actual login. (construct-machine-menu-items CONTEXT
;; HOSTLIST FUNCTION) produces a list of such login items at once.
;; The function (construct-window-menu-item CONTEXT WINDOW FUNCTION) 
;; produces a menu item with the window name as its name, and which calls
;; the function on the window if selected. (construct-window-menu-items
;; CONTEXT CONDITION FUNCTION) produces a list of such items for all windows
;; matching the condition. These functions are useful to produce a menu
;; with all windows, which brings forward the selected window.
;; The function (construct-dialogue-menu-item CONTEXT NAME QUERY FUNCTION)
;; uses the 'simple-dialogue' function to produce a menu item with the given
;; name that when selected asks the user for a string, and then calls the
;; given function with that string.
;;
;; To pop a menu produced with these standard functions, use the function
;; (menu-pop MENU CALLER EVENT).
;;

(defvar menu-stay-popped #f "Whether menu should stay popped when button is released" 'boolean)

(require 'dialogue)

(define (menu-father m)
  (if (deco-parent m)
      (top-deco m)
      (let ((i (get-property m 'father-item)))
        (if i
            (top-deco i)
            #f))))

(define (menu-father-item m)
  (let ((i (get-property (top-deco m) 'father-item)))
    i))

(define (menu-root m)
  (let ((w (menu-father m)))
    (while w
      (set! m w)
      (set! w (menu-father w)))
    m))

(define (menu-pop-cascade i)
  (let* ((expr (get-property i 'menu-expr))
         (cmenu (if expr (apply expr '()) (get-property i 'menu)))
         (croot (menu-root i)))
    (set-property! (menu-father i) 'cascademenu cmenu)
    (set-property! cmenu 'poped #t)
    (set-property! cmenu 'father-item i)
    (menu-pop cmenu #f #f 
              :pos (list (+ (deco-x i) (deco-width i)) (deco-y i))
              :parent (menu-father i))
    ))

(define (menu-unpop-cascade m)
  (let ((cmenu (get-property m 'cascademenu)))
    (if cmenu
        (let ((cur (get-property cmenu 'current)))
          (menu-unpop-cascade cmenu)
          (if cur
              (send-user-event 'inactive cur))
          (set-property! cmenu 'current #f)
          (set-property! cmenu 'poped #f)
          (set-property! cmenu 'father-item #f)
          (menu-unpop cmenu)
          (set-property! m 'cascademenu #f)))))

(define (menu-unpop-old-cascade i)
  (let ((cm (get-property (menu-father i) 'cascademenu))
        (cur (get-property (menu-father i) 'current)))
    (if (and cm
             (not (eq? i (menu-father-item cm))))
        (begin
          (menu-unpop-cascade (menu-father i))
          (if cur
              (begin
                (send-user-event 'inactive cur)
                (set-property! (menu-father i) 'current #f)))))))

(define (menu-unpop-all-cascade w)
  (let* ((w (menu-root w))
         (cur (get-property w 'current))
         (caller (get-property w 'caller)))
    (if menu-stay-popped 
        (set-property! w 'nailed #f))
    (menu-unpop-cascade w)
    (if cur
        (send-user-event 'inactive cur))
    (set-property! w 'current #f)
    (set-property! w 'poped #f)
    (menu-unpop w)
    (if (and autocolormap
             caller
             (not (eq? (root-window) caller)))
        (set-colormap-focus! caller))))

(define (menu-get-item-list m)
  (or (get-property m 'items)
      (let ((res '()))
        (send-user-event (cons 'item-respond
                               (lambda (d) (set! res (cons d res))))
                         m)
        (set! res (reverse! res))
        (set-property! m 'items res)
        res)))

(define std-menu-item-behavior
  (make-behavior
   (on (buttonpress any any)
       (if (not (or (get-property deco 'menu)
                    (get-property deco 'menu-expr)))
           (send-user-event 'pressed deco)))
   (on (buttonrelease any any)
       (send-user-event 'active deco)
       (send-user-event 'doit deco))
   (on (enter) 
       (menu-unpop-old-cascade deco)
       (send-user-event 'active deco)
       (set-property! (menu-father deco) 'current deco)
       (if (and (not (get-property (menu-father deco) 'cascademenu))
                (or (get-property deco 'menu)
                    (get-property deco 'menu-expr)))
           (menu-pop-cascade deco)))
   (on (leave) 
       (if (not (or (get-property deco 'menu)
                    (get-property deco 'menu-expr)))
           (let ((cur (get-property (menu-father deco) 'current)))
             (send-user-event 'inactive deco)
             (if (and cur (eq? cur deco))
                 (set-property! (menu-father deco) 'current #f)))))
   (on (user-event 'doit)
       (let ((caller (get-property (menu-root deco) 'caller))
             (action (get-property deco 'action)))
         (menu-unpop-all-cascade deco)
         (if action
             (action caller))))
   (on (user-event 'item-respond)
       (let ((f (cdr (event-data event))))
         (f deco))) 
   ))

(define std-menu-label-behavior
  (make-behavior 
   (on (buttonrelease any any) 
       (let ((r (menu-root deco)))
         (if (and menu-stay-popped
                  (not (get-property r 'nailed)))
             (set-property! r 'nailed #t)
             (menu-unpop-all-cascade deco))))
   (on (enter)
       (menu-unpop-old-cascade deco))
   ))

(define std-menu-behavior
  (make-behavior
   (on (buttonpress any any)
       #f)
   (on (buttonrelease any any) 
       (menu-unpop-all-cascade deco))
   (on (key "Return")
       (while (get-property deco 'cascademenu)
         (set! deco (get-property deco 'cascademenu)))
       (let ((cur (get-property deco 'current)))
         (if cur
             (if (or (get-property cur 'menu)
                     (get-property cur 'menu-expr))
                 (menu-pop-cascade cur)
                 (send-user-event 'doit cur)))))
   (on (key "Escape")
       (menu-unpop-all-cascade deco))
   (on (key "Down")
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
       (while (get-property deco 'cascademenu)
         (set! deco (get-property deco 'cascademenu)))
       (let ((par (menu-father deco)))
         (if par
             (menu-unpop-cascade par))))
   (on (key "Right")
       (while (get-property deco 'cascademenu)
         (set! deco (get-property deco 'cascademenu)))
       (let ((cur (get-property deco 'current)))
         (if (and cur
                  (or (get-property cur 'menu)
                      (get-property cur 'menu-expr)))
             (menu-pop-cascade cur))))
   ))

(define (menu-unpop w)
  (let ((hook (get-property w 'unpop-hook)))
    (if hook
        (run-hook hook w))
    (unpop-menu w)))

(define (menu-pop mn deco ev . args)
  (let ((hook (get-property mn 'pop-hook))
        (bw (deco-borderwidth mn))
        (wdt (deco-width mn))
        (hgt (deco-height mn))
        (xm (if ev (event-x ev) 0))
        (ym (if ev (event-y ev) 0))
        (pos (get-keyword :pos args '()))
        (par (get-keyword :parent args #f))
        (xpos #f)
        (ypos #f))
    (set! xpos (if (= (length pos) 2)
                   (car pos)
                   (- xm (quotient wdt 2) bw)))
    (set! ypos (if (= (length pos) 2)
                   (cadr pos)
                   (- ym 4 bw)))
    (if (< xpos 0)
        (set! xpos 0))
    (if (> (+ xpos wdt) (screen-width))
        (set! xpos (- (screen-width) wdt)))
    (if (< ypos 0)
        (set! ypos 0))
    (if (> (+ ypos hgt) (screen-height))
        (set! ypos (max 0 (- (screen-height) hgt))))
    (if (and autocolormap
             (not (eq? (root-window) mn)))
        (set-colormap-focus! (root-window)))
    (set-property! mn 'caller deco)
    (pop-menu mn (root-window) xpos ypos :menu-parent par)
    (if hook
        (run-hook hook mn xpos ypos))
))

;;
;; Simple menu constructors
;;

(define (simple-menu-item-make label action)
  (let ((menu (if (deco? action) action #f)))
    (make-deco '() (make-label label :horizontal-margin 2 :vertical-margin 1) '()
               :borderwidth 1
               :bordercolor white
               :behavior (make-behavior 
                          (on (user-event 'active)
                              (set-deco-bordercolor! deco black))
                          (on (user-event 'inactive)
                              (set-deco-bordercolor! deco white))
                          std-menu-item-behavior)
               :property `((action . ,(if menu #f action))
                           (menu . ,menu)))))
     
(define (simple-menu-label-make label)
  (make-deco '() (make-label label :horizontal-margin 2 :vertical-margin 1) '()
             :borderwidth 1
             :bordercolor black
             :behavior std-menu-label-behavior))

(define (simple-menu-separator-make)
  (make-deco '() :height 1 :background black))

(define (simple-menu-make . args)
  (apply make-deco (append args
                           (list :borderwidth 1
                                 :borderpixel black
                                 :behavior std-menu-behavior))))

(define simple-menu-context
  (list :menu-func simple-menu-make
        :item-func simple-menu-item-make
        :label-func simple-menu-label-make
        :separator-func simple-menu-separator-make
        :foreground (lambda () "black")
        :background (lambda () "white")
        :font #f))

;;
;; Remote machine command menus
;;

(define (machine-name host)
  (let ((match (string-match "^\\([^.]*\\)" host)))
    (if match
        (string-capitalize (match:substring match 1))
        host)))

(define (construct-machine-menu-item ctx host func)
  (let ((item-make-func (get-keyword :item-func ctx simple-menu-item-make)))
    (item-make-func (machine-name host)
                    (lambda (x)
                      (func host)))))

(define (construct-machine-menu-items ctx hostlst func)
  (let ((item-make-func (get-keyword :item-func ctx simple-menu-item-make)))
    (map (lambda (host)
           (item-make-func (machine-name host)
                           (lambda (x)
                             (func host))))
         hostlst)))

(define (construct-window-menu-item ctx win func)
  (let ((item-make-func (get-keyword :item-func ctx simple-menu-item-make)))
    (item-make-func (window-name win)
                    (lambda (x)
                      (if (deco-valid? win)
                          (func win))))))

(define (construct-window-menu-items ctx cond func)
  (let ((item-make-func (get-keyword :item-func ctx simple-menu-item-make)))
    (delq #f (map (lambda (win)
                    (if (cond win)
                        (item-make-func (window-name win)
                                        (lambda (x)
                                          (if (deco-valid? win)
                                              (func win))))
                        #f))
                  (list-of-windows 'window)))))

(define (construct-dialogue-menu-item ctx name query func)
  (let ((item-make-func (get-keyword :item-func ctx simple-menu-item-make))
        (fg (get-keyword :foreground ctx #f))
        (bg (get-keyword :background ctx #f))
        (font (get-keyword :font ctx #f)))
    (item-make-func name
                    (lambda (x)
                      (let ((res (simple-dialogue query ""
                                                  :background (make-color (if bg (bg) "white"))
                                                  :active-background (make-color "white")
                                                  :foreground (make-color (if fg (fg) "black"))
                                                  :font (make-font (if font (font) "fixed")))))
                        (if res (func res)))))))

(define (construct-menu ctx . item-lists)
  (let ((menu-make-func (get-keyword :menu-func ctx simple-menu-make))
        (label-make-func (get-keyword :label-func ctx simple-menu-label-make))
        (item-make-func (get-keyword :item-func ctx simple-menu-item-make))
        (separator-make-func (get-keyword :separator-func ctx simple-menu-separator-make)))
    (apply menu-make-func
           (apply append (map (lambda (ele)
                                (cond ((string? ele)
                                       (list (label-make-func ele)))
                                      ((eq? ele #t)
                                       (if separator-make-func
                                           (list (separator-make-func))
                                           '()))
                                      ((null? ele)
                                       '())
                                      ((deco? ele)
                                       (list ele))
                                      ((and (list? ele)
                                            (deco? (car ele)))
                                       ele)
                                      ((and (list? ele)
                                            (string? (car ele))
                                            (or (procedure? (cadr ele))
                                                (deco? (cadr ele))))
                                       (list (item-make-func (car ele)
                                                             (cadr ele))))
                                      ((and (list? ele)
                                            (string? (car ele)))
                                       (list (item-make-func (car ele)
                                                             (apply construct-menu ctx (cadr ele)))))
                                      (#t
                                       '())))
                              item-lists)))))

