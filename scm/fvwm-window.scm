;; fvwm-window.scm --- Fvwm style windows.
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Fvwm style windows. Do '(set-window #t fvwm-window)' to use them.
;;

;;    User customizable variables. Adjust these in your own profile.
(defvar fvwm-color "tan" "Main color of window" 'color)
(defvar fvwm-active-color "palevioletred" "Color when in focus" 'color)
(defvar fvwm-title-color "black" "Color of title text" 'color)
(defvar fvwm-active-title-color #f "Color of text when in focus" 'color)
(defvar fvwm-title-font "6x13bold" "Font of title text" 'font)
(defvar fvwm-title-height #f "Title width. If #f, set from font." 'integer)
(defvar fvwm-frame-width 6 "Frame width." 'integer)
(defvar fvwm-corner-size #f "Resize corner size. If #f, set automatic." 'integer)
(defvar fvwm-has-frame #t "Window has border or resize handles" 'boolean)
(defvar fvwm-has-resize #t "Window has resize handles" 'boolean)
(defvar fvwm-has-title #t "Window has title bar" 'boolean)
(defvar fvwm-wide-shadow #f "Slightly wider shadows around title bar" 'boolean)
(defvar fvwm-inner-border -1 "Use of inner border" 'integer)
(defvar fvwm-outer-border #f "Use of outer border" 'integer)
(defvar fvwm-left-buttons `((,(lambda args (apply horizontal-rectangle args)) ,(lambda (w e) (menu-pop (window-pop) w e)) press))
  "Left plugs in the titlebar, a list of pairs or tripplets:  ( <pixmap-file> <action> [<on-press>] )" 'list)
(defvar fvwm-right-buttons `((,(lambda args (apply up-triangle args)) ,(lambda (w e) (zoom-window w)))
                             (,(lambda args (apply down-triangle args)) ,(lambda (w e) (iconify-window w)) release))
  "Right plugs in the titlebar, a list of pairs or tripplets:  ( <pixmap-file> <action> [<on-press>] )" 'list)
           ; Left and right plugs in the titlebar, a list of pairs
           ; or triplets:  ( <button-graphic> <action> [<action-type>] )
           ; The optional third element <action-type> can be `press' or
           ; `release', if the action should be all done on the button press
           ; (good for menus) or executed on the button release (good for
           ; eg. iconification and deletion of the window).
           ; Possible graphics are: small-square, medium-square,
           ; large-square, horizontal-rectangle, vertical-rectangle,
           ; medium-circle, medium-diamond, down-triangle, and up-triangle.
           ; Check how these are defined if you like to do your own.
(defvar fvwm-border-action (lambda (w e) (user-move-window w e)) "Action to execute when clicking border" 'procedure)
(defvar fvwm-corner-action (lambda (w e) (fvwm-resize-window w e)) "Action to execute when clicking corner" 'procedure)
(defvar fvwm-side-action (lambda (w e) (fvwm-resize-window w e)) "Action to execute when clicking side" 'procedure)
(defvar fvwm-title-action (lambda (w e) (raise-lower-move-window w e)) "Action to execute when clicking titlebar" 'procedure)
           ; Actions to execute when pressing the border, the resize corner,
           ; the resize side, or the title bar.
           ; These actions, and the <action> components for the left and right
           ; plugs above, can actually also be lists of pairs or triplets:
           ; ( <button-spec> <action> [<action-type>] ), where <button-spec>
           ; is a mouse button number or a list with a mouse button number
           ; followed by any number of modifiers.
(defvar fvwm-color-styles `((,(lambda (w) (virtual-nailed w)) "palegreen")
                            (XTerm "grey")
                            (Emacs "lightblue"))
  "Specifications for different colors of different windows" 'list)
           ; List of (<win-spec> <color> <active-color> <title-color>
           ; <active-title-color>) specifications, to get different colors
           ; on different windows. <win-spec> can be an atom (matching the
           ; client class), a string (regexp matching title), or an
           ; arbitrary expression (giving nil or non-nil).
(defvar fvwm-window-styles '((XClock :resize #f :title #f :active-color #f)
                             (XLoad :resize #f :title #f :active-color #f)
                             (XBiff :resize #f :title #f :active-color #f)
                             (Gwm :resize #f :title #f :active-color #f))
  "Specifications for different styles on different windows" 'list)
           ; List of (<win-spec> <key1> <val1> ...) specifications, to
           ; get individual appearances on windows. The possible <key_i>
           ; for which individual values can be specified are:
           ; color, active-color, title-color, active-title-color,
           ; title-font, title-height, frame-width, corner-size, frame,
           ; resize, title, wide-shadow, inner-border, outer-border,
           ; left-buttons, right-buttons, border-action, corner-action,
           ; side-action, title-action.
           ; A window can match several of the <win-spec> in the list (unlike
           ; `fvwm-color-styles' where only the first hit applies).
(defvar fvwm-virtual-colors #f "Affect colors in virtual map." 'boolean)


;;--------------------------------------------------------------------------
;;   End of user customizable things. Here starts the real code.
;;--------------------------------------------------------------------------

(define fvwm-resize-cursors #t)
(define cursor-NW (make-cursor 134))
(define cursor-NE (make-cursor 136))
(define cursor-SW (make-cursor 12))
(define cursor-SE (make-cursor 14))
(define cursor-N  (make-cursor 138))
(define cursor-S  (make-cursor 16))
(define cursor-W  (make-cursor 70))
(define cursor-E  (make-cursor 96))
(define fvwm-frame-cursor (make-cursor 68))

(require 'zoom-window "zoom")
(require 'virtual)

;; COLORS 
;; =======

(define (darken-color col)
  (let ((rgb (color-components col)))
    (make-color (inexact->integer (* (car rgb) 0.67))
                (inexact->integer (* (cadr rgb) 0.67))
                (inexact->integer (* (caddr rgb) 0.67)))))

(define (lighten-color col )
  (let ((wht (color-components (make-color "white")))
        (rgb (color-components col)))
    (make-color (inexact->integer (* (+ (car wht) (car rgb)) 0.5))
                (inexact->integer (* (+ (cadr wht) (cadr rgb)) 0.5))
                (inexact->integer (* (+ (caddr wht) (caddr rgb)) 0.5)))))

(define (make-color-scheme bg fg)
  (let* ((bg (if (string? bg) (make-color bg) bg))
         (fg (if (string? fg) (make-color fg) fg))
         (dark (darken-color bg))
         (light (lighten-color bg)))
    (list light bg dark fg)))

; THE TILES :
; =========

(define (fvwm-vbar-tile size cols spec)
  (let ((pix (make-pixmap size 1 :background (cadr cols))))
    (if (eq? spec 'right)
        (draw-point pix 0 0 :color (car cols))
        (draw-line pix 0 0 1 0 :color (car cols)))
    (if (eq? spec 'left)
        (draw-point pix (- size 1) 0 :color (caddr cols))
        (draw-line pix (- size 1) 0 (- size 2) 0 :color (caddr cols)))
    pix))

(define (fvwm-hbar-tile size cols spec)
  (let ((pix (make-pixmap 1 size :background (cadr cols))))
    (if (eq? spec 'title)
        (draw-point pix 0 0 :color (car cols))
        (draw-line pix 0 0 0 1 :color (car cols)))
    (if spec
        (draw-point pix 0 (- size 1) :color (caddr cols))
        (draw-line pix 0 (- size 1) 0 (- size 2) :color (caddr cols)))
    pix))

(define (fvwm-tl-pixmap w h cols seam spec)
  (let ((pix (make-pixmap w h :background (cadr cols))))
    (draw-line pix 0 0 (- w 1) 0 :color (car cols))
    (draw-line pix 0 1 (- w 1) 1 :color (car cols))
    (draw-line pix 0 0 0 (- h 1) :color (car cols))
    (draw-line pix 1 0 1 (- h 1) :color (car cols))
    (if spec
        (draw-line pix (- h 1) (- h 1) (- w 1) (- h 1) :color (caddr cols))
        (begin
          (draw-line pix (- h 2) (- h 2) (- w 1) (- h 2) :color (caddr cols))
          (draw-line pix (- h 2) (- h 1) (- w 1) (- h 1) :color (caddr cols))))
    (if seam
        (begin
          (draw-line pix (- w 1) 0 (- w 1) (- h 2) :color (car cols))
          (draw-line pix (- w 2) 1 (- w 2) (- h 1) :color (caddr cols))))
    pix))

(define (fvwm-tr-pixmap w h cols seam spec)
  (let ((pix (make-pixmap w h :background (cadr cols))))
    (draw-line pix 0 0 (- w 1) 0 :color (car cols))
    (draw-line pix 0 1 (- w 2) 1 :color (car cols))
    (if seam (draw-line pix 1 0 1 (- h 2) :color (car cols)))
    (if (not spec)
        (draw-line pix (+ (- w h) 1) (- h 1) (+ (- w h) 1) (- h 1) :color (car cols)))
    (if seam (draw-line pix 0 1 0 (- h 1) :color (caddr cols)))
    (draw-line pix (- w 1) 1 (- w 1) (- h 1) :color (caddr cols))
    (draw-line pix (- w 2) 2 (- w 2) (- h 1) :color (caddr cols))
    (if (not spec)
        (draw-line pix 0 (- h 1) (- w h) (- h 1) :color (caddr cols))
        (if (> w h)
            (draw-line pix 0 (- h 1) (- w h 1) (- h 1) :color (caddr cols))))
    (if (not spec)
        (draw-line pix (if seam 2 0) (- h 2) (+ (- w h) 1) (- h 2) :color (caddr cols)))
    pix))

(define (fvwm-bl-pixmap w h cols seam)
  (let ((pix (make-pixmap w h :background (cadr cols))))
    (draw-line pix 0 0 0 (- h 1) :color (car cols))
    (draw-line pix 1 0 1 (- h 2) :color (car cols))
    (draw-line pix h 0 (- w 1) 0 :color (car cols))
    (draw-line pix (- h 1) 1 (- w 1) 1 :color (car cols))
    (if seam (draw-line pix (- w 1) 0 (- w 1) (- h 2) :color (car cols)))
    (draw-line pix 1 (- h 1) (- w 1) (- h 1) :color (caddr cols))
    (draw-line pix 2 (- h 2) (- w (if seam 2 1)) (- h 2) :color (caddr cols))
    (draw-line pix (- h 2) 0 (- h 1) 0 :color (caddr cols))
    (draw-line pix (- h 2) 1 (- h 2) 1 :color (caddr cols))
    (if seam (draw-line pix (- w 2) 1 (- w 2) (- h 1) :color (caddr cols)))
    pix))

(define (fvwm-br-pixmap w h cols seam)
  (let ((pix (make-pixmap w h :background (cadr cols))))
    (draw-line pix 0 0 (+ (- w h) 1) 0 :color (car cols))
    (draw-line pix 0 1 (+ (- w h) 1) 1 :color (car cols))
    (if seam (draw-line pix 1 0 1 (- h 2) :color (car cols)))
    (draw-line pix 0 (- h 1) (- w 1) (- h 1) :color (caddr cols))
    (draw-line pix (if seam 2 0) (- h 2) (- w 1) (- h 2) :color (caddr cols))
    (draw-line pix (- w 1)  0 (- w 1) (- h 1) :color (caddr cols))
    (draw-line pix (- w 2)  0 (- w 2) (- h 2) :color (caddr cols))
    (if seam (draw-line pix 0 1 0 (- h 1) :color (caddr cols)))
    pix))

(define (fvwm-bv-pixmap w h cols)
  (let ((pix (make-pixmap w h :background (cadr cols))))
    (draw-line pix 0 1 (- w 1) 1 :color (car cols))
    (draw-line pix 0 0 0 (- h 1) :color (car cols))
    (draw-line pix 1 1 1 (- h 1) :color (car cols))
    (draw-line pix 1 0 (- w 1) 0 :color (caddr cols))
    (draw-line pix (- w 1) 0 (- w 1) (- h 1) :color (caddr cols))
    (draw-line pix (- w 2) 2 (- w 2) (- h 1) :color (caddr cols))
    pix))

(define (fvwm-tv-pixmap w h cols spec)
  (let ((pix (make-pixmap w h :background (cadr cols))))
    (draw-line pix 0 0 0 (- h 1) :color (car cols))
    (if (not (eq? spec 'right))
        (draw-line pix 1 0 1 (- h 1) :color (car cols)))
    (draw-line pix 0 (- h 1) (- w 2) (- h 1) :color (car cols))
    (if (not (eq? spec 'left))
        (draw-line pix (- w 2) 0 (- w 2) (- h 2) :color (caddr cols)))
    (draw-line pix (- w 1) 0 (- w 1) (- h 1) :color (caddr cols))
    (draw-line pix 1 (- h 2) (- w 1) (- h 2) :color (caddr cols))
    pix))


; Frame Behavior
; ===============

(define fvwm-frame-behavior
  (make-behavior
   (on (user-event 'focus-in)
       (set-deco-background! deco (get-property deco 'activepixmap)))
   (on (user-event 'focus-out)
       (set-deco-background! deco (get-property deco 'pixmap)))
   (on (user-event 'pre-color)
       (let* ((redo-expr (get-property deco 'redo-expr))
              (acols (caddr (event-data event)))
              (ncols (cadr (event-data event)))
              (pix (redo-expr ncols))
              (apix (redo-expr acols)))
         (set-property! deco 'new-npm pix)
         (set-property! deco 'new-apm apix)))
   (on (user-event 're-color)
       (let ((pix (get-property deco 'new-npm))
             (apix (get-property deco 'new-apm)))
         (if (get-property (top-deco deco) 'active)
             (set-deco-background! deco apix)
             (set-deco-background! deco pix))
         (set-property! deco 'pixmap pix)
         (set-property! deco 'activepixmap apix)))))


; Title Behavior
; ===============

(define fvwm-title-behavior
  (make-behavior
   (on (user-event 'focus-in)
       (set-deco-background! deco (get-property deco 'activepixmap)))
   (on (user-event 'focus-out)
       (set-deco-background! deco (get-property deco 'pixmap)))
   (on (user-event 'press)
       (set-deco-background! deco (if (get-property (top-deco deco) 'active)
                                      (get-property deco 'pressedactivepixmap)
                                      (get-property deco 'pressedpixmap))))
   (on (user-event 'release)
       (set-deco-background! deco (if (get-property (top-deco deco) 'active)
                                      (get-property deco 'activepixmap)
                                      (get-property deco 'pixmap))))
   (on (user-event 'pre-color)
       (let* ((redo-expr (get-property deco 'redo-expr))
              (acols (caddr (event-data event)))
              (ncols (cadr (event-data event)))
              (ntile (redo-expr ncols #f))
              (atile (redo-expr acols #f))
              (ptile (redo-expr ncols #t))
              (patile (redo-expr acols #t)))
         (set-property! deco 'new-npm ntile)
         (set-property! deco 'new-apm atile)
         (set-property! deco 'new-ppm ptile)
         (set-property! deco 'new-papm patile)))
   (on (user-event 're-color)
       (let ((ntile (get-property deco 'new-npm))
             (atile (get-property deco 'new-apm))
             (ptile (get-property deco 'new-ppm))
             (patile (get-property deco 'new-papm)))
         (if (get-property (top-deco deco) 'active)
             (if (eq? (deco-background deco) (get-property deco 'pressedactivepixmap))
                 (set-deco-background! deco patile)
                 (set-deco-background! deco atile))
             (if (eq? (deco-background deco) (get-property deco 'pressedpixmap))
                 (set-deco-background! deco ptile)
                 (set-deco-background! deco ntile)))
         (set-property! deco 'pixmap ntile)
         (set-property! deco 'activepixmap atile)
         (set-property! deco 'pressedpixmap ptile)
         (set-property! deco 'pressedactivepixmap patile)))))

(define fvwm-title-label-behavior
  (make-behavior
   (on (user-event 'focus-in)
       (set-deco-background! deco (get-property deco 'activepixmap)))
   (on (user-event 'focus-out)
       (set-deco-background! deco (get-property deco 'pixmap)))
   (on (user-event 'pre-color)
       (let* ((redo-expr (get-property deco 'redo-expr))
              (acols (caddr (event-data event)))
              (ncols (cadr (event-data event)))
              (pix (redo-expr ncols))
              (apix (redo-expr acols)))
         (set-property! deco 'new-npm pix)
         (set-property! deco 'new-apm apix)
         (set-property! deco 'ncols ncols)
         (set-property! deco 'acols acols)))
   (on (user-event 're-color)
       (let ((pix (get-property deco 'new-npm))
             (apix (get-property deco 'new-apm)))
         (if (get-property (top-deco deco) 'active)
             (set-deco-background! deco apix)
             (set-deco-background! deco pix))
         (set-property! deco 'pixmap pix)
         (set-property! deco 'activepixmap apix)))
   (on (user-event 'name-change)
       (let* ((redo-expr (get-property deco 'redo-expr))
              (acols (get-property deco 'acols))
              (ncols (get-property deco 'ncols))
              (pix (redo-expr ncols))
              (apix (redo-expr acols)))
         (if (get-property (top-deco deco) 'active)
             (modify-deco deco :background apix :width (width apix) :height (height apix))
             (modify-deco deco :background pix :width (width pix) :height (height pix)))
         (set-property! deco 'pixmap pix)
         (set-property! deco 'activepixmap apix)))))


; THE PLUGS :
; =========

(defmacro fvwm-border-plug (action pix-expr)
  `(let* ((proc (lambda (cols) 
                  ,pix-expr))
          (pix (proc ncols))
          (apix (proc acols))
          (props `((pixmap . ,pix) (activepixmap . ,apix) (redo-expr . ,proc))))
     (make-deco '()
                :width (width pix)
                :height (height pix)
                :background pix 
                :behavior (make-behavior 
                           fvwm-frame-behavior 
                           (fvwm-button-behavior ,action #f))
                :property props)))
     
(defmacro fvwm-resize-plug (cur action pix-expr)
  `(let* ((proc (lambda (cols)
                  ,pix-expr))
          (pix (proc ncols))
          (apix (proc acols))
          (props `((pixmap . ,pix) (activepixmap . ,apix) (redo-expr . ,proc))))
     (make-deco '()
                :width (width pix)
                :height (height pix)
                :background pix 
                :cursor (if fvwm-resize-cursors ,cur fvwm-frame-cursor)
                :behavior (make-behavior 
                           fvwm-frame-behavior 
                           (fvwm-button-behavior ,action #f))
                :property props)))

(defmacro fvwm-title-plug (pix-expr)
  `(let* ((proc (lambda (cols pressed) 
                  ,pix-expr))
          (npix (proc ncols #f))
          (apix (proc acols #f))
          (ppix (proc ncols #t))
          (papix (proc acols #t))
          (props `((pixmap . ,npix) (activepixmap . ,apix)
                   (pressedpixmap . ,ppix) (pressedactivepixmap . ,papix)
                   (redo-expr . ,proc))))
     (make-deco '()
                :width (width npix)
                :height (height npix)
                :background npix 
                :behavior fvwm-title-behavior
                :property props)))
     
(defmacro fvwm-button-plug (action atype pix-expr)
  `(let* ((proc (lambda (cols pressed) 
                  ,pix-expr))
          (npix (proc ncols #f))
          (apix (proc acols #f))
          (ppix (proc ncols #t))
          (papix (proc acols #t))
          (props `((pixmap . ,npix) (activepixmap . ,apix)
                   (pressedpixmap . ,ppix) (pressedactivepixmap . ,papix)
                   (redo-expr . ,proc))))
     (make-deco '()
                :width (width npix)
                :height (height npix)
                :background npix 
                :behavior (make-behavior 
                           fvwm-title-behavior
                           (fvwm-button-behavior ,action ,atype))
                :property props)))
     
(defmacro fvwm-label-plug (pix-expr)
  `(let* ((proc (lambda (cols) 
                  ,pix-expr))
          (pix (proc ncols))
          (apix (proc acols))
          (props `((pixmap . ,pix) (activepixmap . ,apix)
                   (ncols . ,ncols) (acols . ,acols) (redo-expr . ,proc))))
     (make-deco '()
                :width (width pix)
                :height (height pix)
                :background pix 
                :behavior fvwm-title-label-behavior
                :property props)))

(define (fvwm-border-plug-tl size ncols acols action small)
  (fvwm-border-plug action (fvwm-tl-pixmap size (if small (- size 1) size) cols #f small)))

(define (fvwm-border-plug-tr size ncols acols action small)
  (fvwm-border-plug action (fvwm-tr-pixmap size (if small (- size 1) size) cols #f small)))

(define (fvwm-border-plug-bl size ncols acols action)
  (fvwm-border-plug action (fvwm-bl-pixmap size size cols #f)))

(define (fvwm-border-plug-br size ncols acols action)
   (fvwm-border-plug action (fvwm-br-pixmap size size cols #f)))

(define (fvwm-resize-vplug-tl cornsize size ncols acols action)
   (fvwm-resize-plug cursor-NW action (fvwm-tv-pixmap size (- cornsize size) cols #f)))

(define (fvwm-resize-vplug-tr cornsize size ncols acols action)
   (fvwm-resize-plug cursor-NE action (fvwm-tv-pixmap size (- cornsize size) cols #f)))

(define (fvwm-resize-vplug-tl-small cornsize size tsize ncols acols action wide)
  (fvwm-resize-plug cursor-NW action 
                    (fvwm-tv-pixmap size (- cornsize size tsize (if wide 2 0))
                                    cols #f)))

(define (fvwm-resize-vplug-tr-small cornsize size ncols acols action wide)
  (fvwm-resize-plug cursor-NE action 
                    (fvwm-tv-pixmap size (- cornsize size tsize (if wide 2 0))
                                    cols #f)))

(define (fvwm-resize-vplug-bl cornsize size ncols acols action)
   (fvwm-resize-plug cursor-SW action (fvwm-bv-pixmap size (- cornsize size) cols)))

(define (fvwm-resize-vplug-br cornsize size ncols acols action)
   (fvwm-resize-plug cursor-SE action (fvwm-bv-pixmap size (- cornsize size) cols)))

(define (fvwm-resize-hplug-bl cornsize size ncols acols action)
   (fvwm-resize-plug cursor-SW action (fvwm-bl-pixmap cornsize size cols #t)))

(define (fvwm-resize-hplug-br cornsize size ncols acols action)
   (fvwm-resize-plug cursor-SE action (fvwm-br-pixmap cornsize size cols #t)))

(define (fvwm-resize-hplug-tl cornsize size ncols acols action)
   (fvwm-resize-plug cursor-NW action (fvwm-tl-pixmap cornsize size cols #t #f)))

(define (fvwm-resize-hplug-tr cornsize size ncols acols action)
   (fvwm-resize-plug cursor-NE action (fvwm-tr-pixmap cornsize size cols #t #f)))

(define (fvwm-resize-hplug-tl2 cornsize size ncols acols action)
   (fvwm-resize-plug cursor-NW action (fvwm-tl-pixmap cornsize (- size 1) cols #t #t)))

(define (fvwm-resize-hplug-tr2 cornsize size ncols acols action)
   (fvwm-resize-plug cursor-NE action (fvwm-tr-pixmap cornsize (- size 1) cols #t #t)))


; THE BARS :
; ========


(defmacro fvwm-border-bar (action pix-expr . bar-expr)
  `(let* ((proc (lambda (cols) 
                  ,pix-expr))
          (pix (proc ncols))
          (apix (proc acols))
          (props `((pixmap . ,pix) (activepixmap . ,apix) (redo-expr . ,proc))))
     (make-deco ,@bar-expr
                :cursor fvwm-frame-cursor
                :background pix
                :behavior (make-behavior 
                           fvwm-frame-behavior 
                           (fvwm-button-behavior ,action #f))
                :property props)))

(defmacro fvwm-resize-bar (cur action pix-expr . bar-expr)
  `(let* ((proc (lambda (cols) 
                  ,pix-expr))
          (pix (proc ncols))
          (apix (proc acols))
          (props `((pixmap . ,pix) (activepixmap . ,apix) (redo-expr . ,proc))))
     (make-deco ,@bar-expr
                :background pix
                :cursor (if fvwm-resize-cursors ,cur fvwm-frame-cursor)
                :behavior (make-behavior 
                           fvwm-frame-behavior 
                           (fvwm-button-behavior ,action #f))
                :property props)))

(defmacro fvwm-title-bar (action atype pix-expr . bar-expr)
  `(let* ((proc (lambda (cols pressed) 
                  ,pix-expr))
          (npix (proc ncols #f))
          (apix (proc acols #f))
          (ppix (proc ncols #t))
          (papix (proc acols #t))
          (props `((pixmap . ,npix) (activepixmap . ,apix)
                   (pressedpixmap . ,ppix) (pressedactivepixmap . ,papix)
                   (redo-expr . ,proc))))
     (make-deco ,@bar-expr
                :direction 'horizontal
                :background npix
                :behavior (make-behavior 
                           fvwm-title-behavior 
                           (fvwm-button-behavior ,action ,atype))
                :property props)))

(defmacro fvwm-titlerow-bar (hgt action pix-expr . bar-expr)
  `(let* ((proc (lambda (cols) 
                  ,pix-expr))
          (pix (proc ncols))
          (apix (proc acols))
          (props `((pixmap . ,pix) (activepixmap . ,apix) (redo-expr . ,proc))))
     (apply make-deco (append ,@bar-expr
                              (list :direction 'horizontal
                                    :height ,hgt
                                    :cursor fvwm-frame-cursor
                                    :background pix
                                    :behavior (make-behavior 
                                               fvwm-frame-behavior 
                                               (fvwm-button-behavior ,action #f))
                                    :property props)))))

(define (fvwm-resize-vplug-tl2 cornsize size ncols acols saction caction wide)
  (fvwm-resize-bar cursor-W saction
                   (if wide
                       (fvwm-vbar-tile size cols #f)
                       (fvwm-vbar-tile (- size 1) cols 'left))
                   (if wide
                       (fvwm-resize-plug cursor-NW caction 
                                         (fvwm-tv-pixmap size (- cornsize size)
                                                         cols #f))
                       (fvwm-resize-plug cursor-NW caction 
                                         (fvwm-tv-pixmap (- size 1)
                                                         (- cornsize size -1)
                                                         cols 'left)))))

(define (fvwm-resize-vplug-tr2 cornsize size ncols acols saction caction wide)
  (fvwm-resize-bar cursor-E saction
                   (if wide
                       (fvwm-vbar-tile size cols #f)
                       (fvwm-vbar-tile (- size 1) cols 'right))
                   (if wide
                       (fvwm-resize-plug cursor-NE caction 
                                         (fvwm-tv-pixmap size (- cornsize size)
                                                         cols #f))
                       (fvwm-resize-plug cursor-NE caction 
                                         (fvwm-tv-pixmap (- size 1)
                                                         (- cornsize size -1)
                                                         cols 'right)))))

(define (fvwm-border-vertical-bar size ncols acols action dir)
  (fvwm-border-bar action
                   (if dir
                       (fvwm-vbar-tile (- size 1) cols dir)
                       (fvwm-vbar-tile size cols #f))
                   '()
                   :width (if dir (- size 1) size)))

(define (fvwm-border-top-bar size ncols acols action small)
  (fvwm-border-bar action
                   (if small
                       (fvwm-hbar-tile (- size 1) cols #t)
                       (fvwm-hbar-tile size cols #f))
                   (fvwm-border-plug-tl size ncols acols action small)
                   '()
                   (fvwm-border-plug-tr size ncols acols action small)))

(define (fvwm-border-bottom-bar size ncols acols action)
  (fvwm-border-bar action
                   (fvwm-hbar-tile size cols #f)
                   (fvwm-border-plug-bl size ncols acols action)
                   '()
                   (fvwm-border-plug-br size ncols acols action)))
	 
(define (fvwm-resize-left-bar cornsize size tsize ncols acols saction caction title wide)
  (fvwm-resize-bar cursor-W saction
                   (fvwm-vbar-tile size cols #f)
                   (if title
                       (if (> cornsize (+ tsize size (if wide 2 0)))
                           (fvwm-resize-vplug-tl-small cornsize size tsize ncols acols caction wide) 
                           '())
                       (fvwm-resize-vplug-tl cornsize size ncols acols caction))
                   '()
                   (fvwm-resize-vplug-bl cornsize size ncols acols caction)))

(define (fvwm-resize-right-bar cornsize size tsize ncols acols saction caction title wide)
  (fvwm-resize-bar cursor-E saction
                   (fvwm-vbar-tile size cols #f)
                   (if title
                       (if (> cornsize (+ tsize size (if wide 2 0)))
                           (fvwm-resize-vplug-tr-small cornsize size tsize ncols acols caction wide) 
                           '())
                       (fvwm-resize-vplug-tr cornsize size ncols acols caction))
                   '()
                   (fvwm-resize-vplug-br cornsize size ncols acols caction)))

(define (fvwm-resize-bottom-bar cornsize size ncols acols saction caction)
  (fvwm-resize-bar cursor-S saction
                   (fvwm-hbar-tile size cols #f)
                   (fvwm-resize-hplug-bl cornsize size ncols acols caction) 
                   '() 
                   (fvwm-resize-hplug-br cornsize size ncols acols caction)))

(define (fvwm-resize-top-bar cornsize size ncols acols saction caction small)
  (fvwm-resize-bar cursor-N saction
                   (if small
                       (fvwm-hbar-tile (- size 1) cols #t)
                       (fvwm-hbar-tile size cols #f))
                   (if small
                       (fvwm-resize-hplug-tl2 cornsize size ncols acols caction)
                       (fvwm-resize-hplug-tl cornsize size ncols acols caction))
                   '()
                   (if small
                       (fvwm-resize-hplug-tr2 cornsize size ncols acols caction)
                       (fvwm-resize-hplug-tr cornsize size ncols acols caction))))


; THE TITLEBAR BUTTONS :
; ====================

(define (small-square pixmap size light dark)
  (let* ((x1 (quotient (+ size 2) 3))
         (x2 (- size x1 1)))
    (draw-line pixmap x1 x1 x1 x2 :color light)
    (draw-line pixmap x1 x1 x2 x1 :color light)
    (draw-line pixmap x1 x2 x2 x2 :color dark)
    (draw-line pixmap x2 x1 x2 x2 :color dark)
    pixmap))
         
(define (medium-square pixmap size light dark)
  (let* ((x1 (quotient (+ size 4) 4))
         (x2 (- size x1 1)))
    (draw-line pixmap x1 x1 x1 x2 :color light)
    (draw-line pixmap x1 x1 x2 x1 :color light)
    (draw-line pixmap x1 x2 x2 x2 :color dark)
    (draw-line pixmap x2 x1 x2 x2 :color dark)
    pixmap))

(define (large-square pixmap size light dark)
  (let* ((x1 (quotient (+ size 6) 6))
         (x2 (- size x1 1)))
    (draw-line pixmap x1 x1 x1 x2 :color light)
    (draw-line pixmap x1 x1 x2 x1 :color light)
    (draw-line pixmap x1 x2 x2 x2 :color dark)
    (draw-line pixmap x2 x1 x2 x2 :color dark)
    pixmap))

(define (horizontal-rectangle pixmap size light dark)
  (let* ((x1 (quotient (+ size 6) 6))
         (x2 (- size x1 1))
         (y1 (quotient (+ size 2) 3))
         (y2 (- size y1 1)))
    (draw-line pixmap x1 y1 x1 y2 :color light)
    (draw-line pixmap x1 y1 x2 y1 :color light)
    (draw-line pixmap x1 y2 x2 y2 :color dark)
    (draw-line pixmap x2 y1 x2 y2 :color dark)
    pixmap))

(define (vertical-rectangle pixmap size light dark)
  (let* ((y1 (quotient (+ size 6) 6))
         (y2 (- size y1 1))
         (x1 (quotient (+ size 2) 3))
         (x2 (- size x1 1)))
    (draw-line pixmap x1 y1 x1 y2 :color light)
    (draw-line pixmap x1 y1 x2 y1 :color light)
    (draw-line pixmap x1 y2 x2 y2 :color dark)
    (draw-line pixmap x2 y1 x2 y2 :color dark)
    pixmap))

(define (medium-circle pixmap size light dark)
  (let* ((x1 (quotient (+ size 6) 5))
         (x2 (- size x1 1))
         (o1 (quotient (* (+ size 2) 3) 8))
         (o2 (- size o1 1)))
    (draw-line pixmap o1 x1 o2 x1 :color light)
    (draw-line pixmap o1 x1 x1 o1 :color light)
    (draw-line pixmap x1 o1 x1 o2 :color light)
    (draw-line pixmap x2 o1 x2 o2 :color dark)
    (draw-line pixmap x2 o2 o2 x2 :color dark)
    (draw-line pixmap o1 x2 o2 x2 :color dark)
    pixmap))

(define (medium-diamond pixmap size light dark)
  (let* ((x1 (quotient (+ size 6) 6))
         (x2 (- size x1 (remainder size 2)))
         (h (quotient size 2)))
    (draw-line pixmap h x1 x2 h :color dark)
    (draw-line pixmap x2 h h x2 :color dark)
    (draw-line pixmap h x1 x1 h :color light)
    (draw-line pixmap x1 h h x2 :color light)
    pixmap))

(define (down-triangle pixmap size light dark)
  (let* ((x1 (quotient (+ size 4) 4))
         (x2 (- size x1 (remainder size 2) -1))
         (h (quotient size 2)))
    (draw-line pixmap x1 x1 x2 x1 :color light)
    (draw-line pixmap x1 x1 h x2 :color light)
    (draw-line pixmap x2 x1 h x2 :color dark)
    pixmap))

(define (up-triangle pixmap size light dark)
  (let* ((x1 (quotient (- size 4) 4))
         (x2 (- size x1 (remainder size 2) 2))
         (h (quotient (- size 2) 2)))
    (draw-line pixmap x1 x2 x2 x2 :color dark)
    (draw-line pixmap x2 x2 h x1 :color dark)
    (draw-line pixmap x1 x2 h x1 :color light)
    pixmap))

(define (fvwm-button-pixmap size cols pressed draw-func)
  (let* ((pixmap (make-pixmap size size :background (cadr cols)))
         (last (- size 1))
         (light (car cols))
         (dark (caddr cols)))
    (draw-line pixmap 0 0 last 0 :color (if pressed dark light))
    (draw-line pixmap 0 1 0 last :color (if pressed dark light))
    (draw-line pixmap last 0 last last :color (if pressed light dark))
    (draw-line pixmap 1 last last last :color (if pressed light dark))
    (draw-func pixmap size light dark)
    pixmap))

(define (fvwm-interpret-event event)
  (let ((ecode (cond ((list? event)
                      (car event))
                     ((eq? event #t)
                      any)
                     (#t event)))
        (emod (if (list? event)
                  (apply logior (cdr event))
                  any)))
    (cond ((string? ecode)
           (key ecode emod))
          ((number? ecode)
           (button ecode emod))
          ((event? ecode)
           ecode)
          (#t #f))))

(define fvwm-button-event-code #f)

(define (fvwm-button-action-arc evdesc action atype)
  (make-behavior
   (if (eq? atype 'release)
       (on-event (fvwm-interpret-event evdesc)
                 (lambda (w e)
                   (send-user-event 'press w))
                 (lambda (w e)
                   (send-user-event 'release w)
                   (action (top-deco w) e)))
       (on-event (fvwm-interpret-event evdesc)
                 (lambda (w e)
                   (send-user-event 'press w)
                   (action (top-deco w) e))
                 (lambda (w e)
                   (send-user-event 'release w))))
   ))

(define (fvwm-button-behavior action atype)
  (cond ((not action)
         (make-behavior))
        ((behavior? action)
         action)
        ((list? action)
         (apply make-behavior 
                (map (lambda (ele) 
                       (fvwm-button-action-arc (car ele) (cadr ele)
                                               (if (pair? (cddr ele)) (caddr ele) #f)))
                     action)))
        ((procedure? action)
         (fvwm-button-action-arc #t action atype))
        (#t (make-behavior))))
        
(define (fvwm-construct-button-plug size ncols acols ele)
  (let ((draw (car ele))
        (action (cadr ele))
        (atype (if (pair? (cddr ele)) (caddr ele) #f)))
    (fvwm-button-plug action atype
                      (fvwm-button-pixmap size cols pressed draw))))

; THE TITLE-BAR :
; =============

(define (fvwm-title-label name-expr title-font ncols acols)
  (let ((font (if (string? title-font)
                  (make-font title-font)
                  title-font)))
    (fvwm-label-plug (make-label (name-expr)
                                 :font font
                                 :background (cadr cols)
                                 :foreground (cadddr cols)))))

(define (fvwm-title-seam col size)
  (make-pixmap 1 size :background col))
         
(define (fvwm-inner-titlebar win tfont tsize ncols acols action)
  (fvwm-title-bar action 'press
                  (if pressed 
                      (fvwm-hbar-tile tsize (list (caddr cols)
                                                  (cadr cols)
                                                  (car cols)) 'title)
                      (fvwm-hbar-tile tsize cols 'title))
                  (fvwm-title-plug (fvwm-title-seam (if pressed
                                                        (caddr cols)
                                                        (car cols)) tsize))
                  '()
                  (fvwm-title-label (lambda () (window-name win))
                                    tfont ncols acols)
                  '()
                  (fvwm-title-plug (fvwm-title-seam (if pressed
                                                        (car cols)
                                                        (caddr cols)) tsize))))


(define (fvwm-title-row win tfont tsize ncols acols action seams left-buttons right-buttons)
  (fvwm-titlerow-bar (if seams (+ tsize 2) (+ tsize 1))
                     action
                     (fvwm-hbar-tile (+ tsize 2) cols #f)
                     (if seams
                         (list (fvwm-border-plug action
                                                 (fvwm-title-seam (car cols)
                                                                  (+ tsize 2))))
                         '())
                     (map (lambda (ele)
                            (fvwm-construct-button-plug tsize ncols acols ele))
                          left-buttons)
                     (list (fvwm-inner-titlebar win tfont tsize ncols acols action))
                     (map (lambda (ele)
                            (fvwm-construct-button-plug tsize ncols acols ele))
                          right-buttons)
                     (if seams
                         (list (fvwm-border-plug action
                                                 (fvwm-title-seam (caddr cols)
                                                                  (+ tsize 2))))
                         '())))
                   

; INTERNAL BEHAVIOR OF WINDOWS :
; ============================

(define fvwm-window-behavior
   (make-behavior
        (on (user-event 'focus-out)
            (fvwm-maintain-focus-out deco))
        (on (user-event 'focus-in)
            (fvwm-maintain-focus-in deco))
        (on (user-event 'nail)
            (fvwm-update-color deco))
        (on (user-event 'unnail)
            (fvwm-update-color deco))
))

(define (fvwm-maintain-focus-in win)
  (set-property! win 'active #t)
  (if (and fvwm-virtual-colors
           (not (eq? (get-property win 'virt-col1) (get-property win 'virt-col2))))
      (begin
        (set-property! win 'virt-col (get-property win 'virt-col2))
        (virtual-update))))

(define (fvwm-maintain-focus-out win)
  (set-property! win 'active #f)
  (if (and fvwm-virtual-colors
           (not (eq? (get-property win 'virt-col1) (get-property win 'virt-col2))))
      (begin
        (set-property! win 'virt-col (get-property win 'virt-col1))
        (virtual-update))))

(define (fvwm-get-colors styles props)
  (let* ((col (or (get-keyword :color props #f)
                  (and (pair? styles) (car styles))
                  fvwm-color))
         (acol (or (get-keyword :active-color props
                                (or (and (pair? styles)
                                         (pair? (cdr styles))
                                         (cadr styles))
                                    fvwm-active-color))
                   col))
         (tcol (or (get-keyword :title-color props #f)
                   (and (pair? styles) (pair? (cdr styles)) (pair? (cddr styles)) (caddr styles))
                   fvwm-title-color "black"))
         (atcol (or (get-keyword :active-title-color props 
                                 (or (and (pair? styles) (pair? (cdr styles)) (pair? (cddr styles)) (pair? (cdddr styles)) (cadddr styles))
                                     fvwm-active-title-color))
                    tcol)))
    (list col acol tcol atcol)))

(define (fvwm-resize-window win ev)
  (user-resize-window win ev
                      :corner-size (or (get-property win 'resize-corner-size) 1)))

(define (fvwm-update-color win)
  (let* ((props (matches-cond-all win fvwm-window-styles))
         (cols (fvwm-get-colors (matches-cond win fvwm-color-styles) props))
         (corner-size (get-property win 'resize-corner-size))
         (frame-width (get-property win 'frame-width))
         (title-height (get-property win 'title-height))
         (is-active (get-property win 'active))
         (color-scheme (make-color-scheme (car cols) (caddr cols)))
         (active-color-scheme (make-color-scheme (cadr cols) (cadddr cols))))
    (send-user-event (list 'pre-color color-scheme active-color-scheme) win)
    (send-user-event 're-color win)
    (if (icon-decorated? win)
        (send-user-event 'update-color (icon-deco win)))
    (set-property! win 'virt-col (if fvwm-virtual-colors
                                     (car (if is-active active-color-scheme color-scheme))
                                     #f))
    (set-property! win 'virt-col1 (car color-scheme))
    (set-property! win 'virt-col2 (car active-color-scheme))
    (if fvwm-virtual-colors
        (virtual-update))))


(define (fvwm-window win)
  (let ((props (matches-cond-all win fvwm-window-styles)))
  (let ((cols (fvwm-get-colors (matches-cond win fvwm-color-styles) props))
        (has-frame (get-keyword :frame props fvwm-has-frame))
        (has-resize (get-keyword :resize props fvwm-has-resize))
        (has-title (get-keyword :title props fvwm-has-title))
        (wide-shadow (get-keyword :wide-shadow props fvwm-wide-shadow))
        (title-font (get-keyword :title-font props fvwm-title-font))
        (frame-width (get-keyword :frame-width props fvwm-frame-width))
        (left-buttons (get-keyword :left-buttons props fvwm-left-buttons))
        (right-buttons (get-keyword :right-buttons props fvwm-right-buttons))
        (border-action (get-keyword :border-action props fvwm-border-action))
        (side-action (get-keyword :side-action props fvwm-side-action))
        (corner-action (get-keyword :corner-action props fvwm-corner-action))
        (title-action (get-keyword :title-action props fvwm-title-action))
        (inner-border (get-keyword :inner-border props fvwm-inner-border))
        (outer-border (get-keyword :outer-border props fvwm-outer-border)))
    (let* ((fonthgt (cadr (string-dimensions "" (if (string? title-font)
                                                    (make-font title-font)
                                                    title-font))))
           (title-height (or (get-keyword :title-height props fvwm-title-height)
                            (+ 4 fonthgt)))
           (corner-size (max (or (get-keyword :corner-size props
                                              fvwm-corner-size)
                                 (+ title-height frame-width (if wide-shadow 2 0)))
                             (+ frame-width 2)))
           (color-scheme (make-color-scheme (car cols) (caddr cols)))
           (active-color-scheme (make-color-scheme (cadr cols) (cadddr cols)))
           (inner-borderwidth (if inner-border
                                  (if (number? inner-border)
                                      inner-border -1)
                                  0))
           (borderwidth (if outer-border
                            (if (number? outer-border)
                                outer-border 1)
                            0))
           (bordercolor (make-color "black"))
           (cursor fvwm-frame-cursor)
           (behavior (make-behavior window-behavior
                                    fvwm-window-behavior
                                    std-window-behavior))
           (property `((resize-corner-size . ,(if has-resize corner-size 1))
                       (frame-width . ,frame-width)
                       (title-height . ,title-height)
                       (virt-col . ,(if fvwm-virtual-colors (car color-scheme) #f))
                       (virt-col1 . ,(car color-scheme))
                       (virt-col2 . ,(car active-color-scheme))
                       (active . ,#f)))
           (context (list :borderwidth borderwidth
                          :bordercolor bordercolor
                          :cursor cursor
                          :behavior behavior
                          :property property)))
      (let ((top (if has-frame
                     (if has-resize
                         (fvwm-resize-top-bar corner-size frame-width
                                              color-scheme active-color-scheme
                                              side-action corner-action
                                              (and (not wide-shadow) has-title))
                         (fvwm-border-top-bar frame-width
                                              color-scheme active-color-scheme
                                              border-action
                                              (and (not wide-shadow) has-title)))))
            (left (if has-frame
                      (if has-resize
                          (fvwm-resize-left-bar corner-size frame-width
                                                title-height
                                                color-scheme active-color-scheme
                                                side-action corner-action
                                                has-title wide-shadow)
                          (fvwm-border-vertical-bar frame-width
                                                    color-scheme active-color-scheme
                                                    border-action #f))))
            (right (if has-frame
                       (if has-resize
                           (fvwm-resize-right-bar corner-size frame-width
                                                  title-height
                                                  color-scheme active-color-scheme
                                                  side-action corner-action
                                                  has-title wide-shadow)
                           (fvwm-border-vertical-bar frame-width
                                                     color-scheme active-color-scheme
                                                     border-action #f))))
            (bottom (if has-frame
                        (if has-resize
                            (fvwm-resize-bottom-bar corner-size frame-width
                                                    color-scheme active-color-scheme
                                                    side-action corner-action)
                            (fvwm-border-bottom-bar frame-width
                                                    color-scheme active-color-scheme
                                                    border-action))))
            (titleleft (if (and has-frame has-title)
                           (if has-resize
                               (fvwm-resize-vplug-tl2 corner-size frame-width
                                                      color-scheme active-color-scheme
                                                      side-action corner-action
                                                      wide-shadow)
                               (fvwm-border-vertical-bar frame-width
                                                         color-scheme active-color-scheme
                                                         border-action
                                                         (if (not wide-shadow) 'left #f)))))
            (titleright (if (and has-frame has-title)
                            (if has-resize
                                (fvwm-resize-vplug-tr2 corner-size frame-width
                                                      color-scheme active-color-scheme
                                                      side-action corner-action
                                                      wide-shadow)
                                (fvwm-border-vertical-bar frame-width
                                                          color-scheme active-color-scheme
                                                          border-action 
                                                          (if (not wide-shadow) 'right #f)))))
            (title (if has-title
                       (fvwm-title-row win title-font title-height
                                       color-scheme active-color-scheme
                                       title-action
                                       (or (not has-frame) wide-shadow)
                                       left-buttons right-buttons))))
        (if (not (= inner-borderwidth -1))
            (set-deco-borderwidth! win inner-borderwidth))
        (if has-frame
            (if has-title
                (make-deco top
                           (make-deco titleleft title titleright)
                           (make-deco left win right)
                           bottom
                           :context context)
                (make-deco top
                           (make-deco left win right)
                           bottom
                           :context context))
            (if has-title
                (make-deco title
                           win
                           :context context)
                (make-deco win :context context))))))))
