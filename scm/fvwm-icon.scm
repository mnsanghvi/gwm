;; fvwm-icon.scm --- Fvwm style icons.
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Fvwm style icons. Do '(set-icon #t fvwm-icon)' to use them.
;; See also fvwm-window.scm.
;;

;;    User customizable variables. Adjust these in your own profile.
(defvar fvwm-icon-action (lambda (w e) (iconify-move-window w e))
  "Action to execute when pressing the icon title." 'procedure)
       ; Can also be a list of ( <button-spec> <action>
       ; [<action-type>] ) specifications.
(defvar fvwm-pixmap-path '("/usr/X11R6/include/X11/pixmaps/")
  "Where it is likely to find some good icon pixmaps." 'list)
(defvar fvwm-icon-assoc-list '()
  "Mapping between window-client-class and icon file name when the simple window-client-name heuristic does not work."
  'list)

       ; Also, the variable 'fvwm-window-styles' from "fvwm-window.gwm"
       ; can contain these additional properties: icon-color,
       ; active-icon-color, icon-title-color, active-icon-title-color,
       ; icon-action, icon-outer-border.

(require 'fvwm-window)

; THE ICON :
; ==========

(define (fvwm-get-icon-colors styles props)
  (let* ((col (or (get-keyword :icon-color props
                               (get-keyword :color props #f))
                  (and (pair? styles) (car styles))
                  fvwm-color))
         (acol (or (get-keyword :active-icon-color props
                                (get-keyword :active-color props
                                             (or (and (pair? styles)
                                                      (pair? (cdr styles))
                                                      (cadr styles))
                                                 fvwm-active-color)))
                   col))
         (tcol (or (get-keyword :icon-title-color props
                                (get-keyword :title-color props #f))
                   (and (pair? styles) (pair? (cdr styles)) (pair? (cddr styles)) (caddr styles))
                   fvwm-title-color "black"))
         (atcol (or (get-keyword :active-icon-title-color props 
                                 (get-keyword :active-title-color props 
                                              (or (and (pair? styles) (pair? (cdr styles)) (pair? (cddr styles)) (pair? (cdddr styles)) (cadddr styles))
                                                  fvwm-active-title-color)))
                    tcol)))
    (list col acol tcol atcol)))

(define (fvwm-inner-iconbar win tfont tsize ncols acols action)
  (fvwm-title-bar action 'press
                  (if pressed 
                      (fvwm-hbar-tile tsize (list (caddr cols)
                                                  (cadr cols)
                                                  (car cols)) 'title)
                      (fvwm-hbar-tile tsize cols 'title))
                  (fvwm-title-plug (fvwm-title-seam (if pressed
                                                        (caddr cols)
                                                        (car cols)) tsize))
                  (fvwm-title-label (lambda ()
                                      (if (string=? (window-icon-name win) "icon")
                                          (window-name win)
                                          (window-icon-name win)))
                                    tfont ncols acols)
                  (fvwm-title-plug (fvwm-title-seam (if pressed
                                                        (car cols)
                                                        (caddr cols)) tsize))
                  :separator 4))

(define (fvwm-iconbar win tfont tsize ncols acols action outer-border)
  (fvwm-titlerow-bar (+ tsize 2)
                     action
                     (fvwm-hbar-tile (+ tsize 2) cols #f)
                     (list (fvwm-border-plug action
                                             (fvwm-title-seam (car cols)
                                                              (+ tsize 2))))
                     (list (fvwm-inner-iconbar win tfont tsize ncols acols action))
                     (list (fvwm-border-plug action
                                             (fvwm-title-seam (caddr cols)
                                                              (+ tsize 2))))))

(define (load-pixmap-in-path file path)
  (if (eq? (string-ref file 0) #\/)
      (if (file-exists? file)
          (load-pixmap file)
          (if (file-exists? (string-append file ".xpm"))
              (load-pixmap (string-append file ".xpm"))
              #f))
      (let ((name (search-path path file '(".xpm"))))
        (if name
            (load-pixmap name)
            #f))))

(define fvwm-icon-pixmap-behavior
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
         (if (eq? (deco-background deco) (get-property deco 'activepixmap))
             (set-deco-background! deco apix)
             (set-deco-background! deco pix))
         (set-property! deco 'pixmap pix)
         (set-property! deco 'activepixmap apix)))
   (on (user-event 'icon-pixmap-change)
       (let* ((redo-expr (get-property deco 'redo-expr))
              (acols (get-property deco 'acols))
              (ncols (get-property deco 'ncols))
              (pix (redo-expr ncols))
              (apix (redo-expr acols)))
         (if (eq? (deco-background deco) (get-property deco 'activepixmap))
             (set-deco-background! deco apix)
             (set-deco-background! deco pix))
         (set-property! deco 'pixmap pix)
         (set-property! deco 'activepixmap apix)))))

(define (fvwm-iconcenter win ncols acols)
  (or (window-icon-window win)
      (let* ((proc (lambda (cols) 
                     (window-icon-pixmap win
                                         :foreground (cadddr cols)
                                         :background (cadr cols))))
             (pix (proc ncols))
             (apix (proc acols))
             (props `((pixmap . ,pix) (activepixmap . ,apix)
                      (ncols . ,ncols) (acols . ,acols) (redo-expr . ,proc))))
        (if pix
            (make-deco :width (width pix)
                       :height (height pix)
                       :background pix 
                       :behavior fvwm-icon-pixmap-behavior
                       :property props)
            #f))
      (let* ((file (let ((res (assq (string->symbol (window-client-class win))
                                    fvwm-icon-assoc-list)))
                     (if res (cdr res) (window-client-name win))))
             (pix (load-pixmap-in-path file fvwm-pixmap-path)))
        (if pix
            (make-deco :width (width pix)
                       :height (height pix)
                       :background pix)
            #f))))

(define fvwm-icon-behavior
   (make-behavior
    (on (focus-out) (send-user-event 'focus-out deco))
    (on (focus-in) (send-user-event 'focus-in deco))
    (on (leave) (send-user-event 'focus-out deco))
    (on (enter) (send-user-event 'focus-in deco))
    (on (user-event 'focus-out) (set-property! deco 'active #f))
    (on (user-event 'focus-in) (set-property! deco 'active #t))
    (on (user-event 'update-color)
        (let* ((props (matches-cond-all deco fvwm-window-styles))
               (cols (fvwm-get-colors (matches-cond deco fvwm-color-styles) props))
               (color-scheme (make-color-scheme (car cols) (caddr cols)))
               (active-color-scheme (make-color-scheme (cadr cols) (cadddr cols))))
          (send-user-event (list 'pre-color color-scheme active-color-scheme) deco)
          (send-user-event 're-color deco)))))

(define (fvwm-icon win)
  (let* ((props (matches-cond-all win fvwm-window-styles))
         (cols (fvwm-get-icon-colors (matches-cond win fvwm-color-styles) props))
         (title-font (get-keyword :title-font props fvwm-title-font))
         (icon-action (get-keyword :icon-action props fvwm-icon-action))
         (outer-border (get-keyword :icon-outer-border props
                                    (get-keyword :outer-border props fvwm-outer-border)))
         (fonthgt (cadr (string-dimensions "" (if (string? title-font)
                                                  (make-font title-font)
                                                  title-font))))
         (title-height (or (get-keyword :title-height props fvwm-title-height)
                           (+ 4 fonthgt)))
         (color-scheme (make-color-scheme (car cols) (caddr cols)))
         (active-color-scheme (make-color-scheme (cadr cols) (cadddr cols)))
         (borderwidth (if outer-border
                          (if (number? outer-border)
                              outer-border 1)
                          0))
         (bordercolor (make-color "black"))
         (trans (make-color 'transparent))
         (cursor fvwm-frame-cursor)
         (behavior (make-behavior icon-behavior
                                  fvwm-icon-behavior
                                  std-icon-behavior))
         (property `((title-height . ,title-height)))
         (context (list :cursor cursor
                        :behavior behavior
                        :property property))
         (center (fvwm-iconcenter win color-scheme active-color-scheme))
         (label (fvwm-iconbar win title-font title-height
                              color-scheme active-color-scheme
                              icon-action borderwidth)))
    (if center
        (make-deco (make-deco '() center '()
                              :background trans)
                   (make-deco '() label '()
                              :background trans)
                   :background trans
                   :context context)
        (make-deco label
                   :context context))))
