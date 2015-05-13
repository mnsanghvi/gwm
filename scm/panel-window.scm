;; panel-window.scm ---  Window style for panel windows.
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Panel window style. Simple decoration without title or buttons, 
;; suitable for transient windows, dialogs, Gwm placed menus, and the 
;; small windows at the top of the screen (i.e. clock, biff, load, etc).
;; Do '(set-window WINDOW-CLASS panel-window)' to use it.
;;

;;    User customizable variables. Adjust these in your own profile.
(defvar panel-color "tan" "Default color of panel window" 'color)
(defvar panel-color-styles `() "Specifications for different colors of different windows" 'list)
(defvar panel-check-resource-background #t "Check the resource database for the client color" 'boolean)
(defvar panel-check-menu-background #t "Check the menu background for the color" 'boolean)


;;   End of user customizable things. Here starts the real code.

(if (not (defined? 'make-color-scheme))
    (begin

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

))

(define (panel-left-tile cols shape)
  (let ((pix (make-pixmap 4 1 :background (car cols))))
    (draw-line pix 2 0 3 0 :color (if shape (caddr cols) (cadr cols)))
    pix))

(define (panel-right-tile cols shape)
  (let ((pix (make-pixmap 4 1 :background (caddr cols))))
    (draw-line pix 0 0 1 0 :color (if shape (car cols) (cadr cols)))
    pix))

(define (panel-top-tile cols shape)
  (let ((pix (make-pixmap 1 4 :background (car cols))))
    (draw-line pix 0 2 0 3 :color (if shape (caddr cols) (cadr cols)))
    pix))

(define (panel-bot-tile cols shape)
  (let ((pix (make-pixmap 1 4 :background (caddr cols))))
    (draw-line pix 0 0 0 1 :color (if shape (car cols) (cadr cols)))
    pix))

(define (panel-tl-pixmap cols shape)
  (let ((trans (make-color 'hole))
        (pix (make-pixmap 4 4 :background (car cols))))
    (draw-line pix 0 0 1 0 :color trans)
    (draw-line pix 0 1 0 1 :color trans)
    (draw-line pix 2 3 3 3 :color (if shape (caddr cols) (cadr cols)))
    (draw-line pix 3 2 3 2 :color (if shape (caddr cols) (cadr cols)))
    pix))

(define (panel-br-pixmap cols shape)
  (let ((trans (make-color 'hole))
        (pix (make-pixmap 4 4 :background (caddr cols))))
    (draw-line pix 0 0 1 0 :color (if shape (car cols) (cadr cols)))
    (draw-line pix 0 1 0 1 :color (if shape (car cols) (cadr cols)))
    (draw-line pix 2 3 3 3 :color trans)
    (draw-line pix 3 2 3 2 :color trans)
    pix))

(define (panel-tr-pixmap cols shape)
  (let ((pix (make-pixmap 4 4 :background (make-color 'hole))))
    (draw-rectangle pix 0 1 3 3 :background (cadr cols))
    (draw-rectangle pix 0 0 2 2 :background (car cols))
    (draw-rectangle pix 2 2 2 2 :background (caddr cols))
    (if shape 
        (begin
          (draw-point pix 0 2 :color (caddr cols))
          (draw-point pix 1 3 :color (car cols))))
    pix))

(define (panel-bl-pixmap cols shape)
  (let ((pix (make-pixmap 4 4 :background (make-color 'hole))))
    (draw-rectangle pix 1 0 3 3 :background (cadr cols))
    (draw-rectangle pix 0 0 2 2 :background (car cols))
    (draw-rectangle pix 2 2 2 2 :background (caddr cols))
    (if shape 
        (begin
          (draw-point pix 2 0 :color (caddr cols))
          (draw-point pix 3 1 :color (car cols))))
    pix))


(define (panel-window win)
  (let* ((shape (window-client-shaped? win))
         (speccol (matches-cond win panel-color-styles))
         (cols (make-color-scheme (or (and (pair? speccol)
                                           (car speccol))
                                      (and panel-check-resource-background
                                           (get-x-default (window-client-class win)
                                                         "Background"))
                                      (and panel-check-menu-background
                                           (deco-menu? win)
                                           (not (equal? (color-components (deco-background win))
                                                        '(65535 65535 65535)))
                                           (deco-background win))
                                      panel-color) #f)))
    (make-deco (make-deco (panel-tl-pixmap cols shape)
                          '()
                          (panel-tr-pixmap cols shape)
                          :background (panel-top-tile cols shape))
               (make-deco (make-deco '() :width 4 :background (panel-left-tile cols shape))
                          win
                          (make-deco '() :width 4 :background (panel-right-tile cols shape)))
               (make-deco (panel-bl-pixmap cols shape)
                          '()
                          (panel-br-pixmap cols shape)
                          :background (panel-bot-tile cols shape))
               :behavior (make-behavior window-behavior
                                        std-window-behavior))))

(define (panel-icon-plug win font cols)
  (make-deco (make-label (if (equal? window-icon-name "icon")
                             (window-name win)
                             (window-icon-name win))
                         :font font
                         :background (cadr cols))
             :behavior (make-behavior (on (user-event 'name-change)
                                          (set-deco-part! deco
                                                          (make-label (if (string=? (window-icon-name deco) "icon")
                                                                          (window-name deco)
                                                                          (window-icon-name deco))
                                                                      :font font
                                                                      :background (cadr cols))
                                                          1)))))
  
(define (panel-icon win)
  (let* ((font (if (string? panel-icon-font) (make-font panel-icon-font) panel-icon-font))
         (speccol (matches-cond win panel-color-styles))
         (cols (make-color-scheme (or (and (pair? speccol)
                                           (car speccol))
                                      (and panel-check-resource-background
                                           (get-x-default (window-client-class win)
                                                         "Background"))
                                      (and panel-check-menu-background
                                           (deco-menu? win)
                                           (not (equal? (color-components (deco-background win))
                                                        '(65535 65535 65535)))
                                           (deco-background win))
                                      panel-color) #f)))
    (make-deco (make-deco (panel-tl-pixmap cols #f)
                          '()
                          (panel-tr-pixmap cols #f)
                          :background (panel-top-tile cols #f))
               (make-deco (make-deco '() :width 4 :background (panel-left-tile cols #f))
                          (panel-icon-plug win font cols)
                          (make-deco '() :width 4 :background (panel-right-tile cols #f)))
               (make-deco (panel-bl-pixmap cols #f)
                          '()
                          (panel-br-pixmap cols #f)
                          :background (panel-bot-tile cols #f))
               :behavior (make-behavior icon-behavior
                                        std-icon-behavior))))


