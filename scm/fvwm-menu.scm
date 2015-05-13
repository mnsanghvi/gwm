;; fvwm-menu.scm --- Fvwm style menus
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Fvwm style menus. Do '(set! default-menu-style fvwm-menu-context)'
;; to use them.
;;

;;    User customizable variables. Adjust these in your own profile.
(defvar fvwm-menu-text-color "black" "Foreground of menus" 'color)
(defvar fvwm-menu-color "gray" "Background of menus" 'color)
(defvar fvwm-menu-font "8x13" "Menu item font" 'font)
(defvar fvwm-menu-label-font #f "Menu label font (defaults to item font)" 'font)
(defvar fvwm-menu-item-height 22 "Minimum height of an item" 'integer)
(defvar fvwm-menu-min-width 100 "Minimum width of menu" 'integer)


;;=============================================================

(require 'fvwm-window)

(define (fvwm-menu-internal-behavior)
  (make-behavior
   (on (user-event 'inactive)
       (set-deco-background! deco (get-property deco 'tile)))
   (on (user-event 'active)
       (set-deco-background! deco (get-property deco 'atile)))
   (on (user-event 'pressed)
       (set-deco-background! deco (get-property deco 'ptile)))))

(define (fvwm-menu-item-behavior)
  (make-behavior
   (fvwm-menu-internal-behavior)
   std-menu-item-behavior))

(define (fvwm-menu-label-behavior)
  std-menu-label-behavior)

(define (fvwm-menu-behavior)
  std-menu-behavior)

(define (fvwm-menu-topbar colors)
  (let ((pix (make-pixmap 2 2 :background (car colors))))
    (draw-point pix 1 1 :color (caddr colors))
    (make-deco '() pix
               :height 2
               :background (car colors))))

(define (fvwm-menu-bottombar colors)
  (let ((pix (make-pixmap 2 2 :background (car colors))))
    (draw-point pix 1 1 :color (caddr colors))
    (make-deco pix '()
               :height 2
               :background (caddr colors))))

(define (fvwm-menu-labeltile size colors)
  (let ((pix (make-pixmap 1 size :background (cadr colors))))
    (draw-point pix 0 (- size 3) :color (cadddr colors))
    (draw-point pix 0 (- size 1) :color (cadddr colors))
    pix))

(define (fvwm-menu-itemtile size colors active)
  (let ((pix (make-pixmap 1 size :background (cadr colors))))
    (if active
        (begin
          (draw-line pix 0 0 0 1 :color (car colors))
          (draw-line pix 0 (- size 2) 0 (- size 1) :color (caddr colors))))
    pix))

(define (fvwm-menu-vplug size colors side)
  (let ((pix (make-pixmap 1 size :background (cadr colors)))
        (apix (make-pixmap 1 size :background (if (eq? side 'left)
                                                  (car colors)
                                                  (caddr colors))))
	(ppix (make-pixmap 1 size :background (if (eq? side 'left)
                                                  (caddr colors)
                                                  (car colors)))))
    (if (eq? side 'left)
        (draw-point apix 0 (- size 1) :color (caddr colors))
        (draw-point apix 0 0 :color (car colors)))
    (make-deco '()
               :width 1
               :background pix
               :property `((tile . ,pix) (atile . ,apix) (ptile . ,ppix))
               :behavior (fvwm-menu-internal-behavior))))

(define (fvwm-menu-subarrow size colors)
  (let* ((sz (- size 4))
         (pix (make-pixmap sz sz :background (cadr colors)))
         (apix (make-pixmap sz sz :background (cadr colors)))
         (x1 (quotient sz 3))
         (x2 (- sz x1 (remainder sz 2) -1))
         (h (quotient sz 2)))
    (draw-line pix x1 x1 x1 x2 :color (car colors))
    (draw-line pix x1 x1 x2 h :color (car colors))
    (draw-line pix x1 x2 x2 h :color (caddr colors))
    (draw-line apix x1 x2 x2 h :color (car colors))
    (draw-line apix x1 x1 x1 x2 :color (caddr colors))
    (draw-line apix x1 x1 x2 h :color (caddr colors))
    (make-deco '()
               :width sz
               :height sz
               :background pix
               :property `((tile . ,pix) (atile . ,apix) (ptile . ,apix))
               :behavior (fvwm-menu-internal-behavior))))

(define (fvwm-menu-separator-make)
  (let* ((colors (make-color-scheme fvwm-menu-color fvwm-menu-text-color))
         (tile (make-pixmap 1 2 :background (car colors)))
         (left (make-pixmap 1 2 :background (car colors)))
         (right (make-pixmap 1 2 :background (caddr colors))))
    (draw-point tile 0 0 :color (caddr colors))
    (make-deco left '() right 
               :background tile
               :height 2
               :behavior (fvwm-menu-label-behavior))))

(define (fvwm-menu-item-make label action)
  (let* ((colors (make-color-scheme fvwm-menu-color fvwm-menu-text-color))
         (font (if (string? fvwm-menu-font)
                  (make-font fvwm-menu-font)
                  fvwm-menu-font))
         (menu (if (deco? action) action #f))
         (lab (make-label label 
                          :font font :horizontal-margin 4 :vertical-margin 2
                          :background (cadr colors) :foreground (cadddr colors)))
         (left0 (make-deco '() :width 2 :background (car colors)))
         (right0 (make-deco '() :width 2 :background (caddr colors)))
         (left (fvwm-menu-vplug fvwm-menu-item-height colors 'left))
         (right (fvwm-menu-vplug fvwm-menu-item-height colors 'right))
         (tile (fvwm-menu-itemtile fvwm-menu-item-height colors #f))
         (atile (fvwm-menu-itemtile fvwm-menu-item-height colors #t))
         (ptile (fvwm-menu-itemtile fvwm-menu-item-height (list (caddr colors) (cadr colors) (car colors)) #t))
         (arrow (if menu (fvwm-menu-subarrow fvwm-menu-item-height colors) '()))
         (props `((menu . ,menu)
                  (action . ,(if menu #f action))
                  (tile . ,tile)
                  (atile . ,atile)
		  (ptile . ,ptile))))
    (make-deco left0 left lab '() arrow right right0
               :background tile
               :height fvwm-menu-item-height
               :behavior (fvwm-menu-item-behavior)
               :property props)))

(define (fvwm-menu-label-make label)
  (let* ((colors (make-color-scheme fvwm-menu-color fvwm-menu-text-color))
         (font (cond ((string? fvwm-menu-label-font)
                      (make-font fvwm-menu-label-font))
                     (fvwm-menu-label-font
                      fvwm-menu-label-font)
                     ((string? fvwm-menu-font)
                      (make-font fvwm-menu-font))
                     (#t
                      fvwm-menu-font)))
         (lab (make-label label 
                          :font font :horizontal-margin 4 :vertical-margin 2
                          :background (cadr colors) :foreground (cadddr colors)))
         (left (make-deco '() :width 2 :background (car colors)))
         (right (make-deco '() :width 2 :background (caddr colors)))
         (tile (fvwm-menu-labeltile fvwm-menu-item-height colors)))
    (make-deco left '() lab '() right
               :background tile
               :height fvwm-menu-item-height
               :behavior (fvwm-menu-label-behavior))))

(define (fvwm-menu-make . args)
  (let* ((colors (make-color-scheme fvwm-menu-color fvwm-menu-text-color))
         (top (fvwm-menu-topbar colors))
         (bottom (fvwm-menu-bottombar colors)))
    (apply make-deco (append (list top)
                             args
                             (list bottom)
                             (list :min-width fvwm-menu-min-width
                                   :behavior (fvwm-menu-behavior))))))

(define fvwm-menu-context
  (list :menu-func fvwm-menu-make
        :item-func fvwm-menu-item-make
        :label-func fvwm-menu-label-make
        :separator-func fvwm-menu-separator-make
        :foreground (lambda () fvwm-menu-text-color)
        :background (lambda () fvwm-menu-color)
        :font (lambda () fvwm-menu-font)))


