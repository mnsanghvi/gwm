;; pie-menu.scm --- Demo of how to create a pie-shaped menu
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file implements a menu style that gives you pie shaped menus.
;; The resulting menus are sure ugly, but mainly intended to show that
;; it is possible to do this from the scheme level. Using the same 
;; technique it should be possible to make menus and menu items of any
;; strange shape.
;;

(defvar pie-menu-foreground "black" "Foreground of menus" 'color)
(defvar pie-menu-background "gray" "Background of menus" 'color)
(defvar pie-menu-label-background "darkgray" "Background of menu labels" 'color)
(defvar pie-menu-active-background "white" "Background of active menu items" 'color)
(defvar pie-menu-font "8x13bold" "Menu item font" 'font)
(defvar pie-menu-label-font "8x13" "Menu label font" 'font)

(define (pie-menu-item-behavior)
  (make-behavior
   (on (user-event 'active)
       (let ((acol (get-property deco 'acol)))
         (set-deco-part! deco 1 acol)))
   (on (user-event 'inactive)
       (let ((icol (get-property deco 'icol)))
         (set-deco-part! deco 1 icol)))
   std-menu-item-behavior))

(define (pie-menu-label-behavior)
  std-menu-label-behavior)

(define (pie-menu-behavior)
  std-menu-behavior)

(define (pie-menu-item-make label action)
  (make-deco :background (make-color 'transparent)
             :behavior (pie-menu-item-behavior)
             :property `((label . ,label)
                         (menu . ,(if (deco? action) action #f))
                         (action . ,(if (deco? action) #f action))
                         (acol . #f)
                         (icol . #f))))

(define (pie-menu-label-make label)
  (make-deco :background (make-color 'transparent)
             :behavior (pie-menu-label-behavior)
             :property `((label . ,label))))

(define (pie-menu-separator-make)
  (make-deco :background (make-color 'transparent)
             :behavior (pie-menu-label-behavior)))

(define (pie-menu-make . args)
  (let ((font (if (string? pie-menu-font) (make-font pie-menu-font) pie-menu-font))
        (lfont (if (string? pie-menu-label-font) (make-font pie-menu-label-font) pie-menu-label-font))
        (bg (if (string? pie-menu-background) (make-color pie-menu-background) pie-menu-background))
        (lbg (if (string? pie-menu-label-background) (make-color pie-menu-label-background) pie-menu-label-background))
        (abg (if (string? pie-menu-active-background) (make-color pie-menu-active-background) pie-menu-active-background))
        (fg (if (string? pie-menu-foreground) (make-color pie-menu-foreground) pie-menu-foreground))
        (nitem 0)
        (nmitem 0)
        (nlabel 0)
        (nsep 0)
        (firstlabel #f)
        (firstlen 0)
        (maxlen 0)
        (maxhgt 0)
        (delta #f)
        (angle #f)
        (rad1 #f)
        (rad2 #f)
        (tmarg #f))
    (let* ((lab (get-property (car args) 'label))
           (dim (if lab (string-dimensions lab font) #f)))
      (if (and lab 
               (not (get-property (car args) 'action))
               (not (get-property (car args) 'menu)))
          (begin
            (set! firstlabel #t)
            (set! firstlen (+ (car dim) (cadr dim))))))
    (map (lambda (d)
           (let* ((lab (get-property d 'label))
                  (dim (if lab (string-dimensions lab font) #f))
                  (action (get-property d 'action))
                  (menu (get-property d 'menu)))
             (if lab 
                 (begin
                   (set! maxlen (max maxlen (car dim)))
                   (set! maxhgt (max maxhgt (cadr dim)))))
             (cond ((not lab)
                    (set! nsep (+ nsep 1)))
                   (action
                    (set! nitem (+ nitem 1)))
                   (menu 
                    (set! nmitem (+ nmitem 1)))
                   (#t
                    (set! nlabel (+ nlabel 1))))))
         (if firstlabel (cdr args) args))
    (set! delta (/ (- 360.0 (* 2.0 nsep)) (+ nitem nmitem nlabel)))
    (set! angle (- (/ delta 2)))
    (set! rad1 (inexact->exact (max (/ firstlen 2)
                                    (* maxhgt (/ 45.0 delta)))))
    (set! rad2 (+ rad1 maxlen maxhgt))
    (set! tmarg (quotient maxhgt 2))
    (if (and (not firstlabel)
             (get-property (car args) 'label))
        (set! args (cons (pie-menu-separator-make) args)))
    (pie-menu-setup-center (car args) rad1 rad2 lbg fg lfont tmarg)
    (map (lambda (d)
           (let ((lab (get-property d 'label))
                 (action (get-property d 'action))
                 (menu (get-property d 'menu)))
             (cond ((not lab)
                    (pie-menu-setup-sep d rad1 rad2 angle 2.0 fg)
                    (set! angle (+ angle 2.0)))
                   ((or action menu)
                    (pie-menu-setup-item d rad1 rad2 angle delta bg abg fg font tmarg)
                    (set! angle (+ angle delta)))
                   (#t
                    (pie-menu-setup-label d rad1 rad2 angle delta bg fg lfont tmarg)
                    (set! angle (+ angle delta))))))
         (cdr args))
  (let ((ctx (list :behavior (pie-menu-behavior)
                   :background (make-color 'transparent)
                   :width (* 2 rad2)
                   :height (* 2 rad2))))
    (apply make-deco (append args ctx)))))

(define deg->rad 
  (let ((c (/ (atan 1) 45)))
    (lambda (x) (* x c))))

(define (pie-menu-pix-dimensions rad1 rad2 ang delta)
  (let* ((ang1 (deg->rad ang))
         (ang2 (deg->rad (+ ang delta)))
         (x1 (* rad2 (sin ang1)))
         (y1 (* rad2 (cos ang1)))
         (x2 (* rad2 (sin ang2)))
         (y2 (* rad2 (cos ang2)))
         (x3 (* rad1 (sin ang1)))
         (y3 (* rad1 (cos ang1)))
         (x4 (* rad1 (sin ang2)))
         (y4 (* rad1 (cos ang2)))
         (minx (min x1 x2 x3 x4))
         (miny (min y1 y2 y3 y4))
         (maxx (max x1 x2 x3 x4))
         (maxy (max y1 y2 y3 y4))
         (tang (* (round (+ (/ ang 90) 0.5)) 90.0)))
    (while (< tang (+ ang delta))
      (let ((x (* rad2 (sin (deg->rad tang))))
            (y (* rad2 (cos (deg->rad tang)))))
        (set! minx (min minx x))
        (set! miny (min miny y))
        (set! maxx (max maxx x))
        (set! maxy (max maxy y))
        (set! tang (+ tang 90.0))))
    (set! minx (inexact->exact (- minx 0.5)))
    (set! miny (inexact->exact (- miny 0.5)))
    (set! maxx (inexact->exact (+ maxx 0.5)))
    (set! maxy (inexact->exact (+ maxy 0.5)))
    (list (- minx) maxy (- maxx minx) (- maxy miny))))
  
(define (pie-menu-pix-draw-sep p ox oy rad1 rad2 ang delta col)
  (draw-circle-sector p ox oy rad1 ang (+ ang delta) :foreground col :borderwidth (- rad2 rad1)))
  
(define (pie-menu-pix-draw-angle p ox oy rad1 rad2 ang delta col tcol txt tfont tmarg)
  (let ((tang (- (+ ang (/ delta 2)) 90.0)))
    (draw-circle-sector p ox oy rad1 ang (+ ang delta) :foreground col :borderwidth (- rad2 rad1))
    (draw-text p 
               (inexact->exact (+ ox (* (+ rad1 tmarg) (cos (deg->rad tang)))))
               (inexact->exact (+ oy (* (+ rad1 tmarg) (sin (deg->rad tang)))))
               txt
               :color tcol
               :font tfont
               :angle tang)))
  
(define (pie-menu-pix-draw-center p ox oy rad1 col tcol txt tfont tmarg)
  (draw-circle-sector p ox oy rad1 :background col)
  (if txt
      (draw-text p (+ (- ox rad1) tmarg) oy txt :font tfont :color tcol)))

(define (pie-menu-setup-sep d rad1 rad2 ang delta col)
  (let* ((dims (pie-menu-pix-dimensions rad1 rad2 ang delta))
         (p (make-pixmap (caddr dims) (cadddr dims) :background (make-color 'transparent))))
    (pie-menu-pix-draw-sep p (car dims) (cadr dims) rad1 rad2 ang delta col)
    (deco-add-part! d 1 p)
    (set-deco-anchor! d (list (list 0.0 (- rad2 (car dims)) 0.0)
                              (list 0.0 (- rad2 (cadr dims)) 0.0)))))
  
(define (pie-menu-setup-label d rad1 rad2 ang delta col tcol tfont tmarg)
  (let* ((dims (pie-menu-pix-dimensions rad1 rad2 ang delta))
         (lab (get-property d 'label))
         (p (make-pixmap (caddr dims) (cadddr dims) :background (make-color 'transparent))))
    (pie-menu-pix-draw-angle p (car dims) (cadr dims) rad1 rad2 ang delta col tcol lab tfont tmarg)
    (deco-add-part! d 1 p)
    (set-deco-anchor! d (list (list 0.0 (- rad2 (car dims)) 0.0)
                              (list 0.0 (- rad2 (cadr dims)) 0.0)))))

(define (pie-menu-setup-item d rad1 rad2 ang delta col1 col2 tcol tfont tmarg)
  (let* ((dims (pie-menu-pix-dimensions rad1 rad2 ang delta))
         (lab (get-property d 'label))
         (p1 (make-pixmap (caddr dims) (cadddr dims) :background (make-color 'transparent)))
         (p2 (make-pixmap (caddr dims) (cadddr dims) :background (make-color 'transparent))))
    (pie-menu-pix-draw-angle p1 (car dims) (cadr dims) rad1 rad2 ang delta col1 tcol lab tfont tmarg)
    (pie-menu-pix-draw-angle p2 (car dims) (cadr dims) rad1 rad2 ang delta col2 tcol lab tfont tmarg)
    (deco-add-part! d 1 p1)
    (set-property! d 'icol p1)
    (set-property! d 'acol p2)
    (set-deco-anchor! d (list (list 0.0 (- rad2 (car dims)) 0.0)
                              (list 0.0 (- rad2 (cadr dims)) 0.0)))))

(define (pie-menu-setup-center d rad1 rad2 col tcol tfont tmarg)
  (let* ((lab (get-property d 'label))
         (p (make-pixmap (* 2 rad1) (* 2 rad1) :background (make-color 'transparent))))
    (pie-menu-pix-draw-center p rad1 rad1 rad1 col tcol lab tfont tmarg)
    (deco-add-part! d 1 p)
    (set-deco-anchor! d (list (- rad2 rad1) (- rad2 rad1)))))

(define pie-menu-context
  (list :menu-func pie-menu-make
        :item-func pie-menu-item-make
        :label-func pie-menu-label-make
        :separator-func pie-menu-separator-make
        :foreground (lambda () pie-menu-foreground)
        :background (lambda () pie-menu-background)
        :font (lambda () pie-menu-font)))
