;; twm-menu.scm --- Twm style menus
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Twm style menus. Do '(set! default-menu-style twm-menu-context)'
;; to use them.
;;

;;    User customizable variables. Adjust these in your own profile.
(defvar twm-menu-foreground "black" "Foreground of menus" 'color)
(defvar twm-menu-background "white" "Background of menus" 'color)
(defvar twm-menu-borderwidth #f "Borderwidth of menus (defaults to windows borderwidth)" 'color)
(defvar twm-menu-bordercolor "black" "Border color of menus" 'color)
(defvar twm-menu-label-foreground #f "Foreground of menu labels" 'color)
(defvar twm-menu-label-background #f "Background of menu labels" 'color)
(defvar twm-menu-label-borderwidth 1 "Border width of menu labels" 'integer)
(defvar twm-menu-hilite-bordercolor "black" "Border color of current menu item" 'color)
(defvar twm-menu-hilite-borderwidth 1 "Border width of current menu item" 'integer)
(defvar twm-menu-hilite-foreground #f "Text color of current menu item" 'color)
(defvar twm-menu-hilite-background #f "Background color of current menu item" 'color)
(defvar twm-menu-shadow #t "Whether the menu has a shadow" 'boolean)
(defvar twm-menu-shadow-color "black" "Color of shadow" 'color)
(defvar twm-menu-shadow-offset 6 "Offset of shadow from menu" 'integer)
(defvar twm-menu-font "8x13" "Menu item font" 'font)
(defvar twm-menu-label-font #f "Menu label font (defaults to item font)" 'font)
(defvar twm-menu-item-height 10 "Minimum height of an item" 'integer)
(defvar twm-menu-min-width 100 "Minimum width of menu" 'integer)


;;=============================================================

(require 'twm-window)

(define (twm-menu-item-behavior)
  (make-behavior
   (on (user-event 'active)
       (let ((cols (get-property deco 'cols))
             (tile (get-property deco 'atile)))
         (if (car cols)
             (set-deco-bordercolor! deco (car cols)))
         (if (cadr cols)
             (set-deco-background! deco (cadr cols)))
         (if tile
             (set-deco-part! deco 2 tile))))
   (on (user-event 'inactive)
       (let ((cols (get-property deco 'cols))
             (tile (get-property deco 'ntile)))
         (if (car cols)
             (set-deco-bordercolor! deco (caddr cols)))
         (if (cadr cols)
             (set-deco-background! deco (caddr cols)))
         (if tile
             (set-deco-part! deco 2 tile))))
   std-menu-item-behavior))

(define (twm-menu-label-behavior)
  std-menu-label-behavior)

(define (twm-menu-behavior)
  std-menu-behavior)

(define (twm-menu-item-make label action)
  (let ((font (make-font twm-menu-font))
        (bg (make-color twm-menu-background))
        (fg (make-color twm-menu-foreground))
        (hbg (if twm-menu-hilite-background (make-color twm-menu-hilite-background) #f))
        (hfg (if twm-menu-hilite-foreground (make-color twm-menu-hilite-foreground) #f))
        (hbc (if twm-menu-hilite-bordercolor (make-color twm-menu-hilite-bordercolor) #f)))
    (let* ((cdiff (or (and hbg (not (eq? hbg bg)))
                      (and hfg (not (eq? hfg fg)))))
           (bdiff (and (> twm-menu-hilite-borderwidth 0)
                       hbc
                       (not (eq? hbc bg))))
           (ntile (make-label label :background bg :foreground fg
                              :font font :horizontal-margin 4 :vertical-margin 2))
           (atile (if cdiff 
                      (make-label label 
                                  :background (or hbg bg) :foreground (or hfg fg)
                                  :font font :horizontal-margin 4 :vertical-margin 2)
                      #f))
           (menu (if (deco? action) action #f))
           (cols (list (if bdiff hbc #f)
                       (if cdiff hbg #f)
                       (if (or cdiff bdiff) bg #f))))
      (make-deco '() ntile '()
                :borderwidth twm-menu-hilite-borderwidth
                :bordercolor bg
                :background bg
                :foreground fg
                :min-height twm-menu-item-height
                :behavior (twm-menu-item-behavior)
                :property `((menu . ,menu)
                            (action . ,(if menu #f action))
                            (cols . ,cols)
                            (ntile . ,(if atile ntile #f))
                            (atile . ,atile))))))
     
(define (twm-menu-label-make label)
  (let ((bg (make-color (or twm-menu-label-background
                            twm-menu-background)))
        (fg (make-color (or twm-menu-label-foreground
                            twm-menu-foreground)))
        (font (make-font (or twm-menu-label-font
                             twm-menu-font))))
    (make-deco '() 
               (make-label label :background bg :foreground fg
                           :font font :horizontal-margin 4 :vertical-margin 2)
               '()
               :borderwidth (or twm-menu-label-borderwidth 0)
               :background bg
               :foreground fg
               :bordercolor fg
               :behavior (twm-menu-label-behavior))))

(define (twm-menu-separator-make)
  (make-deco :width (max 1 twm-menu-label-borderwidth)
             :background (make-color twm-menu-foreground)
             :behavior (twm-menu-label-behavior)))

(define (twm-menu-make . args)
  (let ((ctx (list :behavior (twm-menu-behavior)
                   :min-width twm-menu-min-width
                   :borderwidth (or twm-menu-borderwidth twm-borderwidth)
                   :bordercolor (make-color twm-menu-bordercolor)
                   :property `((shadow . #f)
                               (pop-hook . ,(make-hook 3))
                               (unpop-hook . ,(make-hook 1)))))
        (res #f))
    (set! res (apply make-deco (append args ctx)))
    (if twm-menu-shadow
        (begin
          (set-property! res 'shadow (make-deco :width (width res)
                                                :height (height res)
                                                :background (make-color twm-menu-shadow-color)))
          (add-hook! (get-property res 'pop-hook) twm-pop-shadow)
          (add-hook! (get-property res 'unpop-hook) twm-unpop-shadow)))
    res))

(define (twm-unpop-shadow w)
  (if twm-menu-shadow
      (unpop-menu (get-property w 'shadow))))

(define (twm-pop-shadow w xpos ypos)
  (let ((shadow (get-property w 'shadow)))
    (if (and twm-menu-shadow shadow)
        (begin
          (pop-menu shadow
                    (root-window)
                    (+ xpos twm-menu-shadow-offset)
                    (+ ypos twm-menu-shadow-offset)
                    :menu-parent w)
          (lower-window (top-deco shadow) w)))))

(define twm-menu-context
  (list :menu-func twm-menu-make
        :item-func twm-menu-item-make
        :label-func twm-menu-label-make
        :separator-func twm-menu-separator-make
        :foreground (lambda () twm-menu-foreground)
        :background (lambda () twm-menu-background)
        :font (lambda () twm-menu-font)))

   