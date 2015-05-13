;; twm.scm --- Twm like profile
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file can be used either as main profile (using the -f flag),
;; or as user profile (set the environment variable GWMPROFILE or load
;; it from gwmrc.scm).
;;
;; It loads all files required for the twm window and menu style,
;; and sets some default values. When used as a main profile, it also
;; loads the users menurc.scm.
;;

(if (not (defined? 'gwm-loaded))
    (begin
      (define inhibit-gwmrc #t)
      (primitive-load-path "gwm"))
)

(require 'icon-mgr)
(require 'zoom-window "zoom")
(require 'pick-window "pick")

(require 'twm-window)
(require 'twm-icon)
(require 'twm-menu-context "twm-menu")

(require 'parse-file "parse")

(define twm-parse-list (parse-file ".twmrc"))
(define twm-color-parse-list (cadr (or (assq 'Color twm-parse-list) '(() ()))))
(define twm-gray-parse-list (cadr (or (assq 'Grayscale twm-parse-list) '(() ()))))
(define twm-mono-parse-list (cadr (or (assq 'Monochrome twm-parse-list) '(() ()))))

(defmacro twm-set! (sym var def)
  (let ((ele (assq sym twm-parse-list)))
    (list (if (defined? var) 'set! 'define)
          var
          (if ele
              (if (null? (cdr ele))
                  #t
                  (list 'quote (cadr ele)))
              def))))

;(cond ((eq? (screen-type) 'color)
;                              twm-color-parse-list)
;                             ((eq? (screen-type) 'grayscale)
;                              twm-gray-parse-list)
;                             ((eq? (screen-type) 'mono)
;                              twm-mono-parse-list))

(defmacro twm-set-col! (sym var def)
  (let ((ele (assq sym twm-color-parse-list)))
    (list (if (defined? var) 'set! 'define)
          var
          (if ele (list 'quote (cadr ele)) def))))

(twm-set! BorderWidth twm-borderwidth 2)
(twm-set! TitleFont twm-title-font "variable")
(twm-set-col! TitleBackground twm-title-background "white")
(twm-set-col! TitleForeground twm-title-foreground "black")
(twm-set-col! BorderColor twm-active-bordercolor #f)
(twm-set-col! BorderTileBackground twm-border-bg #f)
(twm-set-col! BorderTileForeground twm-border-fg #f)
(set! twm-bordercolor 
      (if (or twm-border-bg twm-border-fg)
          (make-pixmap "gray.xbm"
                       :foreground (make-color (or twm-border-fg "black"))
                       :background (make-color (or twm-border-bg "white")))
          "black"))

(twm-set! SqueezeTitle twm-squeezed-title-lst #f)
(set! twm-squeezed-title (if twm-squeezed-title-lst #t #f))
(set! twm-squeezed-hilite-size 28)

(twm-set! MenuFont twm-menu-font "variable")
(twm-set-col! MenuBackground twm-menu-background "white")
(twm-set-col! MenuForeground twm-menu-foreground "black")
(twm-set-col! MenuTitleBackground twm-menu-label-background "white")
(twm-set-col! MenuTitleForeground twm-menu-label-foreground "black")
(set! twm-menu-bordercolor twm-menu-foreground)
(set! twm-menu-hilite-borderwidth 0)
(set! twm-menu-hilite-foreground twm-menu-background)
(set! twm-menu-hilite-background twm-menu-foreground)
(twm-set! NoMenuShadow twm-menu-no-shadow #f)
(set! twm-menu-shadow (not twm-menu-no-shadow))
(twm-set-col! MenuShadowColor twm-menu-shadow-color "black")
(set! twm-menu-shadow-offset 3)
(set! twm-menu-borderwidth 1)

(set! icon-mgr-no-title #t)
(set! icon-mgr-framed-bars #t)
(twm-set-col! IconManagerBackground icon-mgr-background "white")
(twm-set-col! IconManagerForeground icon-mgr-foreground "black")
(twm-set! IconManagerFont icon-mgr-font "variable")

(set-window #t twm-window)
(set-icon #t twm-icon)
(set! default-menu-style twm-menu-context)

(if (defined? 'inhibit-gwmrc)
    (begin
      (if (%search-load-path "menurc")
          (primitive-load-path "menurc")
          (primitive-load-path "defaultmenurc"))
      (undefine inhibit-gwmrc)))

