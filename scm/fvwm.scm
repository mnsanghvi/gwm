;; fvwm.scm --- Fvwm like profile
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
;; It loads all files required for the fvwm window and menu style,
;; and sets some default values. When used as a main profile, it also
;; loads the users menurc.scm.
;;

(if (not (defined? 'gwm-loaded))
    (begin
      (define inhibit-gwmrc #t)
      (primitive-load-path "gwm"))
)

(require 'zoom-window "zoom")
(require 'pick-window "pick")

(require 'rooms)
(require 'virtual)
(require 'virtual-rooms)
(require 'virtual-pan)

(require 'fvwm-window)
(require 'fvwm-icon)
(require 'fvwm-menu-context "fvwm-menu")

;(require 'parse-file "parse")
;
;(define fvwm-parse-list (parse-file ".fvwmrc"))
;
;(defmacro fvwm-set! (sym var def)
;  (let ((ele (assq sym fvwm-parse-list)))
;    (list (if (defined? var) 'set! 'define)
;          var
;          (if ele
;              (if (null? (cdr ele))
;                  #t
;                  (list 'quote (cadr ele)))
;              def))))
;

(set! virtual-rooms-xpos -5)
(set! virtual-rooms-ypos -5)
(set! initial-rooms '(main other))
(set! rooms-omit-list '(XLoad XClock XBiff Gwm))

(set! virtual-horizontal-step (screen-width))
(set! virtual-vertical-step (screen-height))
(set! virtual-nailed-list '(XLoad XClock XBiff Gwm))
(set! pan-on-enter #t)
(set! pan-x-step virtual-horizontal-step)
(set! pan-y-step virtual-vertical-step)
(set! pan-delay 400)
(set! pan-warp-wrapped #t)

(set! fvwm-virtual-colors #t)

(define left-placement (make-tiled-placement 5 100 6 (screen-height) 'vertical))
(set-icon-placement #t left-placement)

(set-window #t fvwm-window)
(set-icon #t fvwm-icon)
(set! default-menu-style fvwm-menu-context)

(if (defined? 'inhibit-gwmrc)
    (begin
      (if (%search-load-path "menurc")
          (primitive-load-path "menurc")
          (primitive-load-path "defaultmenurc"))
      (undefine inhibit-gwmrc)))


