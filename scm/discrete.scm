;; discrete.scm --- Discrete profile
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
;; It loads all files required for the discrete window and menu style,
;; and sets some default values. When used as a main profile, it also
;; loads the users menurc.scm.
;;

(if (not (defined? 'gwm-loaded))
    (begin
      (define inhibit-gwmrc #t)
      (primitive-load-path "gwm"))
)

(require 'zoom-window "zoom")

(primitive-load-path "discrete-window")
(primitive-load-path "popups")

(set-window #t discrete-window)
(set-icon #t discrete-icon)
(set! default-menu-style discrete-menu-context)

(define left-placement (make-tiled-placement 5 40 200 (screen-height) 'vertical))
(set-icon-placement #t left-placement)

(set! screen-background "gray")

(define discrete-window-title-behavior 
  (make-behavior
   (on-event (button 1 any)
             raise-lower-move-window)
   (on-event (double-button 1 any)
             (lambda (w e) (toggle-shade-window w)))
   (on-event (button 3 any)
             (lambda (deco event)
               (if (defined? 'window-pop)
                   (menu-pop (window-pop) (top-deco deco) event
                             :pos (list (deco-x deco)
                                        (+ (deco-y deco)
                                           (deco-height deco)))))))
   (on-event (double-button 3 any)
             (lambda (w e) (iconify-window w)))
   ))

(if (defined? 'inhibit-gwmrc)
    (begin
      (if (%search-load-path "menurc")
          (primitive-load-path "menurc")
          (primitive-load-path "defaultmenurc"))
      (undefine inhibit-gwmrc)))



