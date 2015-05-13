;; wm2.scm --- Wm2 like profile
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
;; It makes Gwm use the wm2 window and menu style, and sets some default
;; values. 
;;

(if (not (defined? 'gwm-loaded))
    (begin
      (define inhibit-gwmrc #t)
      (primitive-load-path "gwm")
      (undefine inhibit-gwmrc))
)

(require 'wm2-window)

(set-window #t wm2-window)
(set-icon #t wm2-icon)
(set! default-menu-style wm2-menu-context)

(set! screen-behavior
      (make-behavior 
       (on (button 1 any) (menu-pop (wm2-make-icon-menu) deco event))))
