;; gwm.scm --- Common Gwm environment
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This is the main entry point for loading of scheme code in Gwm.
;; Unless an alternative file is given with the "-f" flag, Gwm loads
;; this file on startup.
;;
;; This file sets up a standard environment that all other packages
;; could rely upon. Then it loads "gwmrc.scm" which contains the user
;; specific settings and loading of additional packages.
;;
;; If you write a new package, it can assume that all functionality
;; in this file and in files loaded directly from here are defined.
;; It should not assume any functionality not loaded from here, but
;; instead use 'require to assure it.
;;
;; If you write a new profile, for loading with the "-f" flag, a good
;; idea is to load this file in the beginning of that profile. This is
;; not strictly required for Gwm to work, see e.g. the "fast" profile,
;; but otherwise your profile will probably not work with any other
;; Gwm package.
;;

(define white (make-color "white"))
(define black (make-color "black"))
(define gwm-loaded #t)

(define window-opening (make-hook 1))
(define window-closing (make-hook 1))
(define icon-opening (make-hook 1))
(define icon-closing (make-hook 1))
(define screen-opening (make-hook 1))
(define screen-closing (make-hook 1))

(primitive-load-path "std-func")
(primitive-load-path "advice")
(primitive-load-path "custom-menu")
(primitive-load-path "gnome")
(primitive-load-path "stack-mgr")
(primitive-load-path "window-func")
(primitive-load-path "menu-func")
(primitive-load-path "focus")
(primitive-load-path "placements")

;;----------------------------------------------------------------

(define window-style-list '())
(define icon-style-list '())
(define default-menu-style simple-menu-context)

(defmacro set-window (cond proc)
  `(begin
     (if (not (defined? (quote ,proc)))
         (primitive-load-path (symbol->string (quote ,proc))))
     (let ((ele (assoc (quote ,cond) window-style-list)))
       (if ele
           (set-cdr! ele ,proc)
           (set! window-style-list
                 (cons (cons (quote ,cond) ,proc) window-style-list))))))

(defmacro set-icon (cond proc)
  `(begin
     (if (not (defined? (quote ,proc)))
         (primitive-load-path (symbol->string (quote ,proc))))
     (let ((ele (assoc (quote ,cond) icon-style-list)))
       (if ele
           (set-cdr! ele ,proc)
           (set! icon-style-list
                 (cons (cons (quote ,cond) ,proc) icon-style-list))))))

(define (describe-window win)
  (let ((f (matches-cond win window-style-list)))
    ((or f simple-window) win)))

(define (describe-icon win)
  (let ((f (matches-cond win icon-style-list)))
    ((or f simple-icon) win)))

(define (describe-screen win)
  (modify-deco win 
               :behavior (make-behavior screen-behavior std-screen-behavior)
               :background (if (string? screen-background)
                               (make-color screen-background)
                               screen-background)
               :cursor screen-cursor)
  win)

(let ((name (or (getenv "GWMRC") "gwmrc")))
  (if (not (and (defined? 'inhibit-gwmrc) inhibit-gwmrc))
      (if (%search-load-path name)
          (primitive-load-path name)
          (begin
            (primitive-load-path "init-config")
            (initial-configure-install)))))

(custom-menu-load-preferences)

