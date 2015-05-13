;; init-config.scm --- Construct the personal configuration files via a dialogue
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; The function (initial-configure) checks if the user has a "gwmrc.scm" 
;; file, and if not asks the user about some preferences, and then
;; constructs "gwmrc.scm" (and "menurc.scm") accordingly.
;; The function (initial-configure-install) sets everything up so that
;; 'initial-configure' is called when opening the first screen. This 
;; function can be called if the users "gwmrc.scm" file is not found.
;;

(require 'complex-dialogue "dialogue")
(require 'panel-window)

(define (initial-configure-dialogue files)
  (let ((gl `((header "Welcome to GWM !")
              (((label "Here you can specify an initial configuration for GWM.")
                (label ,(if (cdr files)
                            (simple-format #f "It will produce the files ~S and ~S," (car files) (cdr files))
                            (simple-format #f "It will produce the file ~S," (car files))))
                (label "which you can then edit further.")))
              (((label "  Window style:")
                ((radio style fvwm) (label "Fvwm windows and icons"))
                ((radio style twm) (label "Twm windows and icons"))
                ((radio style wm2) (label "Wm2 windows and icons"))
                ((radio style aquax) (label "MacOS X windows"))
                ((radio style discrete) (label "Discrete windows and icons"))
                ((radio style simple) (label "Simple (no frame) windows and icons"))))
              (((label "  Packages:")
                ((check #f) (label "Virtual Screen"))
                ((check #f) (label "Rooms"))
                ((check #f) (label "Icon Manager"))))
              (((label "  Focus mode:")
                ((radio focus enter) (label "Focus on enter, unfocus on leave"))
                ((radio focus sloppy) (label "Focus on enter (nothing on leave)"))
                ((radio focus click) (label "Focus on click"))
                ((check #f) (label "Autoraise on focus"))))))
        (ml `((((label "  Menus:")
                ((check #t) (label "Machine login menu"))
                ((check #t) (label "Command menu"))
                ((check #f) (label "Machine/Command double menu"))
                ((check #t) (label "Root menu"))
                ((check #t) (label "Window menu"))
                ((check #t) (label "Window list menu"))
                ,(if (file-exists? "/etc/X11/gwm2/debian-menu.scm")
                     '((check #f) (label "Debian menu"))
                     '())))))
        (sl '((() (ok) () (cancel) ()))))
    (complex-dialogue (if (cdr files)
                          (append gl ml sl)
                          (append gl sl))
                      :background (make-color "lightgreen")
                      :decoration panel-window)))

(define (initial-configure-check-files)
  (let* ((homedir (getenv "HOME"))
         (rcname (basename (or (getenv "GWMRC") "gwmrc") ".scm"))
         (gwmdir (if homedir (string-append homedir "/" "gwm/") #f))
         (gwmrc1 (if homedir (string-append homedir "/" rcname ".scm") #f))
         (gwmrc2 (if gwmdir (string-append gwmdir rcname ".scm") #f))
         (menurc1 (if homedir (string-append homedir "/" "menurc.scm") #f))
         (menurc2 (if gwmdir (string-append gwmdir "menurc.scm") #f)))
    (cond ((or (not homedir) (file-exists? gwmrc1) (file-exists? gwmrc2))
           #f)
          ((file-exists? menurc1)
           (cons gwmrc1 #f))
          ((file-exists? menurc2)
           (cons gwmrc2 #f))
          ((not (file-exists? gwmdir))
           (cons gwmrc2 menurc2))
          ((let ((cdir 0)
                 (cscm 0)
                 (cgwm 0)
                 (cpix 0)
                 (cother 0)
                 (dir (opendir gwmdir))
                 (file #f))
             (set! file (readdir dir))
             (while (not (eof-object? file))
               (set! file (string-append gwmdir file))
               (cond ((file-is-directory? file)
                      (set! cdir (+ 1 cdir)))
                     ((string-match "\\.scm" file)
                      (set! cscm (+ 1 cscm)))
                     ((string-match "\\.gwm" file)
                      (set! cgwm (+ 1 cgwm)))
                     ((or (string-match "\\.xpm" file)
                          (string-match "\\.xbm" file)
                          (string-match "\\.png" file))
                      (set! cpix (+ 1 cpix)))
                     (#t
                      (set! cother (+ 1 cother))))
               (set! file (readdir dir)))
             (closedir dir)
             (or (> cscm 0)
                 (and (= cgwm 0)
                      (< cother 5))))
           (cons gwmrc2 menurc2))
          (#t
           (cons gwmrc1 menurc1)))))

(define (initial-configure)
  (let* ((files (initial-configure-check-files))
         (res (if files (initial-configure-dialogue files) #f)))
    (if res
        (let* ((chop (lambda () (if (pair? res) (let ((x (car res))) (set! res (cdr res)) x) #f)))
               (style (chop))
               (virtual (chop))
               (rooms (chop))
               (imgr (chop))
               (focus (chop))
               (autoraise (chop))
               (loginpop (chop))
               (commandpop (chop))
               (machcompop (chop))
               (rootpop (chop))
               (windowpop (chop))
               (winlistpop (chop))
               (debianpop (chop))
               (gfile #f)
               (mfile #f)
               (rcfile (basename (car files))))
          (if (not (file-exists? (dirname (car files))))
              (mkdir (dirname (car files))))
          (set! gfile (open-file (car files) "w"))
          (display ";; " gfile)
          (display rcfile gfile)
          (display "
;;
;; In this file you can specify your personal settings for Gwm.

(define-module (guile-user) :use-module (ice-9 debug))
(debug-enable 'backtrace)

(define simple-clients (list 'XLoad 'XClock 'XBiff 'XBatt 'Gwm window-transient-for))

(define rpos -5)
(define rposdiff 13)

;; General Packages
(require 'pick-window \"pick\")
(require 'zoom-window \"zoom\")
(set! zoom-window-method-list `((XTerm . ,zoom-window-vert)
                                (XVroot . ,zoom-window-prop)
                                (#t . ,zoom-window-full)))
(set! raise-on-resize #t)
(set! raise-on-move #t)
(set! screen-cursor (make-cursor 68))
"
                   gfile)
          (if imgr
              (display "
;; Icon Manager
(require 'icon-mgr)
(set! icon-mgr-title-background \"khaki\")
(set! icon-mgr-background \"lightyellow\")
(set! icon-mgr-sort #t)
(set! iconify-on-start-list '(XConsole \"^gwm\"))
(set! icon-mgr-omit-list simple-clients)
(set! icon-mgr-xpos rpos)
(set! icon-mgr-ypos 4)
(set! rpos (- rpos icon-mgr-width rposdiff))
"
                       gfile))
          (cond ((and virtual rooms)
                 (display "
;; Virtual Screen
(require 'virtual)
(require 'virtual-pan)
(set! virtual-horizontal-step (screen-width))
(set! virtual-vertical-step (screen-height))
(set! virtual-nailed-list (append simple-clients '(XConsole \"^gwm\")))

;; Rooms
(require 'rooms)
(set! initial-rooms '(main other))
(set! rooms-omit-list (append simple-clients '(XConsole \"^gwm\")))

;; Virtual Rooms Map
(require 'virtual-rooms)
(set! virtual-omit-nailed #f)
(set! virtual-omit-list simple-clients)
(set! virtual-fancy-colors '((#f #f \"lightgray\") (#t #f \"white\")))
(set! virtual-background \"lightyellow\")
(set! virtual-rooms-xpos rpos)
(set! virtual-rooms-ypos 4)
(set! rpos (- rpos (* virtual-rooms-xsize (length initial-rooms)) rposdiff))
"
                          gfile))
                (virtual
                 (display "
;; Virtual Screen
(require 'virtual)
(require 'virtual-map)
(require 'virtual-door)
(require 'virtual-pan)
(set! virtual-horizontal-step (screen-width))
(set! virtual-vertical-step (screen-height))
(set! virtual-nailed-list (append simple-clients '(XConsole \"^gwm\")))
(set! virtual-omit-nailed #f)
(set! virtual-omit-list simple-clients)
(set! virtual-fancy-colors '((#f #f \"lightgray\") (#t #f \"white\")))
(set! virtual-background \"lightyellow\")
(set! virtual-xpos rpos)
(set! virtual-ypos 4)
(set! virtual-size '(2 2))
(set! rpos (- rpos virtual-pixsize rposdiff))

(set! initial-doors '(\"Home\" \"Free\"))
(set! door-background \"lightyellow\")
(set! door-borderwidth 1)
(set! door-mgr-xpos 4)
(set! door-mgr-ypos 4)
"
                          gfile))
                (rooms
                 (display "
;; Rooms
(require 'rooms)
(require 'room-mgr)
(set! initial-rooms '(main other))
(set! rooms-omit-list (append simple-clients '(XConsole \"^gwm\")))
(set! room-mgr-background \"lightyellow\")
(set! room-mgr-xpos 4)
(set! room-mgr-ypos 4)
"
                          gfile)))
          (cond ((or (eq? focus 'enter) (eq? focus 'sloppy))
                 (display "
;; Focus
(set! focus-mode 'enter)"
                          gfile)
                 (if (eq? focus 'sloppy)
                     (display "
(set! focus-enter-sloppy #t)"
                          gfile))
                 (if autoraise
                     (display "
(set! focus-enter-delay 0.3)
(set! focus-auto-raise #t)"
                          gfile))
                 (newline gfile))
                ((eq? focus 'click)
                 (display "
;; Focus
(set! focus-mode 'click)
(set! focus-click-resend #t)"
                          gfile)
                 (if autoraise
                     (display "
(set! focus-auto-raise #t)"
                              gfile))
                 (newline gfile)))
          (cond ((eq? style 'fvwm)
                 (display "
;; Fvwm style
(require 'fvwm-window)
(require 'fvwm-icon)
(require 'fvwm-menu-context \"fvwm-menu\")

(set! fvwm-color-styles '((\"^gwm\" \"tan\" \"orange\")
                          (\"^su\" \"gray50\" \"black\" \"black\" \"gray\")
                          (XConsole \"tan\" \"tan\")
                          (XTerm \"grey\")
                          (Emacs \"lightblue\")))
(set! fvwm-window-styles
      `((,(lambda (w) (matches-list w simple-clients))
         :frame #t :resize #f :title #f :frame-width 5 :color \"tan\"
         :active-color #f :outer-border #f :inner-border #f)))
(set! fvwm-border-action (lambda (w e) (raise-lower-move-window w e)))
(set! fvwm-virtual-colors #t)
(set! fvwm-icon-assoc-list '((Emacs . \"lemacs\")))
(set! fvwm-menu-color \"wheat2\")

(set-window #t fvwm-window)
(set-icon #t fvwm-icon)
(set! default-menu-style fvwm-menu-context)
"
                          gfile))
                ((eq? style 'twm)
                 (display "
;; Twm style
(require 'twm-window)
(require 'twm-icon)
(require 'twm-menu-context \"twm-menu\")

(set! twm-title-background \"royalblue\")
(set! twm-title-foreground \"white\")
(set! twm-active-bordercolor \"black\")
(set! twm-notitle-list simple-clients)
(set! twm-icon-foreground \"darkslategray\")
(set! twm-icon-background \"green\")
(set! twm-menu-background \"pink\")
(set! twm-menu-hilite-background \"palevioletred\")
(set! twm-menu-hilite-bordercolor twm-menu-hilite-background)

(set-window #t twm-window)
(set-icon #t twm-icon)
(set! default-menu-style twm-menu-context)
"
                          gfile))
                ((eq? style 'wm2)
                 (display "
;; Wm2 style
(require 'wm2-window)

(set-window #t wm2-window)
(set-icon #t wm2-icon)
(set! default-menu-style wm2-menu-context)
"
                          gfile))
                ((eq? style 'aquax)
                 (display "
;; Fvwm style
(require 'fvwm-window)
(require 'fvwm-icon)
(require 'fvwm-menu-context \"fvwm-menu\")

(set! fvwm-color-styles '((\"^gwm\" \"tan\" \"orange\")
                          (\"^su\" \"gray50\" \"black\" \"black\" \"gray\")
                          (XConsole \"tan\" \"tan\")
                          (XTerm \"grey\")
                          (Emacs \"lightblue\")))
(set! fvwm-window-styles
      `((,(lambda (w) (matches-list w simple-clients))
         :frame #t :resize #f :title #f :frame-width 5 :color \"tan\"
         :active-color #f :outer-border #f :inner-border #f)))
(set! fvwm-border-action (lambda (w e) (raise-lower-move-window w e)))
(set! fvwm-virtual-colors #t)
(set! fvwm-icon-assoc-list '((Emacs . \"lemacs\")))
(set! fvwm-menu-color \"wheat2\")

(set-icon #t fvwm-icon)
(set! default-menu-style fvwm-menu-context)

;; AquaX style
(require 'aquax-window)
(set-window #t aquax-window)
"
                          gfile))
                ((eq? style 'discrete)
                 (display "
;; Discrete style
(require 'discrete-window)

(set! discrete-window-title-behavior 
      (make-behavior
       (on-event (button 1 any)
                 raise-lower-move-window)
       (on-event (double-button 1 any)
                 (lambda (w e) (toggle-shade-window w)))
       (on-event (button 3 any)
                 (lambda (deco event) (menu-pop (window-pop) (top-deco deco) event
                                                :pos (list (deco-x deco)
                                                           (+ (deco-y deco)
                                                              (deco-height deco))))))
       (on-event (double-button 3 any)
                 (lambda (w e) (iconify-window w)))
   ))

(set-window #t discrete-window)
(set-icon #t discrete-icon)
(set! default-menu-style discrete-menu-context)
"
                          gfile))
                ((eq? style 'simple)
                 (display "
;; Simple window style
(set-window #t simple-window)
(set-icon #t simple-icon)
(set! default-menu-style simple-menu-context)
"
                          gfile)))
          (display "
;; Placements
(define top-place-order '(XBiff XBatt XClock XLoad))
(define top-placement 
  (make-tiled-placement rpos 4 200 5 'horizontal
                        :separator 3
                        :sort (lambda (w1 w2)
                                (< (or (list-index top-place-order (string->symbol (window-client-class w1))) 1000)
                                   (or (list-index top-place-order (string->symbol (window-client-class w2))) 1000)))))
(define right-placement
  (make-tiled-placement -5 200 4 (screen-height) 'vertical))

(set-placement 'XBiff top-placement)
(set-placement 'XBatt top-placement)
(set-placement 'XClock top-placement)
(set-placement 'XLoad top-placement)

(set-icon-placement #t right-placement)

;; menu specifications in a separate file
(primitive-load-path \"menurc.scm\")
"
                   gfile)
          (close-port gfile)
          (if (cdr files)
              (let ((binds (make-list 6 #f)))
                (set! mfile (open-file (cdr files) "w"))
                (display 
";; menurc.scm
;; This file contains specification of your popup menus

(primitive-load-path \"popups\")
"
                         mfile)
                (if (or loginpop machcompop)
                    (display "
;; Add names on computers for the login menu to this list
(defvar machine-list
        '()
        \"List of machines that can be logged in to.\" 'list)
"
                             mfile))
                (if (or commandpop machcompop)
                    (display "
(defvar command-pop-list
        `((\"Xterm\" ,(lambda (h) (list \"xterm\" \"-n\" (machine-name h))))
          (\"Emacs\" ,(lambda (h) (list \"emacs\" \"-name\"
                                        (string-append (machine-name h) \"-Emacs\")
                                        \"-geometry\" \"85x50\")))
          \"xcalc\"
          \"mozilla\"
          (\"Root\" \"xterm\" \"-e\" \"su\" \"-\"))
        \"List of shell commands to have in the menu.\" 'list)
"
                             mfile))
                (if rootpop
                    (display "
(defvar root-pop-list
        `((\"Customize\" ,(lambda (x) (custom-menu)))
          (\"Refresh\" ,(lambda (x) (refresh x)))
          (\"Blank\" ,(lambda (x) (start-screensaver)))
          (\"Exec cut\" ,(lambda (x)
                           (display (eval-string (cut-buffer)))
                           (newline)))
          (\"Redecorate all\" ,(lambda (x) (redecorate-all)))
          (\"Deiconify all\" ,(lambda (x) (deiconify-all)))
          (\"Unfocus\" ,(lambda (x) (focus-window #f)))
          #t
          (\"Restart\" ,(lambda (x) (restart)))
          (\"Quit\" ,(lambda (x) (end))))
        \"Items for the root menu.\" 'list)
"
                             mfile)
                    (display "
(defvar root-pop-list
        `((\"Quit\" ,(lambda (x) (end))))
        \"Items for the root menu.\" 'list)
"
                             mfile))
                (if windowpop
                    (display "
(defvar window-pop-list
        `((\"Raise\" ,raise-window)
          (\"Lower\" ,lower-window)
          (\"Move\" ,(lambda (w) (user-move-window w #f)))
          (\"Resize\" ,(lambda (w) (user-resize-window w #f)))
          (\"Iconify\" ,toggle-iconify-window)
          (\"Zoom\" ,(lambda (w) (zoom-window w)))
          (\"Nail\" ,(lambda (w) (virtual-toggle-nail w)))
          (\"Focus\" ,focus-window)
          (\"Refresh\" ,refresh)
          (\"Redecorate\" ,redecorate-window)
          (\"Kill\" ,(lambda (w) (or (delete-window w)
                                     (kill-window w)))))
        \"Items for the window menu.\" 'list)
"
                             mfile)
                    (display "
(defvar window-pop-list
        `((\"Kill\" ,(lambda (w) (or (delete-window w)
                                     (kill-window w)))))
        \"Items for the window menu.\" 'list)
"
                             mfile))
                (if debianpop
                    (display "
;; The generated file loaded below should contain the debian menu hierarchy
(if (file-exists? \"/etc/X11/gwm2/debian-menu.scm\")
    (load \"/etc/X11/gwm2/debian-menu.scm\")
    (define (make-debian-menu-popup)
      (construct-menu default-menu-style \"Debian Menu  (not found)\")))
"
                             mfile))
                (display "
(make-screen-var root-pop #f)
(make-screen-var window-pop #f)"
                         mfile)
                (if loginpop
                    (display "
(make-screen-var login-pop #f)"
                             mfile))
                (if commandpop
                    (display "
(make-screen-var command-pop #f)"
                             mfile))
                (if machcompop
                    (display "
(make-screen-var machine-command-pop #f)"
                             mfile))
                (if debianpop
                    (display "
(make-screen-var debian-pop #f)"
                             mfile))
                (display "

(define (update-menus)
  (root-pop (make-popup \"Root Options\" root-pop-list))
  (window-pop (make-popup \"Window Options\" window-pop-list))"
                         mfile)
                (if loginpop
                    (display "
  (login-pop (make-machine-popup \"Login\" 
                                 (lambda (h) (list \"xterm\" \"-n\" (machine-name h)))
                                 machine-list))"
                             mfile))
                (if commandpop
                    (display "
  (command-pop (make-command-popup \"Commands\" command-pop-list))"
                             mfile))
                (if machcompop
                    (display "
  (machine-command-pop (make-machine-command-popup \"Machine\" machine-list
                                                   \"Command\" command-pop-list))"
                             mfile))
                (if debianpop
                    (display "
  (debian-pop (make-debian-menu-popup))"
                             mfile))
                (display ")

;; Delay creation of menus until screen opening (of the first screen)
(add-hook! screen-opening
           (lambda (s) (if (eq? s (car (list-of-screens))) (update-menus))))

(custom-menu-install-hook \"Menurc\" update-menus)
"
                          mfile)
                (if loginpop
                    (list-set! binds 0 "(login-pop)"))
                (if machcompop
                    (list-set! binds (if (list-ref binds 0) 1 0)
                               "(machine-command-pop)"))
                (if commandpop
                    (list-set! binds (if (not (list-ref binds 0)) 0
                                         (if (list-ref binds 1) 2 1))
                               "(command-pop)"))
                (if debianpop
                    (list-set! binds (if (list-ref binds 1)
                                         (if (list-ref binds 2) 3 2)
                                         (if (list-ref binds 0) 1 0))
                               "(debian-pop)"))
                (if (eq? style 'wm2)
                    (list-set! binds (if (not (list-ref binds 0)) 0
                                         (if rootpop 5 4))
                               "(wm2-make-icon-menu)"))
                (if (or rootpop (not (list-ref binds 4)))
                    (list-set! binds 4 "(root-pop)"))
                (if winlistpop
                    (list-set! binds (if (list-ref binds 4) 5 4)
                               "(make-windows-popup \"Windows\")"))
                (display "
;; Use the menus (and bind window and icon behaviors) 
(set! window-behavior 
  (make-behavior
   (on-event (button 1 any)
             raise-lower-move-window
             :steal (check-modifiers modifiers))
   (on-event (button 3 any)
             (lambda (deco event) (menu-pop (window-pop) deco event))
             :steal (check-modifiers modifiers))
   ))

(set! icon-behavior 
  (make-behavior
   (on-event (button 1 any)
             iconify-move-window)
   (on-event (button 3 any)
             (lambda (deco event) (menu-pop (window-pop) deco event)))
   ))

(set! screen-behavior 
  (make-behavior"
                         mfile)
                (if (list-ref binds 1)
                    (begin
                      (display "
   (on (button 1 shift-mask)
       (menu-pop " mfile)
                      (display (list-ref binds 1) mfile)
                      (display " deco event))" mfile)))
                (if (list-ref binds 0)
                    (begin
                      (display "
   (on (button 1 any)
       (menu-pop " mfile)
                      (display (list-ref binds 0) mfile)
                      (display " deco event))" mfile)))
                (if (list-ref binds 3)
                    (begin
                      (display "
   (on (button 2 shift-mask)
       (menu-pop " mfile)
                      (display (list-ref binds 3) mfile)
                      (display " deco event))" mfile)))
                (if (list-ref binds 2)
                    (begin
                      (display "
   (on (button 2 any)
       (menu-pop " mfile)
                      (display (list-ref binds 2) mfile)
                      (display " deco event))" mfile)))
                (if (list-ref binds 5)
                    (begin
                      (display "
   (on (button 3 shift-mask)
       (menu-pop " mfile)
                      (display (list-ref binds 5) mfile)
                      (display " deco event))" mfile)))
                (if (list-ref binds 4)
                    (begin
                      (display "
   (on (button 3 any)
       (menu-pop " mfile)
                      (display (list-ref binds 4) mfile)
                      (display " deco event))" mfile)))
                (display "))
"
                         mfile)
                (close-port mfile)))
          (if (string=? (car files) (%search-load-path rcfile))
              (primitive-load-path rcfile)))
          )))

(define initial-configure-install
  (let ((new-hook #f)
        (done #f))
    (lambda ()
      (let ((func (lambda (s)
                    (if (not done)
                        (let ((old-hook screen-opening))
                          (set! screen-opening (make-hook 1))
                          (initial-configure)
                          (set! new-hook screen-opening)
                          (set! screen-opening old-hook)
                          (set! done #t)))
                    (modify-deco (screen)
                                 :cursor screen-cursor
                                 :behavior (make-behavior screen-behavior std-screen-behavior))
                    (run-hook new-hook s)
                    (redecorate-all))))
        (add-to-hook! screen-opening func)))))
  
