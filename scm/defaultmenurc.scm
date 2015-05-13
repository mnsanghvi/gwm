;; This file is a minimal replacement for menurc.scm if that file
;; is not found by some packages that need it.

(primitive-load-path "popups")

(defvar root-pop-list
        `(("Customize" ,(lambda (x) (custom-menu)))
          ("Restart" ,(lambda (x) (restart)))
          ("Quit" ,(lambda (x) (end))))
        "Items for the root menu." 'list)

(defvar window-pop-list
        `(("Raise" ,raise-window)
          ("Lower" ,lower-window)
          ("Iconify" ,toggle-iconify-window)
          ("Kill" ,(lambda (w) (or (delete-window w)
                                     (kill-window w)))))
        "Items for the window menu." 'list)

(make-screen-var root-pop (make-popup "Root Options" root-pop-list))
(make-screen-var window-pop (make-popup "Window Options" window-pop-list))
  
(set! screen-behavior 
  (make-behavior
   (on (button 3 any)
       (menu-pop (root-pop) deco event))))

