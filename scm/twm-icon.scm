;; twm-icon.scm --- Twm style icons.
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Twm style icons. Do '(set-icon #t twm-icon)' to use them.
;; See also twm-window.scm.
;;

;;    User customizable variables. Adjust these in your own profile.
(defvar twm-icon-foreground "black" "Default background of icons" 'color)
(defvar twm-icon-background "white" "Default foreground of icons" 'color)
(defvar twm-icon-bordercolor #f "Default bordercolor of icons" 'color)
(defvar twm-icon-font "fixed" "Font in icons" 'font)
(defvar twm-icon-default-pixmap #f "Pixmap filename for default icon" 'string)

(require 'twm-window)

(define (twm-get-icon-colors win)
  (let ((cols (or (matches-cond (window-deco win) twm-fancy-colors)
                  '(#f #f #f))))
    (set! cols (list (or (car cols) twm-icon-foreground)
                     (or (cadr cols) twm-icon-background)
                     (or (caddr cols) twm-icon-bordercolor twm-bordercolor)))
    (map (lambda (c)
           (if (string? c)
               (make-color c)
               c))
       cols)))

(define (twm-smart-icon-name win)
  (if (string=? (window-icon-name win) "icon") ; Means no icon name is specified
      (window-name win)
      (window-icon-name win)))

(define (twm-icon-get-center win colors)
  (or (window-icon-window win)
      (let ((pm (window-icon-pixmap win :background (cadr colors) :foreground (car colors)))
            (beh (make-behavior
                  (on (user-event 'icon-pixmap-change)
                      (let ((cols (get-property (top-deco deco) 'cols)))
                        (set-deco-part! deco 1
                                        (window-icon-pixmap deco
                                                            :background (cadr cols) 
                                                            :foreground (car cols))))))))
        (if pm
            (make-deco pm :behavior beh :background (make-color 'transparent))
            (if twm-icon-default-pixmap
                (make-deco (make-pixmap twm-icon-default-pixmap
                                        :background (cadr colors)
                                        :foreground (car colors))
                           :behavior beh
                           :background (make-color 'transparent))
                #f)))))

(define (twm-icon-get-label win font colors)
  (let ((beh (make-behavior
              (on (user-event 'name-change)
                  (let ((cols (get-property (top-deco deco) 'cols))
                        (font (get-property (top-deco deco) 'font)))
                    (set-deco-part! deco 1 
                                    (make-label (twm-smart-icon-name deco)
                                                :foreground (car cols)
                                                :background (cadr cols)
                                                :font font
                                                :horizontal-margin 2
                                                :vertical-margin 1)))))))
    (make-deco (make-label (twm-smart-icon-name win)
                           :foreground (car colors)
                           :background (cadr colors)
                           :font font
                           :horizontal-margin 2
                           :vertical-margin 1)
               :borderwidth twm-borderwidth
               :bordercolor (caddr colors)
               :behavior beh)))

(define (twm-icon-behavior)
  (make-behavior icon-behavior
                 std-icon-behavior))

(define (twm-icon win)
  (let ((colors (twm-get-icon-colors win))
        (font (if (string? twm-icon-font)
                  (make-font twm-icon-font)
                  twm-icon-font))
        (trans (make-color 'transparent)))
    (let ((center (twm-icon-get-center win colors))
          (label (twm-icon-get-label win font colors)))
      (if center
          (make-deco (make-deco '() center '()
                                :background trans)
                     (make-deco '() label '()
                                :background trans)
                     :background trans
                     :behavior (twm-icon-behavior)
                     :property `((cols . ,colors)
                                 (font . ,font)))
          (make-deco label
                     :behavior (twm-icon-behavior)
                     :property `((cols . ,colors)
                                 (font . ,font)))))))

