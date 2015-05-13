;; twm-window.scm --- Twm style windows.
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Twm style windows. Do '(set-window #t twm-window)' to use them.
;;

;;    User customizable variables. Adjust these in your own profile.
(defvar twm-borderwidth 2 "Borderwidth of most windows" 'integer)
(defvar twm-bordercolor "black" "Color of border (inactive window)" 'color)
(defvar twm-active-bordercolor "white" "Color of active window border" 'color)
(defvar twm-title-background "white" "Default background of titlebar" 'color)
(defvar twm-title-foreground "black" "Default foreground of titlebar" 'color)
(defvar twm-title-font "9x15" "Font in titlebar" 'font)
(defvar twm-fancy-colors '()
  "List of (wintype foreground background border activeborder) specs" 'list)
(defvar twm-notitle-list (list 'Gwm window-transient-for)
  "Untitled windows" 'list)
(defvar twm-active-pixmap "gray" "Filename for active titlebar pattern" 'string)
(defvar twm-left-plugs `(("iconify2" ,(lambda (w e) (iconify-window w))))
  "Left plugs in the titlebar, a list of pairs or tripplets:  ( <pixmap-file> <action> [<on-press>] )" 'list)
(defvar twm-right-plugs `(("resize2" ,(lambda (w e) (twm-resize-window w e)) #t))
  "Right plugs in the titlebar, a list of pairs or tripplets:  ( <pixmap-file> <action> [<on-press>] )" 'list)

(defvar twm-squeezed-title #f "Whether titles should be squeezed" 'boolean)
(defvar twm-squeezed-hilite-size 11 "Size of area showing focus in squeezed titles" 'integer)
(defvar twm-squeezed-title-position 'left "Position of squeezed title, left, right or center" '(left right center))

;;--------------------------------------------------------------------------
;;   End of user customizable things. Here starts the real code.
;;--------------------------------------------------------------------------


(define (twm-resize-window win ev)
  (user-resize-window win ev
                      :cursor (make-cursor 52)
                      :corner-size 1))

(define (twm-get-window-colors win)
  (let ((cols (or (matches-cond win twm-fancy-colors)
                  '(#f #f #f #f))))
    (set! cols (list (or (car cols) twm-title-foreground)
                     (or (cadr cols) twm-title-background)
                     (or (caddr cols) twm-bordercolor)
                     (or (cadddr cols) twm-active-bordercolor twm-bordercolor)))
    (map (lambda (c)
           (if (string? c)
               (make-color c)
               c))
       cols)))

(define (twm-titlebar-plug pixmap-file expr press colors)
  (let ((pixmap (if (pixmap? pixmap-file)
                    pixmap-file
                    (make-pixmap pixmap-file
                                 :background (cadr colors)
                                 :foreground (car colors))))
        (beh (if (behavior? expr)
                 expr
                 (make-behavior
                  (if press
                      (on-event (button any alone)
                                (lambda (w e) (expr (top-deco w) e)))
                      (on-event (button any alone)
                                (lambda (w e) #f)
                                (lambda (w e) (expr (top-deco w) e))))))))
    (make-deco pixmap :background (cadr colors) :behavior beh)))

(define (twm-make-pluglist lst colors)
  (map (lambda (ele)
         (twm-titlebar-plug (car ele) (cadr ele)
                            (if (null? (cddr ele)) #f (caddr ele))
                            colors))
       lst))

(define (twm-make-name-label name font colors)
  (make-label name
              :background (cadr colors)
              :foreground (car colors)
              :font font
              :horizontal-margin 6
              :vertical-margin 1))

(define (twm-middle-bar name font colors squeeze)
  (let ((normal-pm (make-pixmap twm-active-pixmap :foreground (cadr colors)
                                                  :background (cadr colors)))
        (hilit-pm (make-pixmap twm-active-pixmap :foreground (car colors)
                                                 :background (cadr colors)))
        (beh (make-behavior
              (on (user-event 'focus-in)
                  (set-deco-background! (deco-part deco 2)
                                        (get-property deco 'hp)))
              (on (user-event 'focus-out)
                  (set-deco-background! (deco-part deco 2)
                                        (get-property deco 'np)))
              (on (user-event 'name-change)
                  (set-deco-part! deco 1 
                                  (twm-make-name-label (window-name (top-deco deco))
                                                       (get-property (top-deco deco) 'font)
                                                       (get-property (top-deco deco) 'cols)))))))
    (make-deco (twm-make-name-label name font colors) 
               (if squeeze
                   (make-deco '() 
                              :width twm-squeezed-hilite-size
                              :background normal-pm
                              :borderwidth 2
                              :bordercolor (cadr colors))
                   (make-deco '() 
                              :background normal-pm
                              :borderwidth 2
                              :bordercolor (cadr colors)))
               :background (cadr colors)
               :separator 3
               :margin 3
               :direction 'horizontal
               :behavior beh
               :property `((np . ,normal-pm)
                           (hp . ,hilit-pm)))))

(define (twm-titlebar name font colors squeeze)
  (apply make-deco (append (twm-make-pluglist twm-left-plugs colors)
                           (list (twm-middle-bar name font colors squeeze))
                           (twm-make-pluglist twm-right-plugs colors)
                           (list :background (cadr colors)
                                 :separator 2
                                 :margin 2
                                 :direction 'horizontal))))

(define (twm-borderbar colors horiz)
  (let ((beh (make-behavior
              (on (user-event 'focus-in)
                  (set-deco-background! deco (get-property (top-deco deco) 'hbc)))
              (on (user-event 'focus-out)
                  (set-deco-background! deco (get-property (top-deco deco) 'nbc))))))
    (if horiz
        (make-deco '()
                   :background (caddr colors)
                   :behavior beh
                   :height twm-borderwidth)
        (make-deco '()
                   :background (caddr colors)
                   :behavior beh
                   :width twm-borderwidth))))

(define (twm-window-behavior)
  (make-behavior
   window-behavior
   (make-behavior
    (on (user-event 'focus-out)
        (set-deco-bordercolor! deco (get-property (top-deco deco) 'nbc)))
    (on (user-event 'focus-in)
        (set-deco-bordercolor! deco (get-property (top-deco deco) 'hbc))))
   std-window-behavior
   ))

;; TWM Titled Window

(define (twm-titled-window win)
  (let ((font (if (string? twm-title-font)
                  (make-font twm-title-font)
                  twm-title-font))
        (colors (twm-get-window-colors win)))
    (modify-deco win :borderwidth 0)
    (if (> twm-borderwidth 0)
        (make-deco (twm-titlebar (window-name win) font colors #f)
                   (twm-borderbar colors #t)
                   win
                   :behavior (twm-window-behavior)
                   :borderwidth twm-borderwidth
                   :property `((nbc . ,(caddr colors))
                               (hbc . ,(cadddr colors))
                               (cols . ,colors)
                               (font . ,font)))
        (make-deco (twm-titlebar (window-name win) font colors #f)
                   win
                   :behavior (twm-window-behavior)
                   :borderwidth 0
                   :property `((nbc . ,(caddr colors))
                               (hbc . ,(cadddr colors))
                               (cols . ,colors)
                               (font . ,font))))))

;; TWM Squeezed Window

(define (twm-squeezed-window win)
  (let ((font (if (string? twm-title-font)
                  (make-font twm-title-font)
                  twm-title-font))
        (colors (twm-get-window-colors win))
        (trans (make-color 'transparent)))
    (modify-deco win :borderwidth 0)
    (if (> twm-borderwidth 0)
        (let ((top (make-deco (twm-borderbar colors #t)
                              (make-deco (twm-borderbar colors #f)
                                         (twm-titlebar (window-name win) font colors #t)
                                         (twm-borderbar colors #f))))
              (bot (make-deco (make-deco (twm-borderbar colors #f)
                                         win
                                         (twm-borderbar colors #f))
                              (twm-borderbar colors #t)))
              (mid (twm-borderbar colors #t)))
          (if (eq? twm-squeezed-title-position 'right)
              (begin
                (set! top (make-deco '() top :background trans))
                (set! bot (make-deco '() bot :background trans)))
              (if (eq? twm-squeezed-title-position 'center)
                  (begin
                    (set! top (make-deco '() top '() :background trans))
                    (set! bot (make-deco '() bot '() :background trans)))
                  (begin
                    (set! top (make-deco top '() :background trans))
                    (set! bot (make-deco bot '() :background trans)))))
          (make-deco top mid bot
                     :behavior (twm-window-behavior)
                     :borderwidth 0
                     :background (make-color 'transparent)
                     :property `((nbc . ,(caddr colors))
                                 (hbc . ,(cadddr colors))
                                 (cols . ,colors)
                                 (font . ,font))))
        (let ((top (twm-titlebar (window-name win) font colors #t))
              (bot #f))
          (if (eq? twm-squeezed-title-position 'right)
              (begin
                (set! top (make-deco '() top :background trans))
                (set! bot (make-deco '() win :background trans)))
              (if (eq? twm-squeezed-title-position 'center)
                  (begin
                    (set! top (make-deco '() top '() :background trans))
                    (set! bot (make-deco '() win '() :background trans)))
                  (begin
                    (set! top (make-deco top '() :background trans))
                    (set! bot (make-deco win '() :background trans)))))
          (make-deco top bot
                     :behavior (twm-window-behavior)
                     :borderwidth 0
                     :background (make-color 'transparent)
                     :property `((nbc . ,(caddr colors))
                                 (hbc . ,(cadddr colors))
                                 (cols . ,colors)
                                 (font . ,font)))))))

;; TWM Simple Window

(define (twm-simple-window win)
  (let ((colors (twm-get-window-colors win)))
    (modify-deco win :borderwidth 0)
    (make-deco win
               :behavior (twm-window-behavior)
               :borderwidth twm-borderwidth
               :bordercolor (caddr colors)
               :property `((nbc . ,(caddr colors))
                           (hbc . ,(cadddr colors))))))

;; TWM Window 

(define (twm-window win)
  (if (matches-list win twm-notitle-list)
      (twm-simple-window win)
      (if twm-squeezed-title
          (twm-squeezed-window win)
          (twm-titled-window win))))

