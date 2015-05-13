;; wm2-window.scm --- Wm2 style windows, icons and menus.
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file implements wm2 style windows, icons and menus. (That is,
;; there are no real icons in this style - iconified windows are listed
;; in a menu.) To use this, do:
;;   (set-window #t wm2-window)
;;   (set-icon #t wm2-icon)
;;   (set! default-menu-style wm2-menu-context)
;; The function (wm2-make-icon-menu) constructs the standard wm2 menu.
;;

;;    User customizable variables. Adjust these in your own profile.
(defvar wm2-color "gray80" "Main color of window" 'color)
(defvar wm2-font "-*-lucida-bold-r-*-*-14-*-75-75-*-*-*-*" "Font of title text" 'font)
(defvar wm2-terminal-command "xterm" "Command to start a new terminal" '(string list))

(define wm2-icon-list '())

(define (wm2-lighten col)
  (let ((rgb (color-components col))
        (f (lambda (x) (+ 49152 (quotient x 4)))))
    (make-color (f (car rgb)) (f (cadr rgb)) (f (caddr rgb)))))

(define (wm2-plug-behavior)
  (make-behavior (on (user-event 'focus-in)
                     (set-deco-part! deco 1 (get-property deco 'pa)))
                 (on (user-event 'focus-out)
                     (set-deco-part! deco 1 (get-property deco 'pi)))))

(define (wm2-back-behavior)
  (make-behavior (on (user-event 'focus-in)
                     (set-deco-background! deco (get-property deco 'pa)))
                 (on (user-event 'focus-out)
                     (set-deco-background! deco (get-property deco 'pi)))))

(define (wm2-square-behavior)
  (let ((kill #f)
        (done #t))
    (make-behavior (on (buttonpress any any)
                       (set! kill #f)
                       (set! done #f)
                       (send-timer-event 'wm2-kill deco 1.0))
                   (on (user-event 'wm2-kill)
                       (if (not done)
                           (begin
                            (set! kill #t)
                            (set-deco-cursor! deco (make-cursor 88)))))
                   (on (buttonrelease any any)
                       (let ((x (event-relative-x event))
                             (y (event-relative-y event)))
                         (set! done #t)
                         (if (and (>= x 0)
                                  (>= y 0)
                                  (< x (deco-width deco))
                                  (< y (deco-width deco)))
                             (if kill
                                 (delete-window (top-deco deco))
                               (iconify-window (top-deco deco)))
                             (if kill
                                 (set-deco-cursor! deco #f)))))
                   (on (user-event 'focus-in)
                       (show-deco deco))
                   (on (user-event 'focus-out)
                       (hide-deco deco)))))

(define (wm2-corner-behavior)
  (make-behavior (on (user-event 'focus-in)
                     (show-deco deco))
                 (on (user-event 'focus-out)
                     (hide-deco deco))
                 (on (buttonpress any any)
                     (user-resize-window (top-deco deco) event
                                         :corner-size 14 :opaque #t))))

(define (wm2-edge-behavior)
  (make-behavior (on (user-event 'focus-in)
                     (show-deco deco))
                 (on (user-event 'focus-out)
                     (hide-deco deco))))

(define (wm2-title-behavior)
  (make-behavior (on (user-event 'name-change)
                     (set-deco-part! deco 1
                                     (wm2-make-title (top-deco deco)
                                                     (get-property deco 'cols)
                                                     (get-property deco 'font))))))

(define (wm2-in-square win x y)
  (and win
       (deco-valid? win)
       (> x (deco-x win))
       (> y (deco-y win))
       (< x (+ (deco-x win) 20))
       (< y (+ (deco-y win) 20))))

(define wm2-window-behavior
  (let ((entered #f))
    (lambda ()
      (make-behavior (on (leave)
                         (if (and (eq? entered deco)
                                  (not (wm2-in-square deco
                                                      (event-x event)
                                                      (event-y event))))
                             (begin
                               (set! entered #f)
                               (run-hook leave-window-hook deco))))
                     (on (enter)
                         (if (and (not (eq? entered deco))
                                  (not (wm2-in-square entered
                                                      (event-x event)
                                                      (event-y event))))
                             (begin
                               (set! entered deco)
                               (run-hook enter-window-hook deco))))))))

(define (wm2-topleft-pixmap cols)
  (let ((black (caddr cols))
        (trans (make-color 'transparent))
        (p (make-pixmap 26 20 :background (car cols))))
    (draw-line p 0 0 0 19 :color black)
    (draw-line p 1 0 25 0 :color black)
    (draw-rectangle p 4 4 13 13 :background trans :foreground black :borderwidth 1)
    (draw-rectangle p 21 4 5 16 :background trans :foreground black :borderwidth 1)
    p))

(define (wm2-topright-pixmap-i cols)
  (let ((black (caddr cols))
        (p (make-pixmap 9 7 :background (make-color 'hole))))
    (draw-line p 0 0 0 3 :color black)
    p))

(define (wm2-topright-pixmap-a cols)
  (let ((black (caddr cols))
        (p (make-pixmap 9 7 :background (cadr cols))))
    (draw-line p 0 0 8 0 :color black)
    (draw-line p 0 1 0 3 :color black)
    (draw-line p 8 1 8 6 :color black)
    p))

(define (wm2-bottomleft-pixmap-i cols)
  (let ((p (make-pixmap 26 1 :background (make-color 'transparent))))
    p))

(define (wm2-bottomleft-pixmap-a cols)
  (let ((p (make-pixmap 26 1 :background (make-color 'transparent))))
    (draw-line p 20 0 25 0 :color (caddr cols))
    p))

(define (wm2-bottomright-pixmap cols)
  (let ((black (caddr cols))
        (trans (make-color 'transparent))
        (p (make-pixmap 14 14 :background (cadr cols))))
    (draw-polygon p 0 0 14 0 0 14 :color trans)
    (draw-line p 13 0 0 13 :color black)
    (draw-line p 13 3 3 13 :color black)
    p))

(define (wm2-top-pixmap-i cols)
  (let ((black (caddr cols))
        (p (make-pixmap 1 7 :background (car cols))))
    (draw-point p 0 0 :color black)
    (draw-point p 0 3 :color black)
    (draw-line p 0 4 0 6 :color (make-color 'transparent))
    p))

(define (wm2-top-pixmap-a cols)
  (let ((black (caddr cols))
        (p (make-pixmap 1 7 :background (car cols))))
    (draw-point p 0 0 :color black)
    (draw-point p 0 3 :color black)
    (draw-line p 0 4 0 6 :color (cadr cols))
    p))

(define (wm2-left-pixmap-i cols)
  (let ((p (make-pixmap 26 1 :background (make-color 'transparent))))
    p))

(define (wm2-left-pixmap-a cols)
  (let ((p (make-pixmap 26 1 :background (make-color 'transparent))))
    (draw-point p 20 0 :color (caddr cols))
    (draw-line p 21 0 25 0 :color (cadr cols))
    p))

(define (wm2-title-pixmap cols)
  (let ((black (caddr cols))
        (p (make-pixmap 21 1 :background (car cols))))
    (draw-point p 0 0 :color black)
    (draw-point p 20 0 :color black)
    p))

(define (wm2-titlebot-pixmap cols)
  (let ((black (caddr cols))
        (trans (make-color 'hole))
        (p (make-pixmap 21 17 :background (car cols))))
    (draw-polygon p 0 0 1 0 18 17 0 17 :color trans)
    (draw-line p 1 0 17 16 :color black)
    (draw-line p 18 16 20 16 :color black)
    (draw-line p 20 0 20 16 :color black)
    p))

(define (wm2-make-plug pi pa)
  (make-deco pi 
             :background (make-color 'transparent)
             :property `((pi . ,pi)
                         (pa . ,pa))
             :behavior (wm2-plug-behavior)))

(define (wm2-make-square cols)
  (let ((d (make-deco :width 7
                      :height 7
                      :borderwidth 1
                      :background (cadr cols)
                      :foreground (caddr cols)
                      :behavior (wm2-square-behavior)
                      :anchor '(6 6))))
    (hide-deco d)
    d))

(define (wm2-make-corner p w)
  (let* ((bw (deco-borderwidth w))
         (anchor (list (- -1 bw) (- -1 bw)))
         (d (make-deco p
                      :background (make-color 'transparent)
                      :behavior (wm2-corner-behavior)
                      :anchor anchor)))
    (hide-deco d)
    d))

(define (wm2-make-edge cols hori)
  (let* ((anchor (if hori
                     '(0 (1 -1 0) -1 -1)
                     '((1 -1 0) 7 -1 -1)))
         (d (make-deco '()
                      :background (caddr cols)
                      :behavior (wm2-edge-behavior)
                      :anchor anchor)))
    (hide-deco d)
    d))

(define (wm2-make-title w cols font)
  (make-label (window-name w)
              :font font
              :angle -90
              :background (car cols)
              :foreground (caddr cols)
              :vertical-margin 2))

(define wm2-window
  (let ((last-color "")
        (last-font "")
        (col #f)
        (light #f)
        (cols #f)
        (font #f)
        (trans #f)
        (tl #f)
        (tri #f)
        (tra #f)
        (bli #f)
        (bla #f)
        (br #f)
        (ti #f)
        (ta #f)
        (li #f)
        (la #f)
        (tit #f)
        (tib #f))
    (lambda (w)
      (if (not (string=? last-color wm2-color))
          (begin
            (set! col (make-color wm2-color))
            (set! light (wm2-lighten col))
            (set! cols (list col light (make-color "black")))
            (set! trans (make-color 'transparent))
            (set! tl (wm2-topleft-pixmap cols))
            (set! tri (wm2-topright-pixmap-i cols))
            (set! tra (wm2-topright-pixmap-a cols))
            (set! bli (wm2-bottomleft-pixmap-i cols))
            (set! bla (wm2-bottomleft-pixmap-a cols))
            (set! br (wm2-bottomright-pixmap cols))
            (set! ti (wm2-top-pixmap-i cols))
            (set! ta (wm2-top-pixmap-a cols))
            (set! li (wm2-left-pixmap-i cols))
            (set! la (wm2-left-pixmap-a cols))
            (set! tit (wm2-title-pixmap cols))
            (set! tib (wm2-titlebot-pixmap cols))
            (set! last-color wm2-color)))
      (if (not (string=? last-font wm2-font))
          (begin
            (set! font (make-font wm2-font))
            (set! last-font wm2-font)))
      (if (and (= (deco-borderwidth w) 0)
               (not (deco-shaped? w)))
          (set-deco-borderwidth! w 1))
      (make-deco (make-deco tl
                            (make-deco (make-deco (wm2-make-title w cols font)
                                                  tib
                                                  :width 21
                                                  :background tit
                                                  :property `((cols . ,cols)
                                                              (font . ,font))
                                                  :behavior (wm2-title-behavior))
                                       '()
                                       :background trans)
                            '()
                            (wm2-make-plug bli bla)
                            (wm2-make-square cols)
                            :background li
                            :property `((pi . ,li)
                                        (pa . ,la))
                            :behavior (wm2-back-behavior))
                 (make-deco (make-deco '() 
                                       (wm2-make-plug tri tra)
                                       :background ti
                                       :property `((pi . ,ti)
                                                   (pa . ,ta))
                                       :behavior (wm2-back-behavior))
                            w
                            (wm2-make-corner br w)
                            (wm2-make-edge cols #f)
                            (wm2-make-edge cols #t)
                            :background trans)
                 :background trans
                 :behavior (make-behavior (wm2-window-behavior)
                                          window-behavior
                                          std-window-behavior)
                 :direction 'horizontal))))

(define (wm2-icon win)
  (let ((res (make-deco :property `((hidden wm2-icon)))))
    (hide-deco res)
    res))

(define (wm2-add-icon i)
  (if (not (member (window-deco i) wm2-icon-list))
      (set! wm2-icon-list (append! wm2-icon-list (list (window-deco i))))))

(define (wm2-remove-icon i)
  (if (member (window-deco i) wm2-icon-list)
      (set! wm2-icon-list (delete! (window-deco i) wm2-icon-list))))

(define (wm2-menu-item-make label action)
  (let* ((menu (if (deco? action) action #f))
         (font (make-font wm2-font))
         (black (make-color "black"))
         (col (make-color wm2-color))
         (l1 (make-label label :font font :horizontal-margin 2 :vertical-margin 1 :background col :foreground black))
         (l2 (make-label label :font font :horizontal-margin 2 :vertical-margin 1 :background black :foreground col)))
    (make-deco (make-deco '() :width 1 :background black)
               (make-deco l1 '()
                          :direction 'horizontal
                          :borderwidth 1 
                          :background col
                          :bordercolor col
                          :behavior (make-behavior 
                                     (on (user-event 'active)
                                         (set-deco-part! deco 1 l2)
                                         (set-deco-background! deco black))
                                     (on (user-event 'inactive)
                                         (set-deco-part! deco 1 l1)
                                         (set-deco-background! deco col))))
               (make-deco '() :width 1 :background black)
               :direction 'horizontal
               :behavior std-menu-item-behavior
               :property `((action . ,(if menu #f action))
                           (menu . ,menu)))))

(define (wm2-menu-label-make label)
  (let* ((font (make-font wm2-font))
         (black (make-color "black"))
         (col (make-color wm2-color))
         (l1 (make-label label :font font :horizontal-margin 2 :vertical-margin 1 :background col :foreground black)))
    (make-deco (make-deco '() :height 1 :background black)
               (make-deco '() l1 '()
                          :background col)
               (make-deco '() :height 1 :background black)
               :direction 'vertical
               :background col
               :behavior std-menu-label-behavior
               :property '((wm2-label . #t)))))

(define (wm2-menu-separator-make)
  (let ((black (make-color "black")))
    (make-deco
     (make-deco '() :height 1 :background black)
     '()
     (make-deco '() :height 1 :background black)
     :background (make-color wm2-color)
     :direction 'vertical
     :height 4
     :property '((wm2-label . #t)))))

(define (wm2-menu-make . args)
  (let ((len (length args))
        (lst args)
        (llab #t)
        (black (make-color "black"))
        (col (make-color wm2-color)))
    (while (not (null? lst))
      (if (and (deco? (car lst))
               (get-property (car lst) 'wm2-label))
          (begin
            (if llab
                (hide-deco (deco-part (car lst) 1)))
            (if (and (not (null? (cdr lst)))
                     (deco? (cadr lst))
                     (get-property (cadr lst) 'wm2-label))
                (hide-deco (deco-part (car lst) 3)))
            (set! llab #t))
          (set! llab #f))
      (set! lst (cdr lst)))
    (make-deco 
     (apply make-deco (append (if (or (null? args)
                                      (and (deco? (car args))
                                           (get-property (car args) 'wm2-label)))
                                  '()
                                  (list (make-deco '() :height 1 :background black)))
                              args
                              (if llab
                                  '()
                                  (list (make-deco '() :height 1 :background black)))
                              (list :margin 2
                                    :background col)))
     :background col
     :foreground black
     :borderwidth 1
     :direction 'horizontal
     :margin 2
     :behavior std-menu-behavior)))

(define wm2-menu-context
  (list :menu-func wm2-menu-make
        :item-func wm2-menu-item-make
        :label-func wm2-menu-label-make
        :separator-func wm2-menu-separator-make
        :foreground (lambda () "black")
        :background (lambda () wm2-color)
        :font (lambda () wm2-font)))

(define (wm2-make-icon-menu)
  (apply construct-menu
         (append (list default-menu-style 
                       #t
                       (list "New" (if (string? wm2-terminal-command)
                                       (lambda (e) (execute wm2-terminal-command))
                                       (lambda (e) (apply execute wm2-terminal-command))))
                       #t)
                 (map (lambda (x) 
                        (list (window-name x)
                              (lambda (e)
                                (pop-to-window x))))
                      wm2-icon-list)
                 (list #t
                       (list "Exit" (lambda (e) (end)))))))

(advice (iconify-window w) 'wm2-icon 'after (wm2-add-icon w))
(advice (deiconify-window w) 'wm2-icon 'after (wm2-remove-icon w))
