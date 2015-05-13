;; room-mgr.scm --- Simple graphical interface to the rooms package
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file provides a very simple rooms bar, trough which the user can
;; control primary and secondary rooms. Clicking on a room in the bar with
;; the left button makes it the primary room. Clicking with the right
;; button toogles the room as a secondary room. Windows in both the
;; primary room and all current secondary rooms are shown on the screen.
;;

(defvar room-mgr-xpos 200 "Upper left corner of room manager" 'integer)
(defvar room-mgr-ypos 4 "Upper left corner of room manager" 'integer)
(defvar room-mgr-button-width 75 "Room button size" 'integer)
(defvar room-mgr-button-height 16 "Room button size" 'integer)
(defvar room-mgr-font "8x13" "Font in room buttons" 'font)
(defvar room-mgr-background "white" "Background color of inactive room buttons" 'color)
(defvar room-mgr-active-background "gray" "Background color of active room buttons" 'color)
(defvar room-mgr-foreground "black" "Foreground color of room buttons" 'color)
(defvar room-mgr-borderwidth 2 "Border width of room buttons" 'integer)
(defvar room-mgr-dir-horiz #t "Controls position of room buttons." 'boolean)
(defvar room-mgr-dir-len 2 "Number of buttons in each (horizontal or vertical) row." 'integer)
(defvar room-mgr-tile 'transparent "Tile of empty positions" 'color)


(require 'rooms)

(define (get-room-mgr)
  (or (get-property (root-window) 'room-mgr)
      (let ((a (make-vector 3 #f)))
        (vector-set! a 2 initial-rooms)
        (set-property! (root-window) 'room-mgr a)
        a)))

(define (room-button-behavior room bg1 bg2 bg3)
  (make-behavior
   (on (button 1 any) 
       (room-switch-primary room))
   (on (button 3 any)
       (room-toggle-secondary room))
   (on (user-event 'recolor)
       (cond ((eq? room (get-primary-room))
              (set-deco-background! deco bg1))
             ((member room (get-secondary-rooms))
              (set-deco-background! deco bg2))
             (#t
              (set-deco-background! deco bg3))))))

(define (room-mgr-make-button-plug room font fg bg1 bg2 bg3)
  (make-deco '()
             (make-label (symbol->string room)
                         :font font
                         :foreground fg
                         :background (make-color 'transparent))
             '()
             :width room-mgr-button-width
             :height room-mgr-button-height
             :direction 'horizontal
             :behavior (room-button-behavior room bg1 bg2 bg3)))

(define (room-mgr-make-button-space tl)
  (make-deco :width room-mgr-button-width
             :height room-mgr-button-height
             :background tl))

(define (room-mgr-make-vborder ele1 ele2 fg tl)
  (make-deco '() 
             :width room-mgr-borderwidth
             :background (if (or ele1 ele2) fg tl)))

(define (room-mgr-count-runs lst i1 i2 step num)
  (let ((len (length lst))
        (res '())
        (n 0)
        (i 0)
        (last #t))
    (while (< i num)
      (while (and (< i num)
                  (eq? last (if (or (and (>= i1 0) (< i1 len) (list-ref lst i1))
                                    (and (< i2 len) (list-ref lst i2)))
                                #t #f)))
        (set! n (+ n 1))
        (set! i (+ i 1))
        (set! i1 (+ i1 step))
        (set! i2 (+ i2 step)))
      (set! res (cons n res))
      (set! n 0)
      (set! last (not last)))
    (reverse! res)))

(define (room-mgr-make-hborder runs fg tl)
  (let ((last #f)
        (lens #f))
    (if (= (car runs) 0)
        (begin
          (set! runs (cdr runs))
          (set! last #t)))
    (set! lens (map (lambda (n)
                      (set! last (not last))
                      (cons (+ (* n (+ room-mgr-button-width
                                       room-mgr-borderwidth))
                               (if last
                                   room-mgr-borderwidth
                                   (- room-mgr-borderwidth)))
                            last))
                    runs))
    (let ((last (car (reverse lens)))
          (first (car lens)))
      (if (cdr first)
          (set-car! first (- (car first) room-mgr-borderwidth)))
      (if (cdr last)
          (set-car! last (- (car last) room-mgr-borderwidth))))
    (apply make-deco (map (lambda (ele)
                           (make-deco :width (car ele)
                                      :height room-mgr-borderwidth
                                      :background (if (cdr ele) fg tl)))
                         lens))))

(define (room-mgr-make-row room-lst n step num font fg bg1 bg2 bg3 tl)
  (let ((res '())
        (i 0)
        (len (length room-lst))
        (d1 (list-ref room-lst n))
        (d2 #f))
    (while (< i num)
      (set! res (cons (if d1
                          (room-mgr-make-button-plug d1 font fg bg1 bg2 bg3)
                          (room-mgr-make-button-space tl))
                      res))
      (set! n (+ n step))
      (set! i (+ i 1))
      (set! d2 d1)
      (set! d1 (if (or (= i num) (>= n len)) #f (list-ref room-lst n)))
      (if (and (> room-mgr-borderwidth 0)
               (not (= i num)))
          (set! res (cons (room-mgr-make-vborder d2 d1 fg tl)
                          res))))
    (apply make-deco (reverse res))))

(define (room-mgr-make-menu room-lst)
  (let* ((num (length room-lst))
         (rows (if room-mgr-dir-horiz
                   (+ (quotient (- num 1) room-mgr-dir-len) 1)
                   (min num room-mgr-dir-len)))
         (cols (if room-mgr-dir-horiz
                   (min num room-mgr-dir-len)
                   (+ (quotient (- num 1) room-mgr-dir-len) 1)))
         (step (if room-mgr-dir-horiz
                   1 room-mgr-dir-len))
         (bstep (if room-mgr-dir-horiz
                    room-mgr-dir-len 1))
         (font (if (string? room-mgr-font)
                   (make-font room-mgr-font)
                   room-mgr-font))
         (fg (if (string? room-mgr-foreground)
                 (make-color room-mgr-foreground)
                 room-mgr-foreground))
         (bg3 (if (string? room-mgr-background)
                  (make-color room-mgr-background)
                  room-mgr-background))
         (bg1 (if (string? room-mgr-active-background)
                  (make-color room-mgr-active-background)
                  room-mgr-active-background))
         (bg2 (make-pixmap "gray.xbm" :foreground bg1 :background bg3))
         (tile (cond ((eq? room-mgr-tile 'transparent)
                      (make-color 'hole))
                     ((string? room-mgr-tile)
                      (make-pixmap bg room-mgr-tile fg))
                     (room-mgr-tile
                      room-mgr-tile)
                     (#t
                      bg)))
         (res '())
         (i 0)
         (n 0))
    (while (< i rows)
      (set! res (cons (room-mgr-make-row room-lst n step cols font fg bg1 bg2 bg3 tile)
                      res))
      (set! i (+ i 1))
      (set! n (+ n bstep))
      (if (and (> room-mgr-borderwidth 0)
               (not (= i rows)))
          (set! res (cons (room-mgr-make-hborder (room-mgr-count-runs room-lst (- n bstep) n step cols)
                                                 fg tile)
                          res))))
    (set! res (cons room-mgr-borderwidth (cons :borderwidth res)))
    (apply make-deco (reverse res))))

(define (room-mgr-show)
  (let ((room-mgr (get-room-mgr)))
    (if (and room-mgr
             (vector-ref room-mgr 0)
             (deco-valid? (vector-ref room-mgr 0)))
        (let ((menu (vector-ref room-mgr 0)))
          (vector-set! room-mgr 1 (get-menu-gpos menu))
          (delete-window (top-deco menu))))
    (if (and room-mgr
             (vector-ref room-mgr 2)
             (> (length (vector-ref room-mgr 2)) 0))
        (let ((menu (room-mgr-make-menu (vector-ref room-mgr 2)))
              (gpos (or (vector-ref room-mgr 1)
                        (coord->gpos room-mgr-xpos room-mgr-ypos))))
          (vector-set! room-mgr 0 menu)
          (place-menu-gpos menu
                           "room-mgr"
                           gpos)
          (send-user-event 'recolor menu)))))

(define (add-room room)
  (let* ((mgr (get-room-mgr))
         (lst (vector-ref mgr 2)))
    (if (not (member room lst))
        (begin
          (vector-set! mgr 2 (append lst (list room)))
          (room-mgr-show)))))

(define (remove-room room)
  (let* ((mgr (get-room-mgr))
         (lst (vector-ref mgr 2)))
    (if (member room lst)
        (begin
          (vector-set! mgr 2 (delq room lst))
          (room-mgr-show)))))

(advice room-switch-primary 'rooms 'after
  (let ((room-mgr (get-room-mgr)))
    (if (and room-mgr
             (vector-ref room-mgr 0)
             (deco-valid? (vector-ref room-mgr 0)))
        (send-user-event 'recolor (vector-ref room-mgr 0)))))

(advice room-open-secondary 'rooms 'after
  (let ((room-mgr (get-room-mgr)))
    (if (and room-mgr
             (vector-ref room-mgr 0)
             (deco-valid? (vector-ref room-mgr 0)))
        (send-user-event 'recolor (vector-ref room-mgr 0)))))

(advice room-close-secondary 'rooms 'after
  (let ((room-mgr (get-room-mgr)))
    (if (and room-mgr
             (vector-ref room-mgr 0)
             (deco-valid? (vector-ref room-mgr 0)))
        (send-user-event 'recolor (vector-ref room-mgr 0)))))

(advice room-toggle-secondary 'rooms 'after
  (let ((room-mgr (get-room-mgr)))
    (if (and room-mgr
             (vector-ref room-mgr 0)
             (deco-valid? (vector-ref room-mgr 0)))
        (send-user-event 'recolor (vector-ref room-mgr 0)))))

(add-to-hook! screen-opening (lambda (s) (room-mgr-show)))

(custom-menu-install-hook '("Rooms" "Room Manager") room-mgr-show)


(define room-mgr #t)
