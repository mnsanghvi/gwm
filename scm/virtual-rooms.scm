;; virtual-rooms.scm --- Map of a virtual screen per room
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file provides a map of the virtual screen for each room.
;;
;; By default, the left button in the map moves to that room and
;; virtual screen area, the middle button can drag a window
;; in the map to a new position or to another room, and the right
;; button toggles a secondary room.
;;

;;    User customizable variables. Adjust these in your own profile.
(defvar show-virtual-rooms #t "Show the rooms virtual screen maps" 'boolean)
(defvar virtual-rooms-xpos 0 "original position of map" 'integer)
(defvar virtual-rooms-ypos 0 "original position of map" 'integer)
(defvar virtual-rooms-xsize 80 "size of the map" 'integer)
(defvar virtual-rooms-ysize 60 "size of the map" 'integer)

(require 'rooms)
(require 'virtual)
(defaults-to show-virtual #f) ;; use virtual-rooms map instead
(require 'virtual-map)

(define (virms-room-at x y)
  (let* ((virms-map (get-property (root-window) 'virms-map))
         (maplst (and virms-map (vector-ref virms-map 1)))
         (ind (quotient x (+ virtual-rooms-xsize 1))))
    (if (and maplst
             (>= ind 0)
             (< ind (length maplst))
             (>= y 0)
             (< y virtual-rooms-ysize)
             (>= x (* ind (+ virtual-rooms-xsize 1)))
             (< x (+ (* ind (+ virtual-rooms-xsize 1)) virtual-rooms-xsize)))
        (car (list-ref maplst ind))
        #f)))

(define (virms-room-ind-at x y)
  (let* ((ind (quotient x (+ virtual-rooms-xsize 1))))
    (if (and (>= ind 0)
             (>= y 0)
             (< y virtual-rooms-ysize)
             (>= x (* ind (+ virtual-rooms-xsize 1)))
             (< x (+ (* ind (+ virtual-rooms-xsize 1)) virtual-rooms-xsize)))
        ind
        #f)))

(define (virms-room-at-ind ind)
  (let* ((virms-map (get-property (root-window) 'virms-map))
         (maplst (and virms-map (vector-ref virms-map 1))))
    (if (and ind maplst (< ind (length maplst)))
        (car (list-ref maplst ind))
        #f)))
  
(define (virms-visible? w rlst)
  (let ((hid (window-hidden? w))
        (room (or (room-get-window-room w)
                  (get-primary-room))))
    (and (virt-drawable w)
         (not (window-iconified? w))
         (or (not hid) 
             (and (equal? hid '(rooms))))
         (memq room rlst))))

(define (virms-window-at-position room x y)
  (let* ((wins (reverse (list-of-windows 'window 'stacking-order))))
    (while (and (not (null? wins))
                (or (not (virms-visible? (car wins) (list room)))
                    (< x (window-x (car wins)))
                    (>= x (+ (window-x (car wins)) (window-width (car wins))))
                    (< y (window-y (car wins)))
                    (>= y (+ (window-y (car wins)) (window-height (car wins))))))
      (set! wins (cdr wins)))
    (if (null? wins) #f (car wins))))

(define (virms-calc-params rlst)
  ;; Return a list that contains scale factor, x position of origin,
  ;; and y position of origin
  (let ((minx 0)
        (miny 0)
        (maxx (screen-width))
        (maxy (screen-height))
        (xcenter #f)
        (ycenter #f)
        (range #f)
        (scale #f)
        (x0 #f)
        (y0 #f))
    (for-each (lambda (w)
                (if (virms-visible? w rlst)
                    (begin
                      (set! minx (min minx (window-x w)))
                      (set! miny (min miny (window-y w)))
                      (set! maxx (max maxx (+ (window-x w) (window-width w))))
                      (set! maxy (max maxy (+ (window-y w) (window-height w)))))))
              (list-of-windows 'window))
    (set! xcenter (/ (+ minx maxx) 2))
    (set! ycenter (/ (+ miny maxy) 2))

    ;; Our scale factor is a simple quotient, times ten. We divide
    ;; by .95 times the number of pixels to leave some inner border.
    ;; To get the origin, figure out where 0,0 would be given that
    ;; the center of the current screen should be in the center.
    
    (set! scale (max (/ (- maxx minx) (* 0.95 virtual-rooms-xsize))
                     (/ (- maxy miny) (* 0.95 virtual-rooms-ysize))))
    (set! x0 (quotient virtual-rooms-xsize 2))
    (set! y0 (quotient virtual-rooms-ysize 2))
    (set! x0 (- x0 (inexact->integer (/ xcenter scale))))
    (set! y0 (- y0 (inexact->integer (/ ycenter scale))))
    (list scale x0 y0)))

(define (virms-draw-windows pixlst)
  (let* ((rlst (map car pixlst))
         (params (virms-calc-params rlst))
         (curr (get-primary-room))
         (pix (assq curr pixlst)))
    (if pix
        (virt-draw-screen (cdr pix) params 0 (virt-get-color (root-window))))
    (for-each (lambda (w)
                (if (virms-visible? w rlst)
                    (let ((room (or (room-get-window-room w) curr)))
                      (set! pix (assq room pixlst))
                      (if pix
                          (virt-draw-window w (cdr pix) params 1 (virt-get-color w))))))
              (list-of-windows 'window 'stacking-order))))

(define (virms-draw-backgrounds pixlst)
  (let ((prim (get-primary-room))
        (sec (get-secondary-rooms))
        (bg (if (string? virtual-background)
                (make-color virtual-background)
                virtual-background))
        (fg (if (string? virtual-foreground)
                (make-color virtual-foreground)
                virtual-foreground))
        (px #f))
    (for-each (lambda (e)
                (draw-rectangle (cdr e)
                                2 2 (- (width (cdr e)) 4) (- (height (cdr e)) 4)
                                :background bg :borderwidth 2
                                :foreground (cond ((eq? (car e) prim)
                                                   fg)
                                                  ((memq (car e) sec)
                                                   (if (not px)
                                                       (begin
                                                         (set! px (make-pixmap 2 2 :background bg))
                                                         (draw-line px 0 0 1 1 :color fg)))
                                                   px)
                                                  (#t bg))))
              pixlst)))

(define (virtual-rooms-move-to deco event)
  (let* ((maplst (vector-ref (get-property (root-window) 'virms-map) 1))
         (ind (virms-room-ind-at (event-relative-x event)
                                 (event-relative-y event)))
         (realpos (if ind
                      (virt-map-to-real (virms-calc-params (map car maplst))
                                        (- (event-relative-x event)
                                           (* ind (+ virtual-rooms-xsize 1)))
                                        (event-relative-y event)) #f))
         (hswdt virtual-horizontal-step)
         (hshgt virtual-vertical-step)
         (absx #f)
         (absy #f)
         (room (virms-room-at-ind ind)))
    (if (and room realpos)
        (begin
          (set! absx (- (+ (car realpos) (virt-pos-x)) (quotient (screen-width) 2)))
          (set! absy (- (+ (cadr realpos) (virt-pos-y)) (quotient (screen-height) 2)))
          (set! absx (if (< absx 0) 
                         (* hswdt (quotient (- absx (quotient hswdt 2)) hswdt))
                         (* hswdt (quotient (+ absx (quotient hswdt 2)) hswdt))))
          (set! absy (if (< absy 0) 
                         (* hshgt (quotient (- absy (quotient hshgt 2)) hshgt))
                         (* hshgt (quotient (+ absy (quotient hshgt 2)) hshgt))))
          (if (not (eq? (get-primary-room) room))
              (room-switch-primary room))
          (virtual-move-to absx absy)))))

(define (virtual-rooms-move-window deco event)
  (let* ((maplst (vector-ref (get-property (root-window) 'virms-map) 1))
         (ind (or (virms-room-ind-at (event-relative-x event)
                                     (event-relative-y event)) -1))
         (mapleft (+ (deco-x deco) (* ind (+ virtual-rooms-xsize 1))))
         (mapleftall (deco-x deco))
         (maptop (deco-y deco))
         (mapright (+ mapleft virtual-rooms-xsize))
         (mapbottom (+ (deco-y deco) (deco-height deco)))
         (params (virms-calc-params (map car maplst)))
         (realpos (virt-map-to-real params
                                    (- (event-x event) mapleft)
                                    (- (event-y event) maptop)))
         (wind (if (>= ind 0)
                   (virms-window-at-position (virms-room-at-ind ind)
                                             (car realpos) (cadr realpos))
                   #f)))
    (if wind
        (let ((initpos (virt-real-to-map params (window-x wind) (window-y wind)))
              (initcorn (virt-real-to-map params (+ (window-x wind) (window-width wind)) (+ (window-y wind) (window-height wind))))
              (xoff (- (event-x event)))
              (yoff (- (event-y event)))
              (mapoff (* ind (+ virtual-rooms-xsize 1))))
          (if (not virtual-opaque-move)
              (let* ((res (with-user-feedback 
                           (lambda (e)
                             (draw-rubber-rectangle deco
                                                    (+ (car initpos) xoff mapoff (event-x e))
                                                    (+ (cadr initpos) yoff (event-y e))
                                                    (- (car initcorn) (car initpos))
                                                    (- (cadr initcorn) (cadr initpos))))
                           event
                           :cursor (make-cursor 130)
                           :no-freeze #t))
                     (nind (virms-room-ind-at (- (event-x res) mapleftall)
                                              (- (event-y res) maptop))))
                (if (and (= (event-code res) (event-code event)) nind)
                    (let* ((mapoff2 (* nind (+ virtual-rooms-xsize 1) -1))
                           (npos (virt-map-to-real params
                                                  (+ (car initpos) xoff mapoff mapoff2 (event-x res))
                                                  (+ (cadr initpos) yoff (event-y res)))))
                      (move-window wind (car npos) (cadr npos))
                      (if (and (not (= ind nind)) (room-get-window-room wind))
                          (room-set-window-room (car (list-ref maplst nind)) wind))
                      (if (not (window-mapped? wind))
                          (virtual-rooms-update)))))
              (with-user-feedback 
               (lambda (e)
                 (let* ((nind (virms-room-ind-at (- (event-x e) mapleftall)
                                                 (- (event-y e) maptop)))
                        (npos (if nind
                                  (virt-map-to-real params
                                                    (+ (car initpos) xoff
                                                       (* (- ind nind) (+ virtual-rooms-xsize 1))
                                                       (event-x e))
                                                    (+ (cadr initpos) yoff (event-y e)))
                                  #f))
                        (room (if (and nind (< nind (length maplst)))
                                  (car (list-ref maplst nind))
                                  #f)))
                   (if room
                       (begin
                         (move-window wind (car npos) (cadr npos))
                         (if (and (not (eq? (room-get-window-room wind) room))
                                  (room-get-window-room wind))
                             (room-set-window-room room wind))
                         (if (not (window-mapped? wind))
                             (virtual-rooms-update))))))
               event
               :cursor (make-cursor 130)
               :no-freeze #t))))))

(define (virtual-rooms-toggle-secondary deco event)
  (let* ((ind (virms-room-ind-at (event-relative-x event) (event-relative-y event))))
    (if ind
        (begin
          (room-toggle-secondary (virms-room-at-ind ind))
          (virtual-rooms-update)))))
         

(if (not (defined? 'virtual-rooms-behavior))
(define virtual-rooms-behavior
  (make-behavior
   (on-event (button 1 any) (lambda (d e) (virtual-rooms-move-to d e)))
   (on-event (button 2 any) (lambda (d e) (virtual-rooms-move-window d e)))
   (on-event (button 3 any) (lambda (d e) (virtual-rooms-toggle-secondary d e)))
   ))
)

(define (virtual-rooms-show)
  (let ((virms-map (or (get-property (root-window) 'virms-map)
                       (let ((map (make-vector 3 #f)))
                         (set-property! (root-window) 'virms-map map)
                         map))))
    (let ((win (vector-ref virms-map 0)))
      (if (and win (window-valid? win))
          (begin
            (vector-set! virms-map 2 (get-menu-gpos win))
            (vector-set! virms-map 0 #f)
            (delete-window win))))
    (if (and show-virtual-rooms
             (not (null? initial-rooms)))
        (let* ((gpos (or (vector-ref virms-map 2)
                         (coord->gpos virtual-rooms-xpos virtual-rooms-ypos)))
               (pixlst (map (lambda (e)
                              (cons e (make-pixmap virtual-rooms-xsize virtual-rooms-ysize)))
                            initial-rooms))
               (win (apply make-deco (append (map cdr pixlst)
                                             (list :behavior virtual-rooms-behavior
                                                   :background (make-color "black")
                                                   :separator 1
                                                   :direction 'horizontal)))))
          (virms-draw-backgrounds pixlst)
          (virms-draw-windows pixlst)
          (vector-set! virms-map 1 pixlst)
          (vector-set! virms-map 0 win)
          (place-menu-gpos win "virtual-rooms" gpos)))))

(define (virtual-rooms-toggle)
  (set! show-virtual-rooms (not show-virtual-rooms))
  (virtual-rooms-show))

(define (virtual-rooms-update)
  (let ((virms-map (get-property (root-window) 'virms-map)))
    (if (and show-virtual-rooms 
             virms-map
             (vector-ref virms-map 0)
             (window-valid? (vector-ref virms-map 0)))
        (begin
          (virms-draw-backgrounds (vector-ref virms-map 1))
          (virms-draw-windows (vector-ref virms-map 1))))))

(add-to-hook! virtual-update-hook virtual-rooms-update)

(add-to-hook! screen-opening (lambda (s) (virtual-rooms-show)))

(custom-menu-install-hook '("Virtual Screen" "Map") virtual-rooms-show)


(define virtual-rooms #t)
