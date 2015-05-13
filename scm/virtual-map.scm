;; virtual-map.scm --- A map of the virtual screen
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file provides a map of the virtual screen (defined in
;; "virtual.scm"). The map is automatically scaled to show the whole
;; area in the virtual screen that has any windows.
;;
;; By default, clicking the left button in the map will move the visible
;; area to that part of the map, and the middle button can drag a window
;; in the map to a new position. The right button updates the map, but
;; this should normally not be necessary.
;;

;;    User customizable variables. Adjust these in your own profile.
(defvar show-virtual #t "Show the map of the virtual screen" 'boolean)
(defvar virtual-omit-nailed #t "if true, map shows only non-nailed windows" 'boolean)
(defvar virtual-omit-list '() "list of windows not shown in map" 'list)
(defvar virtual-show-filled #t "if true, windows are not drawn transparent in map" 'boolean)
(defvar virtual-fancy-colors '() "list of (wind-class fg bg) specs." 'list)
(defvar virtual-opaque-move #t "if true, moving a window in the map moves it immediately instead of an outline" 'boolean)

(defvar virtual-xpos 0 "original position of map" 'integer)
(defvar virtual-ypos 0 "original position of map" 'integer)
(defvar virtual-pixsize 160 "size of the map" 'integer)
(defvar virtual-background "white" "default background of the map" 'color)
(defvar virtual-foreground "black" "default window frame color on the map" 'color)
(defvar virtual-title-font #f "font of window titles in the map" 'font)
(defvar virtual-title-position #f "position of titles in the map"
  '(top center bottom above below))

(require 'virtual)

(define (virt-drawable win)
  (and (not (and virtual-omit-nailed
                 (virtual-nailed win)))
       (not (deco-icon? win))
       (not (matches-list win virtual-omit-list))))

(define (virt-calc-params)
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
                (if (virt-drawable w)
                    (begin
                      (set! minx (min minx (window-x w)))
                      (set! miny (min miny (window-y w)))
                      (set! maxx (max maxx (+ (window-x w) (window-width w))))
                      (set! maxy (max maxy (+ (window-y w) (window-height w)))))))
              (list-of-windows 'window 'mapped))
    (set! range (max (- maxy miny) (- maxx minx)))
    (set! xcenter (/ (+ minx maxx) 2))
    (set! ycenter (/ (+ miny maxy) 2))

    ;; Our scale factor is a simple quotient, times ten. We divide
    ;; by .95 times the number of pixels to leave some inner border.
    ;; To get the origin, figure out where 0,0 would be given that
    ;; the center of the current screen should be in the center.
    
    (set! scale (/ range (* 0.95 virtual-pixsize)))
    (set! x0 (quotient virtual-pixsize 2))
    (set! y0 (quotient virtual-pixsize 2))
    (set! x0 (- x0 (inexact->integer (/ xcenter scale))))
    (set! y0 (- y0 (inexact->integer (/ ycenter scale))))
    (list scale x0 y0)))

(define (virt-draw-text pix left top wdt hgt name vfont col pos)
  (let* ((font (if (string? vfont) (make-font vfont) vfont))
         (fh (cadr (string-dimensions "" font)))
         (yoff (- fh 2)))
    (if pos
        (set! yoff (cond ((eq? pos 'top)
                          (- fh 2))
                         ((eq? pos 'center)
                          (quotient (+ hgt fh -6) 2))
                         ((eq? pos 'bottom)
                          (- hgt 1))
                         ((eq? pos 'above)
                          -2)
                         ((eq? pos 'below)
                          (+ hgt fh -1))
                         (#t
                          (- fh 2)))))
    (if (or (not pos) (eq? pos 'center) (eq? pos 'bottom) (eq? pos 'top))
        (let ((i (string-length name)))
          (while (and (> i 0)
                      (> (car (string-dimensions (substring name 0 i) font)) wdt))
            (set! i (- i 1)))
          (set! name (substring name 0 i))))
    (draw-text pix (+ 1 left) (+ yoff top) name :font font :color col)))

(define (virt-draw-window win pix params border cols)
  (let ((left (+ (inexact->integer (/ (window-x win) (car params)))
                 (cadr params)))
        (top (+ (inexact->integer (/ (window-y win) (car params)))
                (caddr params)))
        (wdt (inexact->integer (/ (window-width win) (car params))))
        (hgt (inexact->integer (/ (window-height win) (car params)))))
    (if (cadr cols)
        (draw-rectangle pix left top wdt hgt
                        :borderwidth border 
                        :foreground (car cols)
                        :background (cadr cols))
        (draw-rectangle pix left top wdt hgt
                        :borderwidth border 
                        :foreground (car cols)))
    (if virtual-title-font
        (virt-draw-text pix left top wdt hgt (window-name win)
                        virtual-title-font (car cols) virtual-title-position))))

(define (virt-draw-screen pix params border cols)
  (let ((left (cadr params))
        (top (caddr params))
        (wdt (inexact->integer (/ (screen-width) (car params))))
        (hgt (inexact->integer (/ (screen-height) (car params)))))
    (if (cadr cols)
        (draw-rectangle pix left top wdt hgt
                        :borderwidth border 
                        :foreground (car cols)
                        :background (cadr cols))
        (draw-rectangle pix left top wdt hgt
                        :borderwidth border 
                        :foreground (car cols)))))

(define (virt-get-color win)
  (let ((res (if (eq? win (root-window))
                 (if (and (pair? virtual-fancy-colors)
                          (pair? (car virtual-fancy-colors))
                          (not (caar virtual-fancy-colors)))
                     (cdar virtual-fancy-colors)
                     '(#f #f))
                 (or (let ((vc (get-property win 'virt-col)))
                       (if vc (list #f vc) #f))
                     (matches-cond win virtual-fancy-colors)
                     '(#f #f)))))
    (if (not (car res))
        (set-car! res virtual-foreground))
    (if (null? (cdr res))
        (set-cdr! res (list #f)))
    (if (and (not (cadr res))
             virtual-show-filled)
        (set-car! (cdr res) virtual-background))
    (map (lambda (c)
           (if (string? c) (make-color c) c))
         res)))

(define (virt-draw-windows pix params)
  (virt-draw-screen pix params 2 (virt-get-color (root-window)))
  (for-each (lambda (w)
              (if (virt-drawable w)
                  (virt-draw-window w pix params 1 (virt-get-color w))))
            (list-of-windows 'window 'stacking-order 'mapped)))

(define (virt-window-at-pos x y)
  (let ((lst (reverse (list-of-windows 'window 'stacking-order 'mapped)))
        (res #f)
        (w #f))
    (while (not (or res (null? lst)))
      (set! w (car lst))
      (if (and (virt-drawable w)
               (<= (window-x w) x)
               (> (+ (window-x w) (window-width w)) x)
               (<= (window-y w) y)
               (> (+ (window-y w) (window-height w)) y))
          (set! res w))
      (set! lst (cdr lst)))
    res))
    
(define (virt-map-to-real params relx rely)
  (list (inexact->integer (* (- relx (cadr params)) (car params)))
        (inexact->integer (* (- rely (caddr params)) (car params)))))

(define (virt-real-to-map params realx realy)
  (list (+ (inexact->integer (/ realx (car params))) (cadr params))
        (+ (inexact->integer (/ realy (car params))) (caddr params))))

(define (virtual-map-move-to deco event)
  (let ((realpos (virt-map-to-real (virt-calc-params)
                                   (event-relative-x event)
                                   (event-relative-y event)))
        (hswdt virtual-horizontal-step)
        (hshgt virtual-vertical-step)
        (absx #f)
        (absy #f))
    (set! absx (- (+ (car realpos) (virt-pos-x)) (quotient (screen-width) 2)))
    (set! absy (- (+ (cadr realpos) (virt-pos-y)) (quotient (screen-height) 2)))
    (set! absx (if (< absx 0) 
                   (* hswdt (quotient (- absx (quotient hswdt 2)) hswdt))
                   (* hswdt (quotient (+ absx (quotient hswdt 2)) hswdt))))
    (set! absy (if (< absy 0) 
                   (* hshgt (quotient (- absy (quotient hshgt 2)) hshgt))
                   (* hshgt (quotient (+ absy (quotient hshgt 2)) hshgt))))
    (virtual-move-to absx absy)))

(define (virtual-map-move-window deco event)
  (let ((params (virt-calc-params))
        (mapleft (deco-x deco))
        (maptop (deco-y deco))
        (mapright (+ (deco-x deco) (deco-width deco)))
        (mapbottom (+ (deco-y deco) (deco-height deco)))
        (realpos #f)
        (wind #f))
    (set! realpos (virt-map-to-real params
                                    (- (event-x event) mapleft)
                                    (- (event-y event) maptop)))
    (set! wind (virt-window-at-pos (car realpos) (cadr realpos)))
    (virtual-update)
    (if (and wind
             (virt-drawable wind))
        (let ((initpos (virt-real-to-map params (window-x wind) (window-y wind)))
              (initcorn (virt-real-to-map params (+ (window-x wind) (window-width wind)) (+ (window-y wind) (window-height wind))))
              (xoff (- (event-x event)))
              (yoff (- (event-y event))))
          (if (not virtual-opaque-move)
              (let ((res (with-user-feedback 
                          (lambda (e)
                            (draw-rubber-rectangle deco
                                                   (+ (car initpos) xoff (event-x e))
                                                   (+ (cadr initpos) yoff (event-y e))
                                                   (- (car initcorn) (car initpos))
                                                   (- (cadr initcorn) (cadr initpos))))
                          event
                          :cursor (make-cursor 130)
                          :no-freeze #t)))
                (if (and (= (event-code res) (event-code event))
                         (> (event-x res) mapleft)
                         (< (event-x res) mapright)
                         (> (event-y res) maptop)
                         (< (event-y res) mapbottom))
                    (let ((npos (virt-map-to-real params
                                                  (+ (car initpos) xoff (event-x res))
                                                  (+ (cadr initpos) yoff (event-y res)))))
                      (move-window wind (car npos) (cadr npos)))))
              (with-user-feedback 
               (lambda (e)
                 (let ((npos (virt-map-to-real params
                                               (+ (car initpos) xoff (event-x e))
                                               (+ (cadr initpos) yoff (event-y e)))))
                   (move-window wind (car npos) (cadr npos))))
                          event
                          :cursor (make-cursor 130)
                          :no-freeze #t))))))

(if (not (defined? 'virtual-map-behavior))
(define virtual-map-behavior
  (make-behavior
   (on-event (button 1 any) virtual-map-move-to)
   (on-event (button 2 any) virtual-map-move-window)
   (on-event (button 3 any) (lambda (w e) (virtual-update)))
   ))
)

(define (virtual-show-map)
  (let ((virt-map (or (get-property (root-window) 'virt-map)
                      (let ((map (make-vector 3 #f)))
                        (set-property! (root-window) 'virt-map map)
                        map))))
    (let ((win (vector-ref virt-map 0)))
      (if (and win (window-valid? win))
          (begin
            (vector-set! virt-map 2 (get-menu-gpos win))
            (vector-set! virt-map 0 #f)
            (delete-window win))))
    (if show-virtual
        (let* ((params (virt-calc-params))
               (col (if (string? virtual-background)
                        (make-color virtual-background)
                        virtual-background))
               (gpos (or (vector-ref virt-map 2)
                         (coord->gpos virtual-xpos virtual-ypos)))
               (pix (make-pixmap virtual-pixsize virtual-pixsize :background col))
               (win (make-deco pix :behavior virtual-map-behavior)))
          (virt-draw-windows pix params)
          (vector-set! virt-map 1 pix)
          (vector-set! virt-map 0 win)
          (place-menu-gpos win "virtual" gpos)))))

(define (virtual-toggle-map)
  (set! show-virtual (not show-virtual))
  (virtual-show-map))

(define (virtual-update-map)
  (let ((virt-map (get-property (root-window) 'virt-map)))
    (if (and show-virtual 
             virt-map
             (vector-ref virt-map 0)
             (window-valid? (vector-ref virt-map 0)))
        (let ((params (virt-calc-params))
              (col (if (string? virtual-background)
                       (make-color virtual-background)
                       virtual-background)))
          (draw-rectangle (vector-ref virt-map 1)
                          0 0 virtual-pixsize virtual-pixsize
                          :background col)
          (virt-draw-windows (vector-ref virt-map 1) params)))))

(add-to-hook! virtual-update-hook virtual-update-map)

(add-to-hook! screen-opening (lambda (s) (virtual-show-map)))

(add-to-hook! screen-resize-hook virtual-update-map)

(custom-menu-install-hook '("Virtual Screen" "Map") virtual-show-map)


(define virtual-map #t)
