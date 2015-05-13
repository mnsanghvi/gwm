;; rooms.scm --- General rooms package 
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file implements some general room functionality, i.e. allowing
;; windows to be in separate workspace areas (but not geometrically
;; related as on the virtual screen). 
;;
;; Each window can belong to a specific room. Windows that don't belong
;; to any room are visible in all rooms. 
;; There is at any time one room which is the "primary" room, the windows
;; of which are shown, and to which new windows are assigned by default.
;; There can also be a set of additional rooms active, the seconadry
;; rooms, whose windows are also shown at the same time.
;;
;; This is only the basic functionality for dealing with rooms, but not
;; any user interface to them. That is provided by other packages. 
;; However, this code is gnome compliant, so it should be possible to
;; use the gnome interface to switch and move windows between rooms.
;;

;;    User customizable variables. Adjust these in your own profile.
(defvar initial-rooms '() "List of initial rooms" 'list)
(defvar rooms-omit-list '(Gwm) "Windows visible in all rooms" 'list)
(defvar rooms-window-list '() "Initial rooms for different window types" 'list)
(defvar rooms-property-list '() "Association lists per room, with properties :name, :background, :mgr-background, :hidden, :clean, :enter-hook, :leave-hook" 'list) 

(define (get-primary-room)
  (get-property (root-window) 'primary-room))

(define (get-secondary-rooms)
  (or (get-property (root-window) 'secondary-rooms) '()))

(define (get-room-property room key)
  (let ((prop (assoc room rooms-property-list)))
    (if prop
        (get-keyword key (cdr prop) #f)
        #f)))

(define (room-switch-primary room)
  (let* ((curr (get-primary-room))
         (oldbg (get-room-property curr :background))
         (newbg (get-room-property room :background))
         (lhook (get-room-property curr :leave-hook))
         (ehook (get-room-property room :enter-hook)))
    (if lhook (lhook))
    (set-property! (root-window) 'primary-room room)
    (if newbg
        (set-deco-background! (screen) newbg)
        (if oldbg
            (set-deco-background! (screen) (make-color "gray"))))
    (for-each (lambda (w) 
                (room-update-window w))
              (list-of-windows 'window))
    (if ehook (ehook))
    (gnome-room-update room)))

(define (room-open-secondary room)
  (let ((secondary-rooms (get-secondary-rooms)))
    (if (not (memq room secondary-rooms))
        (begin
          (set-property! (root-window) 'secondary-rooms
                         (cons room secondary-rooms))
          (for-each (lambda (w)
                      (room-update-window w))
                    (list-of-windows 'window))))))

(define (room-close-secondary room)
  (let ((secondary-rooms (get-secondary-rooms)))
    (if (memq room secondary-rooms)
        (begin
          (set-property! (root-window) 'secondary-rooms
                         (delq room secondary-rooms))
          (for-each (lambda (w)
                      (room-update-window w))
                    (list-of-windows 'window))))))

(define (room-toggle-secondary room)
  (let ((secondary-rooms (get-secondary-rooms)))
    (set-property! (root-window) 'secondary-rooms
                   (if (memq room secondary-rooms)
                       (delq room secondary-rooms)
                       (cons room secondary-rooms)))
    (for-each (lambda (w)
                (room-update-window w))
              (list-of-windows 'window))))

(define (room-forward)
  (let* ((curr (get-primary-room))
         (ind (list-index initial-rooms curr)))
    (if ind
        (set! ind (remainder (+ 1 ind) (length initial-rooms)))
        (set! ind 0))
    (room-switch-primary (list-ref initial-rooms ind))))

(define (room-backward)
  (let* ((curr (get-primary-room))
         (ind (list-index initial-rooms curr)))
    (if ind
        (set! ind (remainder (+ (length initial-rooms) ind -1) (length initial-rooms)))
        (set! ind 0))
    (room-switch-primary (list-ref initial-rooms ind))))

(define (room-set-window-room room w)
  (set-property! (top-deco w) 'room room)
  (room-update-window (top-deco w))
  (gnome-room-update-window room w))

(define (room-get-window-room w)
  (get-property (top-deco w) 'room))

(define (room-update-window w)
  (if (room-window-visible w)
      (begin
        (unhide-window (window-deco w) 'rooms)
        (if (icon-decorated? w) (unhide-window (icon-deco w) 'rooms)))
      (begin
        (hide-window (window-deco w) 'rooms)
        (if (icon-decorated? w) (hide-window (icon-deco w) 'rooms)))))

(define (room-window-visible w)
  (let ((room (room-get-window-room w))
        (clean (get-room-property (get-primary-room) :clean)))
    (or (and (not room) (not clean))
        (eq? room (get-primary-room))
        (memq room (get-secondary-rooms)))))

(define (room-pop-to-window w)
  (if (not (room-window-visible w))
      (room-switch-primary (room-get-window-room w))))

(define (room-pop-window-to-room w)
  (if (not (room-window-visible w))
      (room-set-window-room (get-primary-room) w)))

(define (room-add-window w)
  (if (not (matches-list w rooms-omit-list))
      (let ((match (matches-cond w rooms-window-list))
            (prop (if (check-gnome-compliance) (get-x-property w '_WIN_WORKSPACE) #f)))
        (room-set-window-room (cond ((and prop (> (length initial-rooms) prop))
                                     (list-ref initial-rooms prop))
                                    ((and match (list? match))
                                     (car match))
                                    (match 
                                     match)
                                    (#t
                                     (get-primary-room)))
                              w))))

(define (gnome-room-update room)
  (if (check-gnome-compliance) 
      (let ((idx (or (list-index initial-rooms room) 0)))
        (set-x-property! (root-window) '_WIN_WORKSPACE idx))))

(define (gnome-room-update-window room w)
  (if (check-gnome-compliance) 
      (let ((idx (or (list-index initial-rooms room) 0))
            (prop (get-x-property w '_WIN_WORKSPACE)))
        (if (and prop (not (= prop idx)))
            (set-x-property! w '_WIN_WORKSPACE idx)))))

(define (gnome-room-move-to idx)
  (if (check-gnome-compliance) 
      (let ((nr (if (> (length initial-rooms) idx)
                    (list-ref initial-rooms idx) #f))
            (cr (get-property (root-window) 'primary-room)))
        (if (not (eq? cr nr))
            (room-switch-primary nr)))))

(define (gnome-room-move-window-to idx w)
  (if (check-gnome-compliance) 
      (let ((nr (if (> (length initial-rooms) idx)
                    (list-ref initial-rooms idx) #f))
            (cr (get-property w 'room)))
        (if (not (eq? cr nr))
            (room-set-window-room nr w)))))

(define room-screen-behavior 
  (make-behavior
   (on (key "XF86Forward" modifiers)
       (room-forward)
       :steal #t)
   (on (key "XF86Back" modifiers)
       (room-backward)
       :steal #t)
   (on (client-message '_WIN_WORKSPACE)
       (if (check-gnome-compliance)
           (gnome-room-move-to (car (event-data event)))))))

(define room-window-behavior 
  (make-behavior
   (on (client-message '_WIN_WORKSPACE)
       (gnome-room-move-window-to (car (event-data event)) deco))))

(modify-behavior std-window-behavior room-window-behavior)

(define (room-init-screen s)
  (set-screen! s)
  (if (check-gnome-compliance)
      (begin
        (register-gnome-feature '_WIN_WORKSPACE)
        (set-x-property! (root-window) '_WIN_WORKSPACE_COUNT
                         (max 1 (length initial-rooms)))
        (set-x-property! (root-window) '_WIN_WORKSPACE 0)
        (set-x-property! (root-window) '_WIN_WORKSPACE_NAMES
                         (if (null? initial-rooms)
                             '("All")
                             (map symbol->string initial-rooms)))))
  (modify-behavior (deco-behavior (root-window)) room-screen-behavior)
  (if (not (null? initial-rooms))
      (begin
        (room-switch-primary (car initial-rooms))
        (for-each room-add-window (list-of-windows 'window)))))

(if (gwm-is-starting)
    (add-to-hook! screen-opening room-init-screen)
    (map room-init-screen (list-of-screens)))

(add-to-hook! pop-to-window-hook room-pop-to-window)

(add-to-hook! window-opening room-add-window)

(custom-menu-package-name '("Rooms"))


(define rooms #t)
