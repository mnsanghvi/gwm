;; virtual-door.scm --- Doors for the virtual screen
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This file defines "doors" on the virtual screen, ie. buttons that
;; when pressed moves the real screen somewhere on the virtual screen.
;; The position to move to can be fixed, or given by an expression.
;;
;; It also defines some very simple "door managing" functions, to add
;; or remove doors dynamically.
;;  '(add-door NAME)' adds a door to the next door-less area.
;;  '(add-door NAME 'free)' adds a door to an area free from windows.
;;  '(maybe-add-door NAME)' adds a door (to a window-free area) only
;; if a door of that name does not exist.
;;  '(goto-door NAME)' moves through a door.
;;  '(remove-door NAME)' removes a door. 
;;
;; There are some functions dealing with automatic generation and
;; removal of doors, suitable to call in menus for example. The doors
;; will have names starting with 'door-auto-string' and a number.
;;  '(fresh-door)' creates a new automatic door in an empty area.
;;  '(ask-fresh-door)' creates a new door after having asked for a name.
;;  '(clean-doors)' removes automatic doors for empty areas.
;; 

;;    User customizable variables. Adjust these in your own profile.
(defvar initial-doors '() "Doors to create on startup" 'list)
(defvar door-font "8x13" "Font in door buttons" 'font)
(defvar door-background "white" "Background color of door buttons" 'color)
(defvar door-foreground "black" "Foreground color of door buttons" 'color)
(defvar door-borderwidth 2 "Border width of door buttons" 'integer)
(defvar door-xsize 90 "Door button size" 'integer)
(defvar door-ysize 16 "Door button size" 'integer)
                               
(defvar door-mgr-dir-horiz #t "Controls mapping of doors on virtual screen." 'boolean)
(defvar door-mgr-dir-len 2 "Number of screenfulls in a (horizontal or vertical) row." 'integer)
(defvar door-mgr-mdir-horiz #t "Controls position of door buttons." 'boolean)
(defvar door-mgr-mdir-len 2 "Number of buttons in each (horizontal or vertical) row." 'integer)
(defvar door-mgr-xpos 0 "Upper left corner of door manager" 'integer)
(defvar door-mgr-ypos 0 "Upper left corner of door manager" 'integer)
(defvar door-mgr-tile 'transparent "Tile of empty positions" 'color)
(defvar door-context '() "List of customizations per door name" 'list)

(defvar door-auto-string "Area " "Name prefix of auto-generated doors" 'string)

(require 'virtual)
(require 'dialogue)

(define (get-door-mgr)
  (or (get-property (root-window) 'door-mgr)
      (let ((a (make-vector 3 #f)))
        (vector-set! a 2 '())
        (set-property! (root-window) 'door-mgr a)
        a)))

(define door-behavior
  (make-behavior
   (on (button any alone)
       (let ((pos (get-property deco 'pos))
             (action (get-property deco 'action)))
         (if (procedure? pos)
             (set! pos (pos)))
         (if (pair? pos)
             (virtual-move-to (car pos) (cdr pos)))
         (if action
             (action))
      ))))

(define (door-make-plug name gotopos action fg bg bw)
  (let ((font (if (string? door-font) (make-font door-font) door-font)))
    (make-deco '()
               (if (pixmap? name)
                   name
                   (make-label name
                               :font font
                               :foreground fg
                               :background bg))
               '()
               :width door-xsize
               :height door-ysize
               :borderwidth bw
               :bordercolor fg
               :background bg
               :direction 'horizontal
               :behavior door-behavior
               :property `((pos . ,gotopos) (action . ,action)))))

(define (door-make name xpos ypos gotopos)
  (let ((bg (if (string? door-background)
                (make-color door-background)
                door-background))
        (fg (if (string? door-foreground)
                (make-color door-foreground)
                door-foreground))
        (font (if (string? door-font) (make-font door-font) door-font)))
    (place-menu-gpos (door-make-plug name gotopos #f fg bg door-borderwidth)
                     "door"
                     (coord->gpos xpos ypos))))

(define (door-mgr-make-space fg bg tl)
  (make-deco :width door-xsize
             :height door-ysize
             :background tl))

(define (door-mgr-make-vborder ele1 ele2 fg bg tl)
  (make-deco '() 
             :width door-borderwidth
             :background (if (or ele1 ele2) fg (or tl bg))))

(define (door-mgr-count-runs lst i1 i2 step num)
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

(define (door-mgr-make-hborder runs fg bg tl)
  (let ((last #f)
        (lens #f))
    (if (= (car runs) 0)
        (begin
          (set! runs (cdr runs))
          (set! last #t)))
    (set! lens (map (lambda (n)
                      (set! last (not last))
                      (cons (+ (* n (+ door-xsize
                                       door-borderwidth))
                               (if last
                                   door-borderwidth
                                   (- door-borderwidth)))
                            last))
                    runs))
    (let ((last (car (reverse lens)))
          (first (car lens)))
      (if (cdr first)
          (set-car! first (- (car first) door-borderwidth)))
      (if (cdr last)
          (set-car! last (- (car last) door-borderwidth))))
    (apply make-deco (map (lambda (ele)
                           (make-deco :width (car ele)
                                      :height door-borderwidth
                                      :background (if (cdr ele) fg (or tl bg))))
                         lens))))

(define (door-mgr-make-row door-lst n step num fg bg tl)
  (let ((res '())
        (i 0)
        (len (length door-lst))
        (d1 (list-ref door-lst n))
        (d2 #f))
    (while (< i num)
      (set! res (cons (if d1
                          (door-make-plug (car d1) (door-virt-coord n) #f fg bg 0)
                          (door-mgr-make-space fg bg tl))
                      res))
      (set! n (+ n step))
      (set! i (+ i 1))
      (set! d2 d1)
      (set! d1 (if (or (= i num) (>= n len)) #f (list-ref door-lst n)))
      (if (and (> door-borderwidth 0)
               (not (= i num)))
          (set! res (cons (door-mgr-make-vborder d2 d1 fg bg tl)
                          res))))
    (apply make-deco (reverse res))))

(define (door-mgr-make-menu door-lst)
  (let* ((num (door-mgr-find-last door-lst))
         (rows (if door-mgr-mdir-horiz
                   (+ (quotient (- num 1) door-mgr-mdir-len) 1)
                   (min num door-mgr-mdir-len)))
         (cols (if door-mgr-mdir-horiz
                   (min num door-mgr-mdir-len)
                   (+ (quotient (- num 1) door-mgr-mdir-len) 1)))
         (step (if door-mgr-mdir-horiz
                   1 door-mgr-mdir-len))
         (bstep (if door-mgr-mdir-horiz
                    door-mgr-mdir-len 1))
         (bg (if (string? door-background)
                 (make-color door-background)
                 door-background))
         (fg (if (string? door-foreground)
                 (make-color door-foreground)
                 door-foreground))
         (tile (cond ((eq? door-mgr-tile 'transparent)
                      (make-color 'hole))
                     ((string? door-mgr-tile)
                      (make-pixmap bg door-mgr-tile fg))
                     (door-mgr-tile
                      door-mgr-tile)
                     (#t
                      bg)))
         (font (if (string? door-font) (make-font door-font) door-font))
         (res '())
         (i 0)
         (n 0))
    (while (< i rows)
      (set! res (cons (door-mgr-make-row door-lst n step cols fg bg tile)
                      res))
      (set! i (+ i 1))
      (set! n (+ n bstep))
      (if (and (> door-borderwidth 0)
               (not (= i rows)))
          (set! res (cons (door-mgr-make-hborder (door-mgr-count-runs door-lst (- n bstep) n step cols)
                                                 fg bg tile)
                          res))))
    (set! res (cons door-borderwidth (cons :borderwidth res)))
    (apply make-deco (reverse res))))

(define (door-mgr-show)
  (let ((door-mgr (get-door-mgr)))
    (if (and door-mgr
             (vector-ref door-mgr 0)
             (deco-valid? (vector-ref door-mgr 0)))
        (let ((menu (vector-ref door-mgr 0)))
          (vector-set! door-mgr 1 (get-menu-gpos menu))
          (delete-window (top-deco menu))))
    (if (and door-mgr
             (vector-ref door-mgr 2)
             (> (door-mgr-find-last (vector-ref door-mgr 2)) 0))
        (let ((menu (door-mgr-make-menu (vector-ref door-mgr 2)))
              (gpos (or (vector-ref door-mgr 1)
                        (coord->gpos door-mgr-xpos door-mgr-ypos))))
          (vector-set! door-mgr 0 menu)
          (place-menu-gpos menu
                           "door-mgr"
                           gpos)))))
  
(define (door-mgr-find-last lst)
  (let ((len (length lst))
        (i 0))
    (set! lst (reverse lst))
    (while (and (not (null? lst)) (not (car lst)))
      (set! lst (cdr lst))
      (set! i (+ 1 i)))
    (- len i)))

(define (door-virt-coord nr)
  (if door-mgr-dir-horiz
      (cons (* (screen-width) (remainder nr door-mgr-dir-len))
            (* (screen-height) (quotient nr door-mgr-dir-len)))
      (cons (* (screen-width) (quotient nr door-mgr-dir-len))
            (* (screen-height) (remainder nr door-mgr-dir-len)))))


;; Door Manager Functionality

(define (get-door nr)
  (let ((lst (vector-ref (get-door-mgr) 2)))
    (if (< nr (length lst))
        (list-ref lst nr)
        #f)))

(define (set-door nr ele)
  (let* ((door-mgr (get-door-mgr))
         (lst (vector-ref door-mgr 2)))
    (if (< nr (length lst))
        (list-set! lst nr ele)
        (begin
          (vector-set! door-mgr 2
                       (append lst (make-list (+ (- nr (length lst)) 1) #f)))
          (list-set! (vector-ref door-mgr 2) nr ele)))))

(define (door-empty-space? virtcoord)
  (let* ((left (- (car virtcoord) (virt-pos-x)))
         (right (+ left (screen-width)))
         (top (- (cdr virtcoord) (virt-pos-y)))
         (bottom (+ top (screen-height))))
    (and-map (lambda (w)
               (let ((midx (+ (window-x w) (quotient (window-width w) 2)))
                     (midy (+ (window-y w) (quotient (window-height w) 2))))
                 (or (virtual-nailed w)
                     (> midx left)
                     (< midx right)
                     (> midy top)
                     (< midy bottom))))
             (list-of-windows 'window))))

(define (door-find-index ind free movable)
  (cond ((and free movable)
         (while (or (get-door ind)
                    (not (door-empty-space? (door-virt-coord ind))))
           (set! ind (+ ind 1))))
        (movable
         (while (get-door ind)
           (set! ind (+ ind 1))))
        (free
         (while (or (not (door-empty-space? (door-virt-coord ind)))
                    (and (get-door ind)
                         (not (caddr (get-door ind)))))
           (set! ind (+ ind 1))))
        (#t
         (while (and (get-door ind)
                     (or (not (caddr (get-door ind)))
                         (not (door-empty-space? (door-virt-coord ind)))))
           (set! ind (+ ind 1)))))
  ind)

(define (door-find-name name)
  (let ((ind 0))
    (or-map (lambda (d)
              (if (and d (equal? name (car d)))
                  ind
                  (begin
                    (set! ind (+ ind 1))
                    #f)))
            (vector-ref (get-door-mgr) 2))))

(define (add-door name . args)
  (let ((startind (if (and (pair? args) (number? (car args))) (car args) 0))
        (free (memq 'free args))
        (movable (memq 'movable args))
        (ind #f)
        (pos #f)
        (ele #f))
    (set! ind (door-find-index startind free movable))
    (set! pos (door-virt-coord ind))
    (set! ele (list name free movable))
    (while (get-door ind)
      (let* ((startind (+ 1 ind))
             (oldele (get-door ind))
             (newind (door-find-index startind (cadr oldele) #f)))
        (set-door ind ele)
        (set! ele oldele)
        (set! ind newind)))
    (set-door ind ele)
    (door-mgr-show)
    pos))

(define (maybe-add-door name . args)
  (let ((ind (door-find-name name)))
    (if ind
        (door-virt-coord ind)
        (apply add-door (cons name args)))))

(define (remove-door arg)
  (let ((ind (cond ((string? arg)
                    (door-find-name arg))
                   ((number? arg)
                    arg)
                   (#t #f))))
    (if ind
        (let ((ele (get-door ind)))
          (set-door ind #f)
          (if (door-empty-space? (door-virt-coord ind))
              (let* ((newind (door-find-index (+ 1 ind) #t #f))
                     (newele (get-door newind)))
                (while newele
                  (set-door ind newele)
                  (set! ind newind)
                  (set! newind (door-find-index (+ 1 ind) #t #f))
                  (set! newele (get-door newind)))
                (set-door ind #f)))))
    (door-mgr-show)))

(define (goto-door name)
  (let ((ind (door-find-name name)))
    (if ind
        (let ((pos (door-virt-coord ind)))
          (virtual-move-to (car pos) (cdr pos))))))

(define (door-auto-name n)
  (string-append door-auto-string
                 (number->string n)))

(define (clean-doors)
  (let* ((lst (vector-ref (get-door-mgr) 2))
         (ind (length ind)))
    (for-each (lambda (d)
                (set! ind (- ind 1))
                (if (and d
                         (door-empty-space? (door-virt-coord ind))
                         (not (member (car d) initial-doors)))
                    (remove-door ind)))
              (reverse lst))
    (let ((dnum (or (get-property (root-window) 'door-auto-num) 0)))
      (while (and (> dnum 0)
                  (not (door-find-name (door-auto-name dnum))))
        (set! dnum (- dnum 1)))
      (set-property! (root-window) 'door-auto-num dnum))))
                      
(define (fresh-door)
  (let ((dnum (+ (or (get-property (root-window) 'door-auto-num) 0) 1)))
    (set-property! (root-window) 'door-auto-num dnum)
    (add-door (door-auto-name dnum) 'free)))

(define (ask-fresh-door)
  (let ((dname (simple-dialogue "New door:" ""
                                :background (if (string? door-background)
                                                (make-color door-background)
                                                door-background)
                                :active-background (make-color "white")
                                :foreground (if (string? door-foreground)
                                                (make-color door-foreground)
                                                door-foreground)
                                :font (if (string? door-font)
                                          (make-font door-font)
                                          door-font))))
    (if dname
        (add-door dname 'free))))

(define (door-add-initial s)
  (for-each (lambda (d)
              (cond ((procedure? d)
                     (d))
                    ((list? d)
                     (apply add-door d))
                    (#t
                     (add-door d))))
            initial-doors))

;; Install hooks

(add-to-hook! screen-opening door-add-initial)

(custom-menu-install-hook '("Virtual Screen" "Doors") door-mgr-show)


(define virtual-door #t)
