;; gnome.scm --- Some gnome compatibility functionality
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Gwm is gnome compatible in the sense that scheme primitives are 
;; provided to allow communication with gnome and with other gnome
;; compatible clients. However, the specific gnome functionality must
;; be provided by scheme code, since it is scheme code that is responsible
;; for the desk top model and for the window behaviors.
;;
;; Therefore the appropriate aspects of gnome compatibility should be
;; provided by specific packages like "virtual.scm", "rooms.scm", 
;; "window-func.scm", etc (this is however not completely done yet). 
;;
;; This file implements aspects of gnome compatibility not obviously
;; belonging to any other package. Currently this is mainly to maintain
;; a property with all client window identities.
;; 

(define (gnome-add-client win)
  (let ((id (window->client-xid win))
        (prop (get-x-property (root-window) '_WIN_CLIENT_LIST)))
    (if (and id (not (and prop (if (list? prop) (memq id prop) (eq? id prop)))))
        (set-x-property! (root-window) '_WIN_CLIENT_LIST id #t))))

(define (gnome-remove-client win)
  (let ((id (window->client-xid win))
        (prop (get-x-property (root-window) '_WIN_CLIENT_LIST)))
    (if (and id prop (if (list? prop) (memq id prop) (eq? id prop)))
        (if (list? prop)
            (set-x-property! (root-window) '_WIN_CLIENT_LIST (delq id prop))
            (get-x-property (root-window) '_WIN_CLIENT_LIST #t)))))

(add-to-hook! screen-opening 
              (lambda (s)
                (register-gnome-feature '_WIN_CLIENT_LIST)
                (map gnome-add-client (list-of-windows))))

(add-to-hook! window-opening gnome-add-client)

(add-to-hook! window-closing gnome-remove-client)

(register-gnome-compliance)

(define gnome #t)
