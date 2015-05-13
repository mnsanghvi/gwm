;; copy.scm --- Copy a decoration
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; This is an example of how to copy a decoration, including its sub-
;; decorations. Since a decoration can be used in only one open window
;; at a time, you have to copy it if you want to use the same decoration
;; for several windows. If the decoration has an inner client or menu,
;; you have to supply a new inner client as the second argument.
;; Note that client specific components, like window names, are also
;; copied as they are, and not modified to suit the new inner client.
;;

(define (copy-deco deco inner)
  (cond ((eq? deco '())
         '())
        ((or (pixmap? deco)
             (active-label? deco))
         deco)
        ((deco? deco)
         (if (eq? deco (inner-deco deco))
             inner
             (let ((ctx (inspect-deco deco))
                   (parts (deco-parts deco)))
               (apply make-deco 
                      (append (map (lambda (p) (copy-deco p inner)) parts)
                              ctx)))))
        (#t
         (error "wrong type to copy ~A" deco))))
