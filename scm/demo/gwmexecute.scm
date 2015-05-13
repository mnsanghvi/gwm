;; gwmexecute.scm --- Demo of how to implement the GWM_EXECUTE feature
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Gwm-1.* had a feature that when you changed the property GWM_EXECUTE
;; on the root screen (or any window) its contents were interpreted as
;; a lisp command and executed. This is removed in Gwm-2.*, of at least
;; two reasons:
;;
;; 1. This is a serious security risk, since anyone who have access to
;;    your display will be able to control Gwm, and therefore also to
;;    ask Gwm to execute any shell command in your name, including to
;;    start a terminal window as you on her/his own display.
;;    This can be prevented by never using xhost and be very careful
;;    with how the xauth cookies are stored, but sometimes this is
;;    impossible or at least very inconvenient.
;;
;; 2. It is unneccesary to have it built-in on the C-level since it can
;;    as easily (or more easily - the code is shorter) be implemented in
;;    scheme instead.
;;
;; So here is an example of how it can be implemented in scheme.
;; Don't use it !
;;

(modify-screen-behavior 
 (on (property-change 'GWM_EXECUTE) 
     (let ((str (get-x-property deco 'GWM_EXECUTE #t)))
       (if (string? str)
           (begin
             (display (eval-string str))
             (newline))))))
