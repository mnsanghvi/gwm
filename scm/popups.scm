;; popups.scm --- Some functions to create standard popup menus
;;
;; Author: Anders Holst  (aho@sics.se) 
;; Copyright (C) 2002  Anders Holst
;;
;; --------------------------------------------------------------------- 
;;
;; Here are some examples of how 'construct-menu' can be used to
;; construct some standard kinds of menus:
;; (make-machine-popup NAME COMMAND MACHINE-LIST) constructs a menu to
;; run COMMAND (a string or list of strings or function taking a host
;; name and returning a list of strings) on the selected remote machine.
;; (make-login-popup NAME MACHINE-LIST) does almost the same, but assumes 
;; that the command is to run telnet to the remote machine.
;; (make-command-popup NAME COMMAND-LIST) constructs a menu to run the
;; selected shell command on the local host.
;; (make-machine-command-popup NAME1 MACHINE-LIST NAME2 COMMAND-LIST)
;; constructs a double menu where the user can select both a remote
;; machine and a command to run on it.
;; (make-windows-popup NAME) constructs a menu of all windows, that "goes
;; to" (raises and makes visible, etc.) the selected window.
;; (make-popup NAME LIST) is just a shorter way of calling 'construct-menu.
;;

(require 'construct-multimenu "multimenu")
(require 'dialogue)

(define remote-shell-program "ssh")
(define remote-login-program "telnet")

;; A simple menu dialogue without memory
(define (menu-dialogue query def action)
  (let ((fg (get-keyword :foreground default-menu-style #f))
        (bg (get-keyword :background default-menu-style #f))
        (abg (make-color "white"))
        (font (get-keyword :font default-menu-style #f))
        (res #f))
    (set! fg (make-color (if fg (fg) "black")))
    (set! bg (make-color (if bg (bg) "white")))
    (set! font (make-font (if font (font) "fixed")))
    (set! res (simple-dialogue query def
                               :background bg
                               :active-background abg
                               :foreground fg
                               :font font))
    (if res
        (action res))))

;; More advanced menu dialogue that remembers last input
(define (construct-dialogue-function ctx query var)
  (let ((fg (get-keyword :foreground ctx #f))
        (bg (get-keyword :background ctx #f))
        (abg (make-color "white"))
        (font (get-keyword :font ctx #f)))
    (set! fg (make-color (if fg (fg) "black")))
    (set! bg (make-color (if bg (bg) "white")))
    (set! font (make-font (if font (font) "fixed")))
    (lambda ()
      (let ((res (simple-dialogue query (variable-ref var)
                                  :background bg
                                  :active-background abg
                                  :foreground fg
                                  :font font)))
        (if res
            (variable-set! var res))
        res))))

(define last-command (make-variable ""))
(define last-machine (make-variable ""))

(define (make-machine-popup name cmd mach-lst)
  (let ((func1 (if (procedure? cmd)
                   (lambda (h) (apply execute (cmd h)))
                   (if (string? cmd)
                       (lambda (h) (execute cmd))
                       (lambda (h) (apply execute cmd)))))
        (func2 (if (procedure? cmd)
                   (lambda (h) 
                     (apply execute (append (if (list? remote-shell-program)
                                                remote-shell-program
                                                (list remote-shell-program))
                                            (list h)
                                            (cmd h))))
                   (if (string? cmd)
                       (lambda (h)
                         (apply execute (append (if (list? remote-shell-program)
                                                remote-shell-program
                                                (list remote-shell-program))
                                                (list h cmd))))
                       (lambda (h)
                         (apply execute (append (if (list? remote-shell-program)
                                                remote-shell-program
                                                (list remote-shell-program))
                                                (list h)
                                                cmd))))))
        (func3 (construct-dialogue-function default-menu-style
                                            "Machine:" last-machine)))
    (construct-menu default-menu-style
                    name
                    (construct-machine-menu-item default-menu-style
                                                 (hostname)
                                                 func1)
                    (construct-machine-menu-items default-menu-style
                                                  (delete (hostname) mach-lst)
                                                  func2)
                    (list "Other..."
                          (lambda (x)
                            (let ((res (func3)))
                              (if res
                                  (func2 res))))))))

(define (make-login-popup name mach-lst)
  (let ((func1 (lambda (h)
                 (execute "xterm" "-T" (machine-name h))))
        (func2 (lambda (h)
                 (apply execute (append (list "xterm" "-T" (machine-name h) "-e")
                                        (if (list? remote-login-program)
                                            remote-login-program
                                            (list remote-login-program))
                                        (list (machine-name h))))))
        (func3 (construct-dialogue-function default-menu-style
                                            "Machine:" last-machine)))
    (construct-menu default-menu-style
                    name
                    (construct-machine-menu-item default-menu-style
                                                 (hostname)
                                                 func1)
                    (construct-machine-menu-items default-menu-style
                                                  (delete (hostname) mach-lst)
                                                  func2)
                    (list "Other..."
                          (lambda (x)
                            (let ((res (func3)))
                              (if res
                                  (func2 res))))))))

(define (make-command-popup name cmd-lst)
  (let ((func (lambda (ele)
                (cond ((string? ele)
                       (list (let ((match (string-match "^\\([^ ]*\\)" ele)))
                               (if match
                                   (string-capitalize (match:substring match 1))
                                   (string-capitalize ele)))
                             (lambda (x)
                               (execute ele))))
                      ((and (list? ele)
                            (procedure? (cadr ele)))
                       (list (car ele)
                             (lambda (x)
                               (apply execute ((cadr ele) (hostname))))))
                      ((list? ele)
                       (list (car ele)
                             (lambda (x)
                               (apply execute (cdr ele)))))
                      (#t '()))))
        (dfunc (construct-dialogue-function default-menu-style
                                            "Command:" last-command)))
    (apply construct-menu (append (list default-menu-style name)
                                  (map func cmd-lst)
                                  (list (list "Other..."
                                              (lambda (x)
                                                (let ((res (dfunc)))
                                                  (if res
                                                      (execute "sh" "-c" res))))))))))

(define (make-machine-command-popup name1 mach-lst name2 cmd-lst)
  (let ((func (lambda (ele)
                (cond ((string? ele)
                       (list (let ((match (string-match "^\\([^ ]*\\)" ele)))
                               (if match
                                   (string-capitalize (match:substring match 1))
                                   (string-capitalize ele)))
                             (lambda ()
                               (list ele))))
                      ((pair? ele)
                       (list (car ele)
                             (if (procedure? (cadr ele))
                                 (lambda ()
                                   (cadr ele))
                                 (lambda ()
                                   (cdr ele)))))
                      (#t '()))))
        (dfunc1 (construct-dialogue-function default-menu-style
                                            "Machine:" last-machine))
        (dfunc2 (construct-dialogue-function default-menu-style
                                            "Command:" last-command)))
    (construct-multimenu default-menu-style
                         (lambda (h c)
                           (if (and h c)
                               (if (string? h) 
                                   (apply execute (append (if (list? remote-shell-program)
                                                              remote-shell-program
                                                              (list remote-shell-program))
                                                          (list h)
                                                          (if (procedure? c) (c h) c)))
                                   (apply execute (if (procedure? c) (c (hostname)) c)))))
                         (append (list name1
                                       (list (machine-name (hostname))
                                             (lambda () #t)))
                                 (map (lambda (h)
                                        (list (machine-name h)
                                              (lambda () h)))
                                      (delete (hostname) mach-lst))
                                 (list (list "Other..."
                                             dfunc1)))
                         (append (list name2)
                                 (map func cmd-lst)
                                 (list (list "Other..."
                                             (lambda ()
                                               (let ((res (dfunc2)))
                                                 (if res
                                                     (list "sh" "-c" res)
                                                     #f)))))))))

(define (make-windows-popup name)
  (construct-menu default-menu-style
                  name
                  (construct-window-menu-items default-menu-style
                                               (lambda (w) (not (string=? (window-client-class w) "Gwm")))
                                               pop-to-window)))

(define (make-popup name lst)
  (apply construct-menu (cons default-menu-style
                              (cons name lst))))


