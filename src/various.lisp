
;;;;  Copyright (c) 2018, Daniel Kochmański (daniel@turtleware.eu)
;;;;
;;;;    Test application and other handy stuff simplifying development.

(in-package #:charming-clim)

;;; Test application.
(clim:define-application-frame cc-test ()
  ()
  (:pane :application-pane :display-function #'display)
  (:geometry :height 100 :width 100))

(defgeneric display (frame pane)
  (:method ((frame cc-test) (pane clim:application-pane))
    (format pane "Hello world!")))

(defun test-charming-clim (&optional (port :charming))
  (let ((clim:*default-server-path* (list port)))
    (clim:run-frame-top-level
     (clim:make-application-frame 'cc-test))))

(defvar *console-io* *terminal-io*)

;;; Swank server and wait loop.
(defun start-swank-and-hang ()
  (asdf:load-system :swank)
  (funcall (find-symbol "CREATE-SERVER" "SWANK") :port 5555 :dont-close t)
  (loop (sleep 1)))

(defun %start-color ()
  (when (eql (charms/ll:has-colors) charms/ll:FALSE)
    (error "Your terminal does not support color."))
  (let ((ret-code (charms/ll:start-color)))
    (if (= ret-code 0)
        T
        (error "start-color error ~s." ret-code))))
