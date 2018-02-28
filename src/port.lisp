
;;;;  Copyright (c) 2018, Daniel Kochmański (daniel@turtleware.eu)
;;;;
;;;;    Implementation of the charming port.

(in-package #:charming-clim)

;;; initialize-instance :after method plays role of the port constructor.
(defmethod initialize-instance :after ((port charming-port) &rest initargs)
  (declare (ignore initargs))
  ;; Fix parents.
  (setf (slot-value (clim:port-pointer port)  'clim:port) port
        (slot-value (clim:frame-manager port) 'clim:port) port)
  ;; Initialize terminal.
  (clim:restart-port port))

;;; In principle port may have many frame managers and each frame manager is
;;; associated with one port. McCLIM implements this by having `frame-managers'
;;; reader in `basic-port', but we have only one fm.
(defmethod climi::frame-managers ((port charming-port))
  (list (clim:frame-manager port)))

;;; This method should restart event processing loop and discard all pending
;;; events. It may (but doesn't have to) restart connection to the server.
(defmethod clim:restart-port :after ((port charming-port))
  ;; If port was not initialized `charms/ll:endwin' will return error.
  (ignore-errors (clim:destroy-port port))
  ;; Initialize charms using the high-level interface.
  (let ((win (charms:initialize)))
    (charms:enable-raw-input)
    (charms:disable-echoing)
    (charms:enable-extra-keys win)
    (charms:disable-non-blocking-mode win)
    ;; This part could be ommited, but we want some visual effect of our WIP.
    (multiple-value-bind (w h) (charms:window-dimensions win)
      (let* ((str "Charming CLIM port initialized.")
             (x (truncate (- w (length str)) 2))
             (y (truncate h 2)))
        (charms:clear-window win)
        (charms:write-string-at-point win str x y)
        (charms:refresh-window win)))))

;;; Finalize connection to the port and release all resources if any.
(defmethod clim:destroy-port :after ((port charming-port))
  (charms:finalize))

(defmethod clim:make-medium ((port charming-port) sheet)
  (make-instance 'charming-medium :sheet sheet))

;;; Note to myself: find the purpose of the following functions, add generic
;;; function definitions and document them:
;;;   port-set-mirror-region
;;;   port-set-mirror-transformation
