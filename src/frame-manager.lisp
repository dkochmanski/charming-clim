
;;;;  Copyright (c) 2018, Daniel Kochmański (daniel@turtleware.eu)
;;;;
;;;;    Implementation of the charming frame manager.

(in-package #:charming-clim)

(defmethod clim:make-pane-1 ((fm charming-frame-manager) (frame clim:application-frame)
                             type &rest initargs)
  (apply #'make-instance type :frame frame :manager fm :port (clim:port frame) initargs))
