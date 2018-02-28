
;;;;  Copyright (c) 2018, Daniel Kochmański (daniel@turtleware.eu)
;;;;
;;;;    Attaching backend to McCLIM core machinery.

(in-package #:charming-clim)

;;; This trick allows us to attach new backends without the hassle of having
;;; central backend registry.
(setf (get :charming :port-type) 'charming-port)
(setf (get :charming :server-path-parser) 'identity)
