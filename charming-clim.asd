;;;; charming-clim.asd
;;;;
;;;; Copyright (c) 2018 Daniel Kochmański

(asdf:defsystem #:charming-clim
  :description "Charming CLIM is cl-charms backend for McCLIM."
  :author "Daniel Kochmański"
  :license "BSD-2-Clause"
  :depends-on (#:mcclim #:cl-charms)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "charming-clim")
               (:file "classes")
               (:file "port")
               (:file "frame-manager")
               (:file "medium")
               (:file "various")))

