;;;; charming-clim.asd
;;;;
;;;; Copyright (c) 2018 Daniel Kochmański

(asdf:defsystem #:charming-clim
  :description "Charming CLIM is cl-charms backend for McCLIM."
  :author "Daniel Kochmański"
  :license "BSD-2-Clause"
  :serial t
  :components ((:file "package")
               (:file "charming-clim")
               (:file "crash-course")))

