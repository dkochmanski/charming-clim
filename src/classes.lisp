
;;;;  Copyright (c) 2018, Daniel Kochmański (daniel@turtleware.eu)
;;;;
;;;;    Class definitions with brief commentary for a CLIM backend based on
;;;;    cl-charms (ncurses wrapper written in Common Lisp).

(in-package #:charming-clim)

;;; Port is a logical connection to a display server. It abstracts managament of
;;; windows, resources, incoming events etc.
;;;
;;; Usually we want to subclass `basic-port' which has some functionality
;;; implemented out of the box and provides a reasonable framework which is
;;; suitable as a starting point for client-server communication model.
;;;
;;; With port we have associated a few objects: its primary pointer, the frame
;;; manager and the keyboard-focus. `keyboard-input-focus' holds the client to
;;; whom keyboard events are dispatched. Called by `stream-set-input-focus'.
;;;
;;; Note to myself: pointer should be member of basic-port with a reader
;;; `port-pointer'. This method is mentioned in the spec in context of other
;;; operators but it is not documented (ommision probably). This should be
;;; documented too.
;;;
;;; Note to myself: we define method `frame-manager' specialized on port in CLIM
;;; package. This method is usually specialized on frame.
(defclass charming-port (clim:basic-port)
  ((pointer :accessor clim:port-pointer
            :initform (make-instance 'charming-pointer))
   (frame-manager :accessor clim:frame-manager
                  :initform (make-instance 'charming-frame-manager))
   ;; See annotation in http://bauhh.dyndns.org:8000/clim-spec/8-1.html#_304
   (keyboard-input-focus :accessor clim:port-keyboard-input-focus
                         :initform nil)))

;;; Pointer is a class representing the pointing device. It is worth noting that
;;; CLIM doesn't mention possibility of multiple pointers associated with the
;;; port, however `tracking-pointer' documentation uses phrasing "primary
;;; pointer" for the sheet being `port-pointer' of its port. That means that
;;; "secondary pointer" is an option too. See method `pointer-event-pointer'.
;;; 
;;; In curses pointer may be in one of three states: invisible, normal and very
;;; visible (usually that means blinking).
;;;
;;; Note to myself: `standard-pointer' should implement some of the common
;;; methods that are currently entirely provided by backends. Same goes for the
;;; coordinates and `pointer-cursor' accessor. Documentation should be provided
;;; too for the `pointer-cursor' which is underdocumented.
(defclass charming-pointer (clim:standard-pointer)
  ((cursor :accessor clim:pointer-cursor
           :initform charms/ll:cursor_normal
           :type (member charms/ll:cursor_invisible
                         charms/ll:cursor_normal
                         charms/ll:cursor_very_visible))
   (x :initform 0)
   (y :initform 0)))

;;; Frame manager is responsible for the actual realization of look-and-feel of
;;; the frame and for selecting pane types for abstract gadgets. When a frame is
;;; adopted by the frame manager, protocol for generating pane hierarchy should
;;; be invoked.
;;;
;;; Note to myself: `basic-port' has slot `frame-managers' (plural) and we
;;; manage a list of such. From my initial investigation there is always one
;;; frame manager associated with the port and we call it in
;;; `Core/clim-core/frames.lisp'. This should be simplified and burden of
;;; implementing multiple frame managers should be put on the backend
;;; implementer. We should add method `frame-managers' which returns all backend
;;; frame managers defaulting to list made of the only frame-manager.
;;;
;;; Note to myself: there seems to be some hackery in port initialize-instance
;;; for each backend to actually set a frame-manager - couldn't it be
;;; implemented with `default-initarg' of `basic-port' subclass?
;;;
;;; Note to myself: all backends provide a hackish mapping between generic
;;; gadget name and a concrete pane class. This involves some symbol manging
;;; etc. It may be worth to investigate whenever some more intelligible internal
;;; protocol could be pulled off for that which will be common for frame
;;; managers.
(defclass charming-frame-manager (clim:frame-manager) ())

;;; Graft is a root window in the display server being represented as a CLIM
;;; sheet. Many grafts may be connected to the same display server (port).
;;;
;;; Note to myself: grafts are pretty versatile but we don't seem to implement
;;; neither units nor orientation. When fixed add appropriate annotation to
;;; `http://bauhh.dyndns.org:8000/clim-spec/edit/apropos?q=find-graft'.
(defclass charming-graft (clim:graft) ())

;;; Medium is an abstraction for graphics state of a sheet like bounding
;;; rectangle, ink, text-style or line-style. Medium also contains a sheet
;;; transformation.
(defclass charming-medium (clim:basic-medium) ())
