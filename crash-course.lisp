(defpackage #:charming-clim/charms-crash-course
  (:use #:cl)
  (:export #:hello-world)
  (:nicknames #:ccc))
(in-package #:charming-clim/charms-crash-course)

;;; Higher level interface additions
(defun draw-window-border (window
                           &optional
                             (ls #\|) (rs #\|) (ts #\-) (bs #\-)
                             (tl #\+) (tr #\+) (bl #\+) (br #\+))
  (apply #'charms/ll:wborder (charms::window-pointer window)
         (mapcar #'char-code (list ls rs ts bs tl tr bl br))))

(defun draw-window-box (window &optional (verch #\|) (horch #\-))
  (charms/ll:box (charms::window-pointer window) (char-code verch) (char-code horch)))

(defun start-color ()
  (when (eql (charms/ll:has-colors) charms/ll:FALSE)
    (error "Your terminal does not support color."))
  (let ((ret-code (charms/ll:start-color)))
    (if (= ret-code 0)
        T
        (error "start-color error ~s." ret-code))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defconstant +black+   charms/ll:COLOR_BLACK)
  (defconstant +red+     charms/ll:COLOR_RED)
  (defconstant +green+   charms/ll:COLOR_GREEN)
  (defconstant +yellow+  charms/ll:COLOR_YELLOW)
  (defconstant +blue+    charms/ll:COLOR_BLUE)
  (defconstant +magenta+ charms/ll:COLOR_MAGENTA)
  (defconstant +cyan+    charms/ll:COLOR_CYAN)
  (defconstant +white+   charms/ll:COLOR_WHITE))

(defmacro define-color-pair ((name pair) foreground background)
  `(defparameter ,name (progn (charms/ll:init-pair ,pair ,foreground ,background)
                              (charms/ll:color-pair ,pair))))

(define-color-pair (+white/blue+ 1) +white+ +blue+)
(define-color-pair (+black/red+ 2) +black+ +red+)

(defun draw-window-background (window color-pair)
  (charms/ll:wbkgd (charms::window-pointer window) color-pair))

(defmacro with-colors ((window color-pair) &body body)
  (let ((winptr (gensym)))
    (alexandria:once-only (color-pair)
      `(let ((,winptr (charms::window-pointer ,window)))
         (charms/ll:wattron ,winptr ,color-pair)
         ,@body
         (charms/ll:wattroff ,winptr ,color-pair)))))


;;; first program (with border!)
(defun hello-world ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (loop named hello-world
       with window = (charms:make-window 50 15 10 10)
       do (progn
            (charms:clear-window window)
            (draw-window-border window)
            (charms:write-string-at-point window "Hello world!" 0 0)
            (charms:refresh-window window)

            ;; Process input
            (when (eql (charms:get-char window) #\q)
              (return-from hello-world))))))


;;; Second program (colors!)
(defun pretty-hello-world ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (start-color)
    (loop named hello-world
       with window = (charms:make-window 50 15 10 10)
       do (progn
            (charms:clear-window window)
            (draw-window-background window +white/blue+)
            (with-colors (window +white/blue+)
              (charms:write-string-at-point window "Hello world!" 0 0))
            (with-colors (window +black/red+)
              (charms:write-string-at-point window "Hello world!" 0 1))
            (charms:refresh-window window)

            ;; Process input
            (when (eql (charms:get-char window :ignore-error t) #\q)
              (return-from hello-world))))))


;;; third program, amazing computation
(defun amazing-hello-world ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (start-color)
    (loop named hello-world
       with window = (let ((win (charms:make-window 50 15 10 10)))
                       (charms:enable-non-blocking-mode win)
                       win)
       for flip-flop = (not flip-flop)
       do (progn
            (charms:clear-window window)
            (draw-window-background window +white/blue+)
            (with-colors (window (if flip-flop
                                     +white/blue+
                                     +black/red+))
              (charms:write-string-at-point window "Hello world!" 0 0))
            (charms:refresh-window window)
            ;; Process input
            (when (eql (charms:get-char window :ignore-error t) #\q)
              (return-from hello-world))
            (sleep 1)))))

;;; asynchronous input hack (should be a mailbox!)
(defparameter *recompute-flag* nil "ugly and unsafe hack for communication")
(defparameter *recompute-thread* nil)

(defun start-recompute-thread ()
  (when *recompute-thread*
    (bt:destroy-thread *recompute-thread*))
  (setf *recompute-thread*
        (bt:make-thread
         #'(lambda ()
             (loop
                (sleep 1)
                (setf *recompute-flag* t))))))

(defun stop-recompute-thread ()
  (when *recompute-thread*
    (bt:destroy-thread *recompute-thread*)
    (setf *recompute-thread* nil)))

(defun display-amazing-hello-world (window flip-flop)
  (charms:clear-window window)
  (draw-window-background window +white/blue+)
  (with-colors (window (if flip-flop
                           +white/blue+
                           +black/red+))
    (charms:write-string-at-point window "Hello world!" 0 0))
  (charms:refresh-window window))

(defun get-amazing-hello-world-input (window)
  (when *recompute-flag*
    (setf *recompute-flag* nil)
    (return-from get-amazing-hello-world-input :compute))
  (charms:get-char window :ignore-error t))

(defun improved-amazing-hello-world ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (start-color)
    (let ((window (charms:make-window 50 15 10 10))
          (flip-flop nil))
      (charms:enable-non-blocking-mode window)
      (display-amazing-hello-world window flip-flop)
      (loop named hello-world
         do (case (get-amazing-hello-world-input window)
              ((#\q #\Q) (return-from hello-world))
              (:compute (setf flip-flop (not flip-flop))
                        (display-amazing-hello-world window flip-flop))
              ;; don't be a pig to a processor
              (otherwise (sleep 1/60)))))))

