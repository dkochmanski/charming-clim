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

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun start-color ()
    (when (eql (charms/ll:has-colors) charms/ll:FALSE)
      (error "Your terminal does not support color."))
    (let ((ret-code (charms/ll:start-color)))
      (if (= ret-code 0)
          T
          (error "start-color error ~s." ret-code))))
  
  (defconstant +black+   charms/ll:COLOR_BLACK)
  (defconstant +red+     charms/ll:COLOR_RED)
  (defconstant +green+   charms/ll:COLOR_GREEN)
  (defconstant +yellow+  charms/ll:COLOR_YELLOW)
  (defconstant +blue+    charms/ll:COLOR_BLUE)
  (defconstant +magenta+ charms/ll:COLOR_MAGENTA)
  (defconstant +cyan+    charms/ll:COLOR_CYAN)
  (defconstant +white+   charms/ll:COLOR_WHITE)
  (charms:with-curses () (start-color)))

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


;;; third program, amazing computation (asynchronous input)
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
(defvar *recompute-thread* nil)

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


;;; fourth program, greedy desires (gadgets and keyboard input)

(define-color-pair (+black/white+ 3) +black+ +white+) ; color for text-input (inactive)
(define-color-pair (+black/cyan+ 4) +black+ +cyan+)   ; color for text-input (active)
(define-color-pair (+yellow/black+ 5) +yellow+ +black+) ; color for button (inactive)
(define-color-pair (+red/black+ 6) +red+ +black+) ; color for button (active)

(defparameter *active-gadget* nil)
(defparameter *computation-name* "Hello world!")

;;; gadget should be type of `window' or `panel' – we are simplistic
(defclass gadget ()
  ((position :initarg :position :accessor gadget-position)))

(defgeneric display-gadget (window gadget &key &allow-other-keys)
  (:method ((window charms:window) (gadget gadget) &key)
    (declare (ignore window gadget))))

(defgeneric handle-input (gadget input &key &allow-other-keys)
  (:method (gadget input &key)
    (declare (ignore gadget input))
    NIL))

(defclass text-input-gadget (gadget)
  ((buffer :accessor gadget-buffer)
   (width :initarg :width :reader gadget-width)))

(defun make-text-input-gadget (width x y)
  (let ((gadget (make-instance 'text-input-gadget
                               :width width
                               :position (cons x y)))
        (array (make-array width
                           :element-type 'character
                           :initial-element #\space
                           :fill-pointer t)))
    (setf (gadget-buffer gadget) array
          (fill-pointer array) 0)
    gadget))

(defmethod display-gadget ((window charms:window) (gadget text-input-gadget) &key)
  (with-colors (window (if (eql gadget *active-gadget*)
                           +black/cyan+
                           +black/white+))
    (let ((background (make-string (gadget-width gadget) :initial-element #\space)))
      (destructuring-bind (x . y) (gadget-position gadget)
        (charms:write-string-at-point window background x y)
        (charms:write-string-at-point window (gadget-buffer gadget) x y)))))

(defmethod handle-input ((gadget text-input-gadget) input &key)
  (let ((buffer (gadget-buffer gadget)))
    (case input
      ((#\Backspace #\Rubout)
       (unless (zerop (fill-pointer buffer))
         (vector-pop buffer)))
      ((#\Return #\Newline)
       (unless (zerop (fill-pointer buffer))
         (setf *computation-name* (copy-seq buffer)
               (fill-pointer buffer) 0)))
      ((#\ESC #\ETX) ;; etx is C-c
       (setf (fill-pointer buffer) 0))
      (otherwise
       (when (ignore-errors (graphic-char-p input))
         (vector-push input buffer))))))

(defclass button-gadget (gadget)
  ((label :initarg :label :reader gadget-label)
   (action :initarg :action :reader gadget-action)))

(defun make-button-gadget (text callback x y)
  (make-instance 'button-gadget :label text :action callback :position (cons x y)))

(defmethod display-gadget ((window charms:window) (gadget button-gadget) &key)
  (with-colors (window (if (eql gadget *active-gadget*)
                           +red/black+
                           +yellow/black+))
    (destructuring-bind (x . y) (gadget-position gadget)
      (charms:write-string-at-point window (gadget-label gadget) x y))))

(defmethod handle-input ((gadget button-gadget) input &key)
  (when (member input '(#\return #\newline))
    (funcall (gadget-action gadget))))

(defun toggle-recompute-thread ()
  (if *recompute-thread*
      (stop-recompute-thread)
      (start-recompute-thread)))

(defparameter *gadgets*
  (list (make-text-input-gadget 26 2 13)
        (make-button-gadget " Toggle " 'toggle-recompute-thread 30 11)
        (make-button-gadget "  Exit  " 'exit-application 40 11)
        (make-button-gadget " Accept " 'accept-input-box 30 13)
        (make-button-gadget " Cancel " 'cancel-input-box 40 13)))

(defun accept-input-box ()
  (handle-input (car *gadgets*) #\return))

(defun cancel-input-box ()
  (handle-input (car *gadgets*) #\esc))

(defun exit-application ()
  (throw :exit :exit-button))

(defun display-greedy-hello-world (window flip-flop)
  (charms:clear-window window)
  (draw-window-background window +white/blue+)
  (with-colors (window (if flip-flop
                           +white/blue+
                           +black/red+))
    (charms:write-string-at-point window *computation-name* 2 1))
  (dolist (g *gadgets*)
    (if (eql g *active-gadget*)
        (display-gadget window g)
        (charms:with-restored-cursor window
          (display-gadget window g))))
  (charms:refresh-window window))

(defun get-greedy-hello-world-input (window)
  (when *recompute-flag*
    (setf *recompute-flag* nil)
    (return-from get-greedy-hello-world-input :compute))
  (charms:get-char window :ignore-error t))

(defun greedy-hello-world ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (start-color)
    (let ((window (charms:make-window 50 15 10 10))
          (flip-flop nil))
      (charms:enable-non-blocking-mode window)
      (display-greedy-hello-world window flip-flop)
      (catch :exit
        (loop
           do (let ((input (get-greedy-hello-world-input window)))
                ;; (when input
                ;;   (format *xxx* "have ~s~%" input))
                (case input
                  (#\Dc1 ;; this is C-q
                   (throw :exit :c-q))
                  (#\Dc2 ;; this is C-r
                   (charms:clear-window charms:*standard-window*)
                   (charms:refresh-window charms:*standard-window*)
                   (display-greedy-hello-world window flip-flop))
                  (#\tab
                   (alexandria:if-let ((remaining (cdr (member *active-gadget* *gadgets*))))
                     (setf *active-gadget* (car remaining))
                     (setf *active-gadget* (car *gadgets*)))
                   (display-greedy-hello-world window flip-flop))
                  (:compute
                   (setf flip-flop (not flip-flop))
                   (display-greedy-hello-world window flip-flop))
                  (otherwise
                   (if (handle-input *active-gadget* input)
                       ;; redisplay only if handle-input returns non-NIL
                       (display-greedy-hello-world window flip-flop)
                       ;; don't be a pig to a processor
                       (sleep 1/60))))))))))


;;; fifth program, enterprise software (mouse integration)

(defgeneric bounding-rectangle (gadget)
  (:method ((gadget text-input-gadget))
    (destructuring-bind (x . y) (gadget-position gadget)
      (values x
              y
              (+ x -1 (gadget-width gadget))
              y)))
  (:method ((gadget button-gadget))
    (destructuring-bind (x . y) (gadget-position gadget)
      (values x
              y
              (+ x -1 (length (gadget-label gadget)))
              y))))

(defun region-contains-position-p (gadget x y)
  (multiple-value-bind (x-min y-min x-max y-max)
      (bounding-rectangle gadget)
    (and (<= x-min x x-max)
         (<= y-min y y-max))))

(defun distribute-mouse-event (bstate x y)
  (dolist (g *gadgets*)
    (when (region-contains-position-p g x y)
      (setf *active-gadget* g)
      (when (eql bstate charms/ll:button1_clicked)
        (handle-input g #\return))
      (return))))

(defun display-enterprise-hello-world (window flip-flop)
  (charms:with-restored-cursor window
    (charms:clear-window window)
    (draw-window-background window +white/blue+)
    (if flip-flop
        (with-colors (window +white/blue+)
          (charms:write-string-at-point window *computation-name* 2 1))
        (with-colors (window +black/red+)
          (charms:write-string-at-point window *computation-name* 2 1)))
    (dolist (g *gadgets*) (display-gadget window g))
    (charms:refresh-window window)))

(defun get-enterprise-hello-world-input (window)
  (when *recompute-flag*
    (setf *recompute-flag* nil)
    (return-from get-enterprise-hello-world-input :compute))
  (let ((c (charms/ll:wgetch (charms::window-pointer window))))
    (when (not (eql c charms/ll:ERR))
      (alexandria:switch (c)
        (charms/ll:KEY_BACKSPACE #\Backspace)
        (charms/ll:KEY_MOUSE :KEY-MOUSE)
        (otherwise (charms::c-char-to-character c))))))

(defun enterprise-process-event (window flip-flop)
  (loop
     (let ((input (get-enterprise-hello-world-input window)))
       (case input
         (#\Dc1 ;; this is C-q
          (throw :exit :c-q))
         (#\Dc2 ;; this is C-r
          (display-enterprise-hello-world window flip-flop))
         (#\tab
          (alexandria:if-let ((remaining (cdr (member *active-gadget* *gadgets*))))
            (setf *active-gadget* (car remaining))
            (setf *active-gadget* (car *gadgets*)))
          (display-enterprise-hello-world window flip-flop))
         (#\Latin_Small_Letter_S_With_Caron ;; this is S-[tab]
          (if (eql *active-gadget* (car *gadgets*))
              (setf *active-gadget* (alexandria:lastcar *gadgets*))
              (do ((g *gadgets* (cdr g)))
                  ((eql *active-gadget* (cadr g))
                   (setf *active-gadget* (car g)))))
          (display-enterprise-hello-world window flip-flop))
         (:key-mouse
          (handler-case (multiple-value-bind (bstate x y z id)
                            (charms/ll:getmouse)
                          (declare (ignore z id))
                          ;; window starts at 10,10
                          (decf x 10)
                          (decf y 10)
                          (charms:move-cursor window x y)
                          (distribute-mouse-event bstate x y)
                          (display-enterprise-hello-world window flip-flop))
            (error () nil)))
         (:compute
          (setf flip-flop (not flip-flop))
          (display-enterprise-hello-world window flip-flop))
         (otherwise
          (if (handle-input *active-gadget* input)
              ;; redisplay only if handle-input returns non-NIL
              (display-enterprise-hello-world window flip-flop)
              ;; don't be a pig to a processor
              (sleep 1/60)))))))

(defun start-mouse ()
  (charms/ll:mousemask
   (logior charms/ll:all_mouse_events charms/ll:report_mouse_position))
  (format cl-user::*console-io* "~c[?1003h" #\esc))

(defun enterprise-hello-world ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (start-color)
    (let ((window (charms:make-window 50 15 10 10))
          (flip-flop nil))
      ;; full enterprise ay?
      (charms:enable-non-blocking-mode window)
      (charms:enable-extra-keys window)
      (start-mouse)
      (display-enterprise-hello-world window flip-flop)
      (catch :exit
        (loop (funcall 'enterprise-process-event window flip-flop))))))
