
(in-package #:clim-user)

(defun draw-grid (sheet &optional (xstep 1) (ystep 1))
  (let ((ink +gray+))
    (loop for coord from 0 to 40 by xstep
       do (draw-line* sheet coord 0 coord 40 :ink ink))
    (loop for coord from 0 to 40 by ystep
       do (draw-line* sheet 0 coord 40 coord :ink ink))))

(defun draw-coords (sheet unit &optional (label "1mm"))
  (let ((ink +red+))
    ;; orientation
    (draw-arrow* sheet 0 0 44 0 :ink ink :head-width .5 :head-length 1)
    (draw-arrow* sheet 0 0 0 43 :ink ink :head-width .5 :head-length 1)
    ;; units
    (surrounding-output-with-border (sheet :background +grey+)
      (draw-text* sheet unit 40 0 :align-x :right :align-y :center :text-size :small))
    (surrounding-output-with-border (sheet :background +grey+)
      (draw-text* sheet unit 0 40 :align-x :center :align-y :bottom :text-size :small))
    ;; show where is 1mm
    (let ((1mm (/ 100 15.875)))
      (draw-line* sheet -0.5 1mm 0.5 1mm :line-thickness 1)
      (draw-line* sheet 1mm -0.5 1mm 0.5 :line-thickness 1)
      (draw-text* sheet label -1 1mm :align-x :right :align-y :center :text-size :small)
      (draw-text* sheet label 1mm -1 :align-x :center :align-y :bottom :text-size :small))))

;; http://bauhh.dyndns.org:8000/clim-spec/12-4.html

(defun round1* (p dx dy)
  (make-point (multiple-value-bind (x r) (round (/ (point-x p) dx))
                (if (= r -0.5)
                    (* (1- x) dx)
                    (* x dx)))
              (multiple-value-bind (y r) (round (/ (point-y p) dy))
                (if (= r -0.5)
                    (* (1- y) dy)
                    (* y dy)))))

(defun round2* (p dx dy)
  (make-point (multiple-value-bind (x r) (round (/ (point-x p) dx))
                (if (= r -0.5)
                    (* (1- x) dx)
                    (* x dx)))
              (multiple-value-bind (y r) (round (/ (point-y p) dy))
                (if (= r -0.5)
                    (* (1- y) dy)
                    (* y dy)))))

(defun rectangle-frame (p1 p2 &optional (xwidth 1) (ywidth 1))
  (let* ((p1 (round1* p1 xwidth ywidth))
         (p2 (round2* p2 xwidth ywidth))
         (diff (region-difference (make-rectangle p1 p2)
                                  (make-rectangle* (+ (point-x p1) xwidth)
                                                   (+ (point-y p1) ywidth)
                                                   (- (point-x p2) xwidth)
                                                   (- (point-y p2) ywidth)))))
    (if (eql diff +nowhere+)
        +everywhere+
        diff)))

(defun slap-rectangle (sheet p1 p2 ink &optional (xwidth 1) (ywidth 1))
  (draw-rectangle sheet (round1* p1 xwidth ywidth) (round2* p2 xwidth ywidth)
                  :ink ink :clipping-region (rectangle-frame p1 p2 xwidth ywidth))
  (draw-rectangle sheet p1 p2 :ink +red+ :filled nil))

(defun relative-position (p0 p1 dx dy)
  (make-point (/ (- (point-x p1) (point-x p0)) dx)
              (/ (- (point-y p1) (point-y p0)) dy)))

(defun label-rectangle (sheet p0 p1 p2 xwidth ywidth)
  (draw-text sheet (format nil "(~,2f, ~,2f)"
                           (abs (point-x (relative-position p0 p1 xwidth ywidth)))
                           (abs (point-y (relative-position p0 p1 xwidth ywidth))))
             p1
             :align-x :center :align-y :center :text-size :small)
  (draw-text sheet (format nil "(~,2f, ~,2f)"
                           (abs (point-x (relative-position p0 p2 xwidth ywidth)))
                           (abs (point-y (relative-position p0 p2 xwidth ywidth))))
             p2
             :align-x :center :align-y :center :text-size :small))

(defparameter *p0* (make-point 0 0))
(defparameter *p0-graphics* (make-point 0 40))
(defparameter *r1p1* (make-point 5 5))
(defparameter *r1p2* (make-point 22 35))
(defparameter *r2p1* (make-point 25 10))
(defparameter *r2p2* (make-point 30 15))

(defun draw-objects (sheet dx dy)
  (slap-rectangle sheet *r1p1* *r1p2* +violet+ dx dy)
  (slap-rectangle sheet *r2p1* *r2p2* +cyan+ dx dy))

(defun draw-screen (sheet unit &key (dx 1) (dy dx) (scale 10) (orientation :default))
  (window-clear sheet)
  (ecase orientation
    (:default (with-translation (sheet 50 50)
                (with-scaling (sheet scale)
                  (draw-grid sheet dx dy)
                  (draw-coords sheet unit)
                  (draw-objects sheet dx dy)
                  (label-rectangle sheet *p0* *r1p1* *r1p2* dx dy)
                  (label-rectangle sheet *p0* *r2p1* *r2p2* dx dy))))
    (:graphics (%draw-graphics-screen sheet unit dx dy scale))
    (:graphics-wrong (%draw-gr-screen-wrong sheet unit dx dy scale))))

(defun %draw-gr-screen-wrong (sheet unit dx dy scale
                              &aux (tr (compose-transformations
                                        ;; we must know screen size!
                                        (make-translation-transformation 0 500)
                                        (make-scaling-transformation 1 -1))))
  (window-clear sheet)
  (with-drawing-options (sheet :transformation tr)
    (with-translation (sheet 50 50)
      (with-scaling (sheet scale)
        (draw-grid sheet dx dy)
        (draw-coords sheet unit)
        (draw-objects sheet dx dy)
        (label-rectangle sheet *p0* *r1p1* *r1p2* dx dy)
        (label-rectangle sheet *p0* *r2p1* *r2p2* dx dy)))))

(defun %draw-graphics-screen (sheet unit dx dy scale
                              &aux (tr (compose-transformations
                                        ;; we must know screen size!
                                        (make-translation-transformation 0 500)
                                        (make-scaling-transformation 1 -1))))
  (with-drawing-options (sheet :transformation tr)
    (with-translation (sheet 50 50)
      (with-scaling (sheet scale)
        (draw-grid sheet dx dy)
        (draw-coords sheet unit))))
  (with-translation (sheet 50 50)
    (with-scaling (sheet scale)
      (draw-objects sheet dx dy)))
  ;; gross hack to show "device-native" coordinates
  (with-translation (sheet 50 50)
    (with-scaling (sheet scale)
      (let ((r1p1 (make-point (point-x *r1p1*) (point-y *r1p2*)))
            (r1p2 (make-point (point-x *r1p2*) (point-y *r1p1*)))
            (r2p1 (make-point (point-x *r2p1*) (point-y *r2p2*)))
            (r2p2 (make-point (point-x *r2p2*) (point-y *r2p1*))))
        (label-rectangle sheet *p0-graphics* r1p1 r1p2 dx dy)
        (label-rectangle sheet *p0-graphics* r2p1 r2p2 dx dy)))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *screens* (make-hash-table))
  (defvar *scale* 10))

(defun show-screen (sheet name &optional (scale *scale*))
  ;; Try (show-screen *standard-output* :console) in the Listener.
  (funcall (gethash name *screens*) sheet scale))

(defmacro dse (name description unit &key (dx 1) (dy dx) (orientation :default))
  `(setf (gethash ,name *screens*)
         (lambda (sheet &optional (scale *scale*))
           ,description
           (draw-screen sheet ,unit :dx ,dx :dy ,dy :scale scale :orientation ,orientation))))

(dse :console "Terminal with ridiculously small characters." "ch (5x8)" :dx 5 :dy 8)
(dse :72dpi "Very old graphical display." "px (72 ppi)" :dx (/ 160 72))
(dse :96dpi "Old graphical display (WPF's DIP)." "px (96 ppi)" :dx (/ 160 96))
(dse :mdpi "MDPI (Material Design's density-independent pixel)" "dp (160 ppi)")
(dse :hdpi "HDPI" "px (240 ppi)" :dx (/ 160 240))
(dse :xxxhdpi "XXXHDPI" "px (640 ppi)" :dx (/ 160 640))
(dse :printer "HQ printer" "dot (2560 dpi)" :dx (/ 160 2560))
(dse :plotter "LQ plotter" "dot (80 dpi)" :orientation :graphics :dx (/ 160 80))
(dse :drawer "MDPI drawer" "dot (160 dpi)" :orientation :graphics)
(dse :plotter* "LQ plotter (malfunctioning" "dot (80 dpi)" :orientation :graphics-wrong :dx (/ 160 80))
(dse :drawer* "MDPI drawer (malfunctioning)" "dot (160 dpi)" :orientation :graphics-wrong)

(defun test (&rest keys)
  (maphash (lambda (k v)
             (when (or (null keys)
                       (member k keys))
               (let ((sheet (open-window-stream :label (format nil "~a" k)
                                                :width 500
                                                :height 500
                                                :scroll-bars nil)))
                 (sleep 0.1)
                 (funcall v sheet))))
           *screens*))


(defun draw-screen-app (sheet &optional (grid t))
  (with-translation (sheet 40 40)
    (with-scaling (sheet 10)
      (when grid
        (draw-grid sheet 10 10))
      (draw-coords sheet "dp" "1cm")
      (draw-rectangle* sheet 2 2 28 28 :ink +grey42+)
      (draw-circle* sheet 15 8 5 :ink +dark-green+)
      (draw-circle* sheet 15 22 5 :ink +dark-red+)
      (draw-text* sheet "Remove?" 15 15
                  :align-x :center
                  :align-y :center
                  :text-size :huge
                  :text-family :fix
                  :text-face :italic)
      (draw-text* sheet "YES" 15 8
                  :align-x :center
                  :align-y :center
                  :text-size :huge
                  :text-family :fixed
                  :text-face :bold
                  :ink +white+)
      (draw-text* sheet "NO" 15 22
                  :align-x :center
                  :align-y :center
                  :text-size :huge
                  :text-family :fixed
                  :text-face :bold
                  :ink +white+)
      (repaint-sheet sheet +everywhere+))))

(defun draw-console-app (sheet &optional (grid t) (fg +black+) (bg +white+))
  (with-translation (sheet 40 40)
    (with-scaling (sheet 10)
      (when grid
        (draw-grid sheet 5/2 8/2)
        (draw-coords sheet "char" "1cm"))
      (with-translation (sheet (* 7 5/2) (* 4 8/2))
        (flet ((draw-string** (str y &optional (ink fg))
                 (dotimes (v (length str))
                   (draw-rectangle* sheet
                                    (* v 5/2)
                                    (* y 8/2)
                                    (* (1+ v) 5/2)
                                    (* (1+ y) 8/2)
                                    :ink bg)
                   (draw-text* sheet (elt str v) (* v 5/2) (* y 8/2)
                               :align-x :left
                               :align-y :top
                               :text-size 36
                               :ink ink
                               :text-family :fixed))))
          (draw-string** "Bus Time" 0)
          (draw-string** "--------" 1)
          (draw-string** "16   now" 2 +red+)
          (draw-string** "10   >1m" 3 +red+)
          (draw-string** "03z   5m" 4 +green+)))
      (repaint-sheet sheet +everywhere+))))

(defun draw-two-apps (sheet)
  (window-clear sheet)
  (draw-console-app sheet)
  (with-translation (sheet 460 0)
    (draw-screen-app sheet)))

(defun draw-two-screens (sheet)
  (window-clear sheet)
  (draw-on-console sheet)
  (with-translation (sheet 460 0)
    (draw-on-screen sheet)))

(defun draw-on-screen (sheet)
  (draw-screen-app sheet nil)
  (draw-console-app sheet nil +white+ +black+))

(defun draw-on-console (sheet)
  (with-translation (sheet 40 40)
    (with-scaling (sheet 10)
      (flet ((draw-cell (x y &key ch (fc +white+) (bc +black+))
               (setf x (* x 5/2) y (* y 8/2))
               (draw-rectangle* sheet x y (+ x 5/2) (+ y 8/2) :ink bc)
                                        ;(draw-rectangle* sheet x y (+ x 5/2) (+ y 8/2) :ink +gray+ :filled nil)
               (when ch
                 (draw-text* sheet ch x y
                             :align-x :left
                             :align-y :top
                             :text-size 34
                             :ink fc
                             :text-family :fixed))))
        (dotimes (x 16)
          (dotimes (y 10)
            (draw-cell x y)))
        (draw-coords sheet "char" "1cm")
        (let ((y 1))
          (loop for x from 1 to 3 do (draw-cell x y :bc +grey42+))
          (draw-cell 4 y :bc +dark-green+ :ch #\Y)
          (draw-cell 5 y :bc +dark-green+ :ch #\E)
          (draw-cell 6 y :bc +dark-green+ :ch #\S)
          (draw-cell 7 y :bc +dark-green+)
          (loop for x from 8 to 10 do (draw-cell x y :bc +grey42+)))
        (let ((y 2))
          (loop for x from 1 to 3 do (draw-cell x y :bc +grey42+))
          (draw-cell 4 y :bc +dark-green+)
          (draw-cell 5 y :bc +dark-green+)
          (draw-cell 6 y :bc +dark-green+)
          (draw-cell 7 y :bc +dark-green+)
          (loop for x from 8 to 10 do (draw-cell x y :bc +grey42+)))
        (let ((y 3)
              (str " _Remove?_"))
          (dotimes (v (length str))
            (draw-cell (+ v 1) y :ch (elt str v) :bc +grey42+)))
        (let ((y 4))
          (loop for x from 1 to 4 do (draw-cell x y :bc +grey42+))
          (draw-cell 5 y :bc +dark-red+)
          (draw-cell 6 y :bc +dark-red+)
          (loop for x from 7 to 10 do (draw-cell x y :bc +grey42+)))
        (let ((y 5))
          (loop for x from 1 to 3 do (draw-cell x y :bc +grey42+))
          (draw-cell 4 y :bc +dark-red+)
          (draw-cell 5 y :bc +dark-red+ :ch #\N)
          (draw-cell 6 y :bc +dark-red+ :ch #\O)
          (draw-cell 7 y :bc +dark-red+)
          (loop for x from 8 to 10 do (draw-cell x y :bc +grey42+)))
        (let ((y 6))
          (loop for x from 1 to 4 do (draw-cell x y :bc +grey42+))
          (draw-cell 5 y :bc +dark-red+)
          (draw-cell 6 y :bc +dark-red+)
          (loop for x from 7 to 10 do (draw-cell x y :bc +grey42+))))))
  (draw-console-app sheet nil +white+ +grey20+)
  (repaint-sheet sheet +everywhere+))
