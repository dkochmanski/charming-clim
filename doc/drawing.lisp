
(in-package #:clim-user)

(defun draw-grid (sheet &optional (xstep 1) (ystep 1))
  (let ((ink +gray+))
    (loop for coord from 0 to 40 by xstep
       do (draw-line* sheet coord 0 coord 40 :ink ink))
    (loop for coord from 0 to 40 by ystep
       do (draw-line* sheet 0 coord 40 coord :ink ink))))

(defun draw-coords (sheet unit)
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
      (draw-text* sheet "1mm" -1 1mm :align-x :right :align-y :center :text-size :small)
      (draw-text* sheet "1mm" 1mm -1 :align-x :center :align-y :bottom :text-size :small))))

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
(dse :72dpi "Very old graphical display." "px (72 dpi)" :dx (/ 160 72))
(dse :96dpi "Old graphical display (WPF's DIP)." "px (96 dpi)" :dx (/ 160 96))
(dse :mdpi "MDPI (Material Design's density-independent pixel)" "dp (160 dpi)")
(dse :hdpi "HDPI" "px (240 dpi)" :dx (/ 160 240))
(dse :xxxhdpi "XXXHDPI" "px (640 dpi)" :dx (/ 160 640))
(dse :printer "HQ printer" "px (2560 dpi)" :dx (/ 160 2560))
(dse :plotter "LQ plotter" "px (80 dpi)" :orientation :graphics :dx (/ 160 80))
(dse :plotter* "LQ plotter" "px (80 dpi)" :orientation :graphics-wrong :dx (/ 160 80))
(dse :drawer "MDPI drawer" "px (160 dpi)" :orientation :graphics)
(dse :drawer* "MDPI drawer (malfunctioning)" "px (160 dpi)" :orientation :graphics-wrong)

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
