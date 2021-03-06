;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;;
;;;; Basic line string definitions.

(in-package #:geos)

(defclass line-string (geometry xarray::xarray-like)
  ((cached-coords
    :accessor cached-coords
    :initarg :coords))
  (:documentation "A series of points."))

(define-geos-fun "GEOSGeom_createLineString" :pointer
  (sequence %coord-sequence))

(defmethod make-geos-rep ((self line-string))
  (destructuring-bind (length dims) (array-dimensions (cached-coords self))
    (declare (ignore dims))
    (let ((seq (geos-coord-seq-create length 2)))
      (dotimes (idx length)
        (geos-coord-seq-set-x seq idx (coerce (aref (cached-coords self) idx 0) 'double-float))
        (geos-coord-seq-set-y seq idx (coerce (aref (cached-coords self) idx 1) 'double-float)))
      (geos-geom-create-line-string seq))))

(defun make-line-string (coords)
  "Make a line string. COORDS must a a sequence of x-y coordinates."
  (make-instance 'line-string :coords (ensure-coordinate-sequence coords)))

(defgeneric ensure-line-string (coords)
  (:method ((coords line-string))
    coords)
  (:method (coords)
    (make-line-string coords)))

(defmethod xdims ((line line-string))
  (list (geos-coord-seq-get-size (geos-geom-get-coord-seq line))
        (geos-coord-seq-get-dimensions (geos-geom-get-coord-seq line))))

(defmethod xref ((line line-string) &rest subscripts)
  (assert (= 2 (length subscripts)))
  (assert (and (>= (first subscripts) 0)
               (< (first subscripts) (geos-coord-seq-get-size (geos-geom-get-coord-seq line)))))
  (assert (and (>= (second subscripts) 0)
               (< (second subscripts) (geos-coord-seq-get-dimensions (geos-geom-get-coord-seq line)))))
  (geos-coord-seq-get-ordinate (geos-geom-get-coord-seq line) (first subscripts) (second subscripts)))
