;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;;
;;;; Basic linear ring definitions.

(in-package #:geos)

(defclass linear-ring (line-string)
  ())

(define-geos-fun "GEOSGeom_createLinearRing" :pointer
  (sequence %coord-sequence))

(defmethod make-geos-rep ((self linear-ring))
  (destructuring-bind (length dims) (array-dimensions (cached-coords self))
    (declare (ignore dims))
    (let ((seq (geos-coord-seq-create length 2)))
      (dotimes (idx length)
        (geos-coord-seq-set-x seq idx (coerce (aref (cached-coords self) idx 0) 'double-float))
        (geos-coord-seq-set-y seq idx (coerce (aref (cached-coords self) idx 1) 'double-float)))
      (geos-geom-create-linear-ring seq))))

(defun make-linear-ring (coords)
  (make-instance 'linear-ring :coords (ensure-coordinate-ring coords)))

(defgeneric ensure-linear-ring (coords-or-linear-ring)
  (:method ((linear-ring linear-ring))
    linear-ring)
  (:method (coords)
    (make-linear-ring coords)))

