;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;;
;;;; Basic multi-point definitions.

(in-package #:geos)

(defclass multi-point (geometry)
  ((cached-points
    :initarg :points
    :accessor cached-points)))

(defmethod make-geos-rep ((self multi-point))
  ;; Release ownership of all points.
  (loop :for point :across (cached-points self)
     :do (release-ownership point))
  ;; Make the multi-point.
  (geos-geom-create-collection :multi-point (cached-points self) (array-dimension (cached-points self) 0)))

(defun make-multi-point (points)
  (make-instance 'multi-point :points (map 'vector #'ensure-point points)))

