;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;;
;;;; Basic multi-polygon definitions.

(in-package #:geos)

(defclass multi-polygon (geometry)
  ((cached-polygons
    :initarg :polygons
    :accessor cached-polygons)))

(defmethod make-geos-rep ((self multi-polygon))
  ;; Release ownership of all polygons.
  (loop :for polygon :across (cached-polygons self)
     :do (release-ownership polygon))
  ;; Make the multi-polygon.
  (geos-geom-create-collection :multi-polygon (cached-polygons self)
                               (array-dimension (cached-polygons self) 0)))

(defun make-multi-polygon (polygons)
  (make-instance 'multi-polygon :polygons (map 'vector #'ensure-polygon polygons)))
