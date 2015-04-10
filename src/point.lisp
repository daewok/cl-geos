;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;;
;;;; Basic point definitions.

(in-package #:geos)

(defclass point (geometry)
  ((cached-x
    :accessor cached-x
    :initarg :x
    :documentation "The X value of the point. Cached when retrieved from
GEOS.")
   (cached-y
    :accessor cached-y
    :initarg :y
    :documentation "The X value of the point. Cached when retrieved from
GEOS."))
  (:documentation "A single point, consisting of an x and a y component."))

(defun make-point* (x y)
  (make-instance 'point :x x :y y))

(defgeneric make-point (coords)
  "Make a point. COORDS must be a sequence of length two."
  (:method ((coords list))
    (assert (= 2 (length coords)))
    (make-instance 'point
                   :x (first coords)
                   :y (second coords)))
  (:method ((coords array))
    (assert (and (= 1 (array-rank coords))
                 (= 2 (array-dimension coords 0))))
    (make-instance 'point
                   :x (aref coords 0)
                   :y (aref coords 1))))

(defgeneric ensure-point (coords)
  (:method ((coords point))
    coords)
  (:method (coords)
    (make-point coords)))


;;; CFFI

(define-geos-fun "GEOSGeom_createPoint" :pointer
  (sequence %coord-sequence))

(define-geos-fun ("GEOSGeomGetX" :make-friendly? nil) (%geos-return :error-on -1)
  (point %geometry)
  (value :pointer))

(defun geos-geom-get-x (point)
  (with-foreign-object (value :double)
    (%geos-geom-get-x (context-handle-pointer *context-handle*) point value)
    (mem-ref value :double)))

(define-geos-fun ("GEOSGeomGetY" :make-friendly? nil) (%geos-return :error-on -1)
  (point %geometry)
  (value :pointer))

(defun geos-geom-get-y (point)
  (with-foreign-object (value :double)
    (%geos-geom-get-y (context-handle-pointer *context-handle*) point value)
    (mem-ref value :double)))

(defmethod make-geos-rep ((self point))
  (let ((coord-seq (geos-coord-seq-create 1 2)))
    (geos-coord-seq-set-x coord-seq 0 (coerce (cached-x self) 'double-float))
    (geos-coord-seq-set-y coord-seq 0 (coerce (cached-y self) 'double-float))
    (geos-geom-create-point coord-seq)))

(defmethod x ((self point))
  (unless (cached-x self)
    (setf (cached-x self) (geos-geom-get-x self)))
  (cached-x self))

(defmethod y ((self point))
  (unless (cached-y self)
    (setf (cached-y self) (geos-geom-get-y self)))
  (cached-y self))
