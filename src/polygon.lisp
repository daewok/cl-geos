;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;;
;;;; Basic polygon definitions.

(in-package #:geos)

(defclass polygon (geometry)
  ((cached-shell
    :initarg :shell
    :accessor cached-shell)
   (cached-holes
    :initarg :holes
    :accessor cached-holes)))

(define-geos-fun "GEOSGeom_createPolygon" :pointer
  (shell %geometry)
  (holes %geometry-array)
  (num-holes :uint))

(defmethod make-geos-rep ((self polygon))
  ;; Release ownership of the shell and holes.
  (release-ownership (cached-shell self))
  (mapc #'release-ownership (cached-holes self))
  ;; Make the polygon object.
  (geos-geom-create-polygon (cached-shell self) (cached-holes self) (length (cached-holes self))))

(defun make-polygon (shell &optional holes)
  "Given a boundary as a linear-ring and a list of holes (also linear-rings),
construct a polygon."
  (make-instance 'polygon
                 :holes (mapcar #'ensure-linear-ring holes)
                 :shell (ensure-linear-ring shell)))

(defgeneric ensure-polygon (shell)
  (:method ((shell polygon))
    shell)
  (:method (shell)
    (make-polygon shell)))
