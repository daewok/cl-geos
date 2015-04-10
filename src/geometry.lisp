;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;; Contains the base classes that the other files rely on.

(in-package #:geos)

(defclass geometry ()
  ((geos-pointer
    :initform nil
    :accessor geos-pointer
    :initarg :geos-pointer
    :documentation "The pointer to the GEOS representation of the geometry.")
   (cancel-finalization-fn
    :initform nil
    :accessor cancel-finalization-fn))
  (:documentation "Top level class for any GEOS geometry."))

(defgeneric make-geos-rep (geometry)
  (:documentation "Given an initialized Lisp representation of GEOMETRY, create
a GEOS representation of the geometry and return the pointer to that
representation."))

(defmethod initialize-instance :around ((self geometry) &rest initargs &key geos-pointer)
  "Ensure that, after all Lisp side initialization and checks are complete, that
a GEOS representation of the object is created and its pointer saved."
  (declare (ignore initargs))
  (prog1 (call-next-method)
    (unless geos-pointer
      (setf (geos-pointer self) (make-geos-rep self)))
    ;; Make a new binding the finalize-geometry? that we can use to control this
    ;; geometry's finalizer.
    (let ((finalize-geometry? t)
          (pointer (geos-pointer self)))
      (tg:finalize self #'(lambda ()
                            (when finalize-geometry?
                              (geos-geom-destroy pointer))))
      (setf (cancel-finalization-fn self) #'(lambda () (setf finalize-geometry? nil))))))

(defun release-ownership (geometry)
  ;; Release the ownership of a geometry by cancelling its finalizers.
  (funcall (cancel-finalization-fn geometry)))


;;; CFFI

(defcenum %geometry-types
  :point
  :line-string
  :linear-ring
  :polygon
  :multi-point
  :multi-line-string
  :multi-polygon
  :geometry-collection)

(define-foreign-type %geometry ()
  ()
  (:actual-type :pointer)
  (:simple-parser %geometry))

(define-foreign-type %geometry-array ()
  ()
  (:actual-type :pointer)
  (:simple-parser %geometry-array))

(define-geos-fun "GEOSGeom_destroy" :void
  (geometry-pointer :pointer))

(define-geos-fun "GEOSGeomTypeId" %geometry-types
  (geometry %geometry))

(define-geos-fun "GEOSGeom_createCollection" :pointer
  (type %geometry-types)
  (geoms %geometry-array)
  (num-geoms :uint))

(define-geos-fun "GEOSGeom_clone" %geometry
  (g %geometry))

(defun clone (geometry)
  "Clone the geometry."
  (geos-geom-clone geometry))

(defmethod translate-to-foreign ((value geometry) (type %geometry))
  (geos-pointer value))

(defmethod translate-to-foreign (value (type %geometry))
  value)

(defparameter *type-map* '((:point . point)
                           (:line-string . line-string)
                           (:linear-ring . linear-ring)
                           (:polygon . polygon)
                           (:multi-point . multi-point)
                           (:multi-line-string . multi-line-string)
                           (:multi-polygon . multi-polygon)))

(defmethod translate-from-foreign (value (type %geometry))
  (unless (null-pointer-p value)
    ;; Here's where things get fun. First, determine the type of the geometry from
    ;; GEOS, then instantiate the correct Lisp type.
    (make-instance (cdr (assoc (geos-geom-type-id value) *type-map*))
                   :geos-pointer value)))

(defmethod translate-to-foreign ((value list) (type %geometry-array))
  (cffi:foreign-alloc :pointer :initial-contents (mapcar #'geos-pointer value)))

(defmethod translate-to-foreign ((value array) (type %geometry-array))
  (cffi:foreign-alloc :pointer :initial-contents (map 'vector #'geos-pointer value)))

(defmethod free-translated-object (value (type %geometry-array) param)
  (declare (ignore param))
  (foreign-free value))

