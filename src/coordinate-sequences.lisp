;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;;
;;;; Defines operations on coordinate sequences. The internal representation of
;;;; coordinate sequences is an array with points on the rows, x in column 0,
;;;; and y in column 1.

(in-package #:geos)

(declaim (inline assert-coordinate-sequence))
(defun assert-coordinate-sequence (seq)
  (assert (and (arrayp seq)
               (= 2 (array-rank seq))
               (= 2 (array-dimension seq 1)))))

(defgeneric ensure-coordinate-sequence (seq))

(defmethod ensure-coordinate-sequence ((seq array))
  (assert-coordinate-sequence seq)
  seq)

(defmethod ensure-coordinate-sequence ((seq list))
  (assert (every #'listp seq))
  (let ((first-length (length (first seq)))
        (seq-length (length seq)))
    (assert (= 2 first-length))
    (assert (every #'(lambda (x) (= first-length (length x))) seq))
    (make-array (list seq-length first-length) :initial-contents seq :adjustable t)))

(defun make-ring (array)
  "Make the given array into a ring by copying the first column as the last
column."
  (destructuring-bind (old-length dims) (array-dimensions array)
    (let ((new-array (adjust-array array (list (1+ old-length) dims))))
      (dotimes (idx dims)
        (setf (aref new-array old-length idx) (aref new-array 0 idx)))
      new-array)))

(defun ensure-coordinate-ring (seq)
  (setf seq (ensure-coordinate-sequence seq))
  (destructuring-bind (length dims) (array-dimensions seq)
    (declare (ignore dims))
    (unless (and (= (aref seq 0 0)
                    (aref seq (1- length) 0))
                 (= (aref seq 0 1)
                    (aref seq (1- length) 1)))
      (setf seq (make-ring seq)))
    seq))



;;; CFFI

(defctype %coord-sequence :pointer)

(define-geos-fun "GEOSCoordSeq_create" %coord-sequence
  (size :unsigned-int)
  (dims :unsigned-int))

(define-geos-fun "GEOSCoordSeq_setX" %geos-return
  (sequence %coord-sequence)
  (idx :unsigned-int)
  (value :double))

(define-geos-fun "GEOSCoordSeq_setY" %geos-return
  (sequence %coord-sequence)
  (idx :unsigned-int)
  (value :double))

(define-geos-fun ("GEOSCoordSeq_getX" :make-friendly? nil) %geos-return
  (sequence %coord-sequence)
  (idx :unsigned-int)
  (value :double))

(defun geos-coord-seq-get-x (sequence idx)
  (with-foreign-object (value :double)
    (%geos-coord-seq-get-x (context-handle-pointer *context-handle*) sequence idx value)
    (mem-ref value :double)))

(define-geos-fun ("GEOSCoordSeq_getY" :make-friendly? nil) %geos-return
  (sequence %coord-sequence)
  (idx :unsigned-int)
  (value :double))

(defun geos-coord-seq-get-y (sequence idx)
  (with-foreign-object (value :double)
    (%geos-coord-seq-get-y (context-handle-pointer *context-handle*) sequence idx value)
    (mem-ref value :double)))

(define-geos-fun ("GEOSCoordSeq_getOrdinate" :make-friendly? nil) %geos-return
  (sequence %coord-sequence)
  (idx :unsigned-int)
  (dim :unsigned-int)
  (value :pointer))

(defun geos-coord-seq-get-ordinate (sequence idx dim)
  (with-foreign-object (value :double)
    (%geos-coord-seq-get-ordinate (context-handle-pointer *context-handle*) sequence idx dim value)
    (mem-ref value :double)))

(define-geos-fun ("GEOSGeom_getCoordSeq") %coord-sequence
  (g %geometry))

(define-geos-fun ("GEOSCoordSeq_getDimensions" :make-friendly? nil) %geos-return
  (s %coord-sequence)
  (dims :pointer))

(defun geos-coord-seq-get-dimensions (sequence)
  (with-foreign-object (value :int)
    (%geos-coord-seq-get-dimensions (context-handle-pointer *context-handle*) sequence value)
    (mem-ref value :int)))

(define-geos-fun ("GEOSCoordSeq_getSize" :make-friendly? nil) %geos-return
  (s %coord-sequence)
  (size :pointer))

(defun geos-coord-seq-get-size (sequence)
  (with-foreign-object (value :int)
    (%geos-coord-seq-get-size (context-handle-pointer *context-handle*) sequence value)
    (mem-ref value :int)))
