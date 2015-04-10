;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;;
;;;; IO using WKT and WKB

(in-package #:geos)

;;; WKT Reading

(define-geos-fun "GEOSWKTReader_create" :pointer)

(define-geos-fun "GEOSWKTReader_destroy" :void
  (reader :pointer))

(define-geos-fun "GEOSWKTReader_read" %geometry
  (reader :pointer)
  (wkt :string))

(defun from-wkt-string (string)
  (let ((reader (geos-wktreader-create)))
    (prog1 (geos-wktreader-read reader string)
      (geos-wktreader-destroy reader))))


;;; WKT Writing

(defparameter *wkt-trim-p* nil
  "If non-NIL, the number of decimals in WKT output is trimmed to the minimum
necessary.")

(defparameter *wkt-rounding-precision* -1
  "Set the precision of the output. If -1, prints the maximum precision.")

(defparameter *print-object-print-wkt* t
  "If T, prints the WKT representation of geometries in print-object.")

(define-geos-fun "GEOSWKTWriter_create" :pointer)

(define-geos-fun "GEOSWKTWriter_destroy" :void
  (writer :pointer))

(define-geos-fun "GEOSWKTWriter_write" (:string :free-from-foreign t)
  (writer :pointer)
  (geometry %geometry))

(define-geos-fun "GEOSWKTWriter_setRoundingPrecision" :void
  (writer :pointer)
  (precision :int))

(define-geos-fun "GEOSWKTWriter_setTrim" :void
  (writer :pointer)
  (trim %geos-bool))

(defun to-wkt (geometry &optional (stream *standard-output*))
  (let ((writer (geos-wktwriter-create)))
    (geos-wktwriter-set-trim writer *wkt-trim-p*)
    (geos-wktwriter-set-rounding-precision writer *wkt-rounding-precision*)
    (format stream "~A" (geos-wktwriter-write writer geometry))
    (geos-wktwriter-destroy writer)))

(defun wkt-string (geometry)
  (with-output-to-string (stream)
    (to-wkt geometry stream)))

(defmethod print-object ((self geometry) stream)
  (print-unreadable-object (self stream :type t :identity (not *print-object-print-wkt*))
    (when *print-object-print-wkt*
      (let ((*wkt-trim-p* t))
        (princ "WKT: \"" stream)
        (to-wkt self stream)
        (princ "\"" stream)))))



;;; WKB Writing

(define-geos-fun "GEOSWKBWriter_create" :pointer)

(define-geos-fun "GEOSWKBWriter_destroy" :void
  (writer :pointer))

(define-geos-fun ("GEOSWKBWriter_write" :make-friendly? nil) :pointer
  (writer :pointer)
  (geometry %geometry)
  (size-out :pointer))

(defun geos-wkbwriter-write (writer geometry stream)
  (with-foreign-object (size :unsigned-int)
    (let ((output-ptr (%geos-wkbwriter-write (context-handle-pointer *context-handle*) writer geometry size)))
      (dotimes (i (mem-aref size :unsigned-int))
        (write-byte (mem-aref output-ptr :unsigned-char i) stream))
      (foreign-free output-ptr))))

(defun to-wkb (geometry &optional (stream *standard-output*))
  (let ((writer (geos-wkbwriter-create)))
    (geos-wkbwriter-write writer geometry stream)
    (geos-wkbwriter-destroy writer)))

(defun wkb-file (geometry filespec &key (if-exists :error))
  (with-open-file (stream filespec :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists if-exists)
    (to-wkb geometry stream)))
