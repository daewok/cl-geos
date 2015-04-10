;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;;
;;;; Basic multi-line-string definitions.

(in-package #:geos)

(defclass multi-line-string (geometry)
  ((cached-line-strings
    :initarg :line-strings
    :accessor cached-line-strings)))

(defmethod make-geos-rep ((self multi-line-string))
  ;; Release ownership of all line strings.
  (loop :for line-string :across (cached-line-strings self)
     :do (release-ownership line-string))
  ;; Make the multi-line-string.
  (geos-geom-create-collection :multi-line-string (cached-line-strings self)
                               (array-dimension (cached-line-strings self) 0)))

(defun make-multi-line-string (line-strings)
  (make-instance 'multi-line-string :line-strings (map 'vector #'ensure-line-string line-strings)))
