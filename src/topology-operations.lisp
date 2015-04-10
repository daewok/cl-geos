;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;;
;;;; Contains bindings for GEOS's constructive operations.

(in-package #:geos)

(define-geos-fun ("GEOSEnvelope" :geos-prefix? nil) %geometry
  (geometry %geometry))

(define-geos-fun ("GEOSIntersection" :geos-prefix? nil) %geometry
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSConvexHull" :geos-prefix? nil) %geometry
  (geometry %geometry))

(define-geos-fun ("GEOSDifference" :geos-prefix? nil) %geometry
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSSymDifference" :geos-prefix? nil) %geometry
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSBoundary" :geos-prefix? nil) %geometry
  (geometry %geometry))

(define-geos-fun ("GEOSUnion" :geos-prefix? nil) %geometry
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSUnaryUnion" :geos-prefix? nil) %geometry
  (geometry %geometry))

(define-geos-fun ("GEOSPointOnSurface" :geos-prefix? nil) %geometry
  (geometry %geometry))

(define-geos-fun ("GEOSGetCentroid" :geos-prefix? nil) %geometry
  (geometry %geometry))

(define-geos-fun ("GEOSNode" :geos-prefix? nil) %geometry
  (geometry %geometry))

;; (define-geos-fun "GEOSClipByRect" %geometry
;;   (geometry %geometry)
;;   (xmin :double)
;;   (ymin :double)
;;   (xmax :double)
;;   (ymax :double))

;; (defun clip-by-rectangle (geometry xmin ymin xmax ymax)
;;   (geos-clip-by-rect geometry (coerce xmin 'double-float)
;;                      (coerce ymin 'double-float)
;;                      (coerce xmax 'double-float)
;;                      (coerce ymax 'double-float)))
