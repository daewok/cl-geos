;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL

(defpackage #:geos
  (:use :cl
        :cffi)
  (:shadow #:intersection
           #:union)
  (:export #:geometry
           #:clone

           #:point
           #:make-point
           #:make-point*

           #:line-string
           #:make-line-string

           #:linear-ring
           #:make-linear-ring

           #:polygon
           #:make-polygon

           #:multi-point
           #:make-multi-point

           #:multi-line-string
           #:make-multi-line-string

           #:multi-polygon
           #:make-multi-polygon

           ;; I/O functions
           #:*wkt-trim-p*
           #:*wkt-rounding-precision*
           #:*print-object-print-wkt*
           #:from-wkt-string
           #:to-wkt
           #:wkt-string
           #:to-wkb
           #:wkb-file

           ;; Topology Operations
           #:envelope
           #:intersection
           #:convex-hull
           #:difference
           #:sym-difference
           #:boundary
           #:union
           #:unary-union
           #:point-on-surface
           #:get-centroid
           #:node

           ;; Predicates
           #:disjoint-p
           #:touches-p
           #:intersects-p
           #:crosses-p
           #:within-p
           #:contains-p
           #:overlaps-p
           #:equals-p
           #:equals-exact-p
           #:covers-p
           #:covered-by-p
           #:is-empty-p
           #:is-simple-p
           #:is-ring-p
           #:is-closed-p
           #:is-valid-p))


