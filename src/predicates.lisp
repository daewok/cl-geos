;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;;
;;;; Contains bindings for GEOS's predicates.

(in-package #:geos)

(define-geos-fun ("GEOSDisjoint" :geos-prefix? nil :predicate? t) %geos-bool
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSTouches" :geos-prefix? nil :predicate? t) %geos-bool
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSIntersects" :geos-prefix? nil :predicate? t) %geos-bool
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSCrosses" :geos-prefix? nil :predicate? t) %geos-bool
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSWithin" :geos-prefix? nil :predicate? t) %geos-bool
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSContains" :geos-prefix? nil :predicate? t) %geos-bool
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSOverlaps" :geos-prefix? nil :predicate? t) %geos-bool
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSEquals" :geos-prefix? nil :predicate? t) %geos-bool
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSEqualsExact" :geos-prefix? nil :predicate? t) %geos-bool
  (geometry-1 %geometry)
  (geometry-2 %geometry)
  (tolerance :double))

(define-geos-fun ("GEOSCovers" :geos-prefix? nil :predicate? t) %geos-bool
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSCoveredBy" :geos-prefix? nil :predicate? t) %geos-bool
  (geometry-1 %geometry)
  (geometry-2 %geometry))

(define-geos-fun ("GEOSisEmpty" :geos-prefix? nil :predicate t) %geos-bool
  (geometry %geometry))

(define-geos-fun ("GEOSisSimple" :geos-prefix? nil :predicate t) %geos-bool
  (geometry %geometry))

(define-geos-fun ("GEOSisRing" :geos-prefix? nil :predicate t) %geos-bool
  (geometry %geometry))

(define-geos-fun ("GEOSisClosed" :geos-prefix? nil :predicate t) %geos-bool
  (geometry %geometry))

(define-geos-fun ("GEOSisValid" :geos-prefix? nil :predicate t) %geos-bool
  (geometry %geometry))
