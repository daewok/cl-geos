(in-package #:geos/test)

(in-suite :cl-geos)

(def-test intersects-1 ()
  (wkt-binary-test "POLYGON EMPTY"
                   "POLYGON EMPTY"
                   #'intersects-p
                   nil
                   nil))

(def-test intersects-2 ()
  (wkt-binary-test "POLYGON((1 1,1 5,5 5,5 1,1 1))"
                   "POINT(2 2)"
                   #'intersects-p
                   t
                   t))

(def-test intersects-3 ()
  (wkt-binary-test "MULTIPOLYGON(((0 0,0 10,10 10,10 0,0 0)))"
                   "POLYGON((1 1,1 2,2 2,2 1,1 1))"
                   #'intersects-p
                   t
                   t))

