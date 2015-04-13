(in-package #:geos/test)

(in-suite :cl-geos)

(def-test contains-1 ()
  (wkt-binary-test "POLYGON EMPTY"
                   "POLYGON EMPTY"
                   #'contains-p
                   nil
                   nil))

(def-test contains-2 ()
  (wkt-binary-test "POLYGON((1 1,1 5,5 5,5 1,1 1))"
                   "POINT(2 2)"
                   #'contains-p
                   t
                   nil))

(def-test contains-3 ()
  (wkt-binary-test "MULTIPOLYGON(((0 0,0 10,10 10,10 0,0 0)))"
                   "POLYGON((1 1,1 2,2 2,2 1,1 1))"
                   #'contains-p
                   t
                   nil))
