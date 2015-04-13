(in-package #:geos/test)

(in-suite :cl-geos)


(def-test get-centroid-1 ()
  (wkt-transform-test "POINT(10 0)"
                      #'get-centroid
                      "POINT(10 0)"))

(def-test get-centroid-2 ()
  (wkt-transform-test "LINESTRING(0 0, 10 0)"
                      #'get-centroid
                      "POINT (5 0)"))

(def-test get-centroid-3 ()
  (wkt-transform-test "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"
                      #'get-centroid
                      "POINT (5 5)"))

(def-test get-centroid-4 ()
  (wkt-transform-test "POLYGON(( \
56.528666666700 25.2101666667, \
56.529000000000 25.2105000000, \
56.528833333300 25.2103333333, \
56.528666666700 25.2101666667))"
                      #'get-centroid
                      "POINT (56.528833 25.210333)"))

(def-test get-centroid-5 ()
  (wkt-transform-test "LINESTRING EMPTY"
                      #'get-centroid
                      "POINT EMPTY"))
