(in-package #:geos/test)

(in-suite :cl-geos)


(def-test convex-hull-1 ()
  (wkt-transform-test "MULTIPOINT (130 240, 130 240, 130 240, 570 240, 570 240, 570 240, 650 240)"
                      #'convex-hull
                      "LINESTRING (130 240, 650 240)"))
