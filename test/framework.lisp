(defpackage #:geos/test
  (:use #:cl
        #:geos
        #:fiveam)
  (:shadowing-import-from #:geos
                          #:union
                          #:intersection))

(in-package #:geos/test)

(def-suite :cl-geos)

(defun wkt-transform-test (input transformation-function expected)
  (let (%input
        %output
        %expected)
    (setf %input (from-wkt-string input))
    (setf %output (funcall transformation-function %input))
    (setf %expected (from-wkt-string expected))
    (is (equals-exact-p %expected %output 1d-6))))

(defun wkt-binary-test (geom-1 geom-2 function geom-1.geom-2 geom-2.geom-1)
  (let ((%geom-1 (from-wkt-string geom-1))
        (%geom-2 (from-wkt-string geom-2)))
    (if geom-1.geom-2
        (is-true (funcall function %geom-1 %geom-2))
        (is-false (funcall function %geom-1 %geom-2)))
    (if geom-2.geom-1
        (is-true (funcall function %geom-2 %geom-1))
        (is-false (funcall function %geom-2 %geom-1)))))
