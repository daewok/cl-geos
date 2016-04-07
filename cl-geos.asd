;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;; CL-GEOS system definition

(defsystem #:cl-geos
  :description "A CFFI wrapper of GEOS for performing geometric operations in Lisp."
  :version (:read-file-form "version.lisp-expr")
  :serial t
  :depends-on (#:cffi
               #:trivial-garbage
               #:xarray)
  :author "Eric Timmons <etimmons@mit.edu>"
  :license "Lisp-LGPL"
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "library")
                         (:file "geometry")
                         (:file "coordinate-sequences")
                         (:file "point")
                         (:file "line-string")
                         (:file "linear-ring")
                         (:file "polygon")
                         (:file "multi-point")
                         (:file "multi-line-string")
                         (:file "multi-polygon")
                         (:file "io")
                         (:file "topology-operations")
                         (:file "predicates"))))
  :in-order-to ((test-op (load-op #:cl-geos/test)))
  :perform (test-op :after (op c)
                    (funcall (read-from-string "fiveam:run!") :cl-geos)))

(defsystem #:cl-geos/test
  :description "Tests for CL-GEOS."
  :version (:read-file-form "version.lisp-expr")
  :serial t
  :depends-on (#:cl-geos
               #:fiveam)
  :author "Eric Timmons <etimmons@mit.edu>"
  :license "Lisp-LGPL"
  :components ((:module "test"
                        :components
                        ((:file "framework")
                         (:file "contains-test")
                         (:file "convex-hull-test")
                         (:file "get-centroid-test")
                         (:file "intersects-test")))))
