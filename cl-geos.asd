;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;; CL-GEOS system definition

(asdf:defsystem #:cl-geos
  :description "A CFFI wrapper of GEOS."
  :version (:read-file-form "version.lisp-expr")
  :serial t
  :depends-on (#:cffi
               #:trivial-garbage)
  :author "Eric Timmons <etimmons@mit.edu>"
  :licence "Lisp-LGPL"
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
                         (:file "predicates")))))

