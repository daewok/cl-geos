;;;; Copyright 2015, Eric Timmons
;;;; Licensed under Lisp-LGPL
;;;; Contains code to load the GEOS library and some basic operations.

(in-package #:geos)

;;; Define and load the library.
(define-foreign-library libgeos
  (:unix (:or "libgeos_c.so.1" "libgeos_c.so"))
  (t (:default "libgeos_c")))

(use-foreign-library libgeos)


;;; Initialization and finalization.

(defctype %context-handle :pointer)

(defcfun "initGEOS_r" %context-handle
  (notice-func :pointer)
  (error-func :pointer))

(defcfun "finishGEOS_r" %context-handle
  (context %context-handle))

(defun init-geos ()
  "Return a new GEOS context handle."
  ;; using printf here is clearly suboptimal, but GEOS requires its error and
  ;; notice handlers to be variadic functions, which we can't define using
  ;; CFFI's defcallback.
  (initgeos-r (foreign-symbol-pointer "printf")
              (foreign-symbol-pointer "printf")))

(defun finish-geos (handle)
  "Destroy a GEOS handle."
  ;; For symmetry with init-geos.
  (finishgeos-r handle))

(defstruct (context-handle (:constructor %make-context-handle))
  pointer)

(defun make-context-handle ()
  (let* ((raw-pointer (init-geos))
         (handle (%make-context-handle :pointer raw-pointer)))
    (tg:finalize handle #'(lambda () (finish-geos raw-pointer)))
    handle))

(defvar *context-handle* nil)

(defun register-context-handle ()
  (setf *context-handle* (make-context-handle)))

(uiop:register-image-restore-hook 'register-context-handle)


;;; Basic datatypes and error handling.

(define-condition geos-error (error)
  ((message
    :initarg :message
    :reader geos-error-message))
  (:report
   (lambda (condition stream)
     (format stream "~A" (geos-error-message condition)))))

(define-foreign-type %geos-return ()
  ((error-on
    :initarg :error-on
    :reader error-on))
  (:actual-type :int))

(define-parse-method %geos-return (&key (error-on 0))
  (make-instance '%geos-return :error-on error-on))

(defmethod translate-from-foreign (value (type %geos-return))
  (when (= value (error-on type))
    (error 'geos-error :message "Unknown error."))
  value)

(define-foreign-type %geos-bool ()
  ((error-on
    :initarg :error-on
    :reader error-on))
  (:actual-type :char))

(define-parse-method %geos-bool (&key (error-on 2))
  (make-instance '%geos-bool :error-on error-on))

(defmethod translate-to-foreign (value (type %geos-bool))
  (if value
      1
      0))

(defmethod translate-from-foreign (value (type %geos-bool))
  (when (= value (error-on type))
    (error 'geos-error :message "Unknown error."))
  (not (zerop value)))


;;; Convenience functions.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun split-camel-case (string)
    (with-output-to-string (result)
      (loop :for c :across string
         :with last-was-lowercase := nil
         :when (and last-was-lowercase
                    (upper-case-p c))
           :do (princ "-" result)
         :if (lower-case-p c)
           :do (setf last-was-lowercase t)
         :else
           :do (setf last-was-lowercase nil)
         :if (equal #\_ c)
           :do (princ "-" result)
         :else
           :do (princ (char-upcase c) result)))))

(defmacro define-geos-fun (name-and-options return-type &body args)
  ;; Determine the lisp name of the raw function (with handler) argument and
  ;; without.
  (let* ((name (if (listp name-and-options)
                   (first name-and-options)
                   name-and-options))
         (make-friendly? (if (listp name-and-options)
                             (getf (rest name-and-options) :make-friendly? t)
                             t))
         (geos-prefix? (if (listp name-and-options)
                           (getf (rest name-and-options) :geos-prefix? t)
                           t))
         (predicate? (if (listp name-and-options)
                         (getf (rest name-and-options) :predicate? nil)
                         nil))
         (root-name (split-camel-case (subseq name 4)))
         (raw-cfun (intern (format nil "%GEOS-~A" root-name)))
         (friendly-fun (intern (format nil "~:[~;GEOS-~]~A~:[~;-P~]" geos-prefix? root-name predicate?)))
         (real-name (format nil "~A_r" name))
         (arg-names (mapcar #'first args)))
    `(progn
       (defcfun (,real-name ,raw-cfun) ,return-type
         (handle %context-handle)
         ,@args)
       ,(when make-friendly?
              `(progn (declaim (inline ,friendly-fun))
                      (defun ,friendly-fun ,arg-names
                        (,raw-cfun (context-handle-pointer *context-handle*) ,@arg-names)))))))

