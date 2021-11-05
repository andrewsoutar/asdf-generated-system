(uiop:define-package #:com.andrewsoutar.asdf-generated-system
  (:nicknames #:com.andrewsoutar.asdf-generated-system/main)
  (:use #:cl #:uiop #:asdf)
  (:export #:generated-component #:generate-op)
  (:export #:find-primary-system #:generated-system #:generated-system-dependencies))
(cl:in-package #:com.andrewsoutar.asdf-generated-system/main)

(defclass generated-component () ())

(defclass generate-op (non-propagating-operation) ())

(defmethod component-depends-on ((o prepare-op) (c generated-component))
  `((generate-op ,c) ,@(call-next-method)))
(defmethod component-depends-on ((o prepare-source-op) (c generated-component))
  `((generate-op ,c) ,@(call-next-method)))

;;; HACK There's gotta be a better way to do this...
(defmethod component-depends-on ((o asdf/concatenate-source::basic-concatenate-source-op) (s system))
  `((generate-op ,@(required-components s :goal-operation 'load-op
                                          :keep-operation 'generate-op
                                          :other-systems (operation-monolithic-p o)))
    ,@(call-next-method)))


(defclass generated-system (system generated-component) ())

(defgeneric generated-system-dependencies (system subname)
  (:method-combination append))

(defun find-primary-system (system)
  (let* ((system-name (coerce-name system))
         (primary-name (primary-system-name system-name)))
    (unless (equal primary-name system-name)
      (values (find-system primary-name nil)
              primary-name
              (subseq system-name (1+ (length primary-name)))))))

(defmethod output-files ((o generate-op) (c generated-system))
  (multiple-value-bind (primary primary-name subname) (find-primary-system c)
    (declare (ignore primary-name))
    (let* ((gen-dir (system-relative-pathname (or primary c) "generated" :type :directory))
           (dir (subpathname gen-dir subname :type :directory)))
      (list (subpathname dir "generated.lisp")))))

(defclass generated-system-cl-file (cl-source-file) ())

(defmethod component-pathname ((c generated-system-cl-file))
  (output-file 'generate-op (component-parent c)))

(defun sysdef-generated-system-search (system-name)
  (multiple-value-bind (primary primary-name subname) (find-primary-system system-name)
    (declare (ignore primary-name))
    (when (typep primary 'generated-system)
      (eval `(defsystem ,system-name
               :class ,(class-of primary)
               :source-file ,(system-source-file primary)
               :pathname ,(component-pathname primary)
               :depends-on ,(generated-system-dependencies primary subname)
               :around-compile ,(asdf/component:around-compile-hook primary)
               :components ((generated-system-cl-file "generated")))))))

(pushnew 'sysdef-generated-system-search *system-definition-search-functions*)
