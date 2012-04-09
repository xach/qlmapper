;;;; qlmapper.lisp

(in-package #:qlmapper)

;;; "qlmapper" goes here. Hacks and glory await!

(defvar *sbcl-program* sb-ext:*runtime-pathname*)

(defun eval-defvar-forms (environment-pairs)
  (loop for (name value) on environment-pairs by #'cddr
        for sym = (format nil "cl-user::~A" name)
        collect "--eval"
        collect (format nil "(defvar ~A (sb-posix:getenv ~S))" sym name)
        collect "--eval"
        collect (format nil "(export '~A '#:cl-user)" sym)))

(defun environment-list (environment-pairs)
  (loop for (name value) on environment-pairs by #'cddr
        collect (format nil "~A=~A" name value)))

(defun flatlist (&rest args)
  (alexandria:flatten args))

(defun run-sbcl (file &rest environment-pairs)
  (run-program (native-namestring (pathname *sbcl-program*))
               (flatlist "--noinform"
                         "--non-interactive"
                         "--no-userinit"
                         "--no-sysinit"
                         "--load" (native-namestring
                                   (ql-setup:qmerge "setup.lisp"))
                         (eval-defvar-forms environment-pairs)
                         "--eval"
                         (format nil  "(setf cl:*default-pathname-defaults* ~
                                       #p~S)"
                                 (native-namestring *default-pathname-defaults*))
                         "--load" (native-namestring
                                   (truename file)))
               :environment (append (environment-list environment-pairs)
                                    (sb-ext:posix-environ))
               :output *standard-output*))


(defgeneric base-directory (object)
  (:method ((release ql-dist:release))
    (ql-dist:base-directory release))
  (:method ((system ql-dist:system))
    (base-directory (ql-dist:release system))))

(defun map-objects (file
                    &key dist-name function)
  (unless (probe-file file)
    (error "~S does not exist" file))
  (let ((dist (ql-dist:find-dist dist-name)))
    (unless dist
      (error "~S does not name any known dist" dist-name))
    (let ((objects (funcall function dist)))
      (dolist (object objects)
        (ql-dist:ensure-installed object)
        (let ((*default-pathname-defaults*
               (base-directory object)))
          (run-sbcl file
                    "*qlmapper-object-name*"
                    (ql-dist:name object)))))))

(defun map-releases (file &optional (dist-name "quicklisp"))
  "For each release in a dist (defaults to the \"quicklisp\" dist),
  start an independent SBCL process and load FILE with the variable
  CL-USER:*QLMAPPER-OBJECT-NAME* bound to the release's name."
  (map-objects file
               :dist-name dist-name
               :function #'ql-dist:provided-releases))

(defun map-systems (file &optional (dist-name "quicklisp"))
  "For each system in a dist (defaults to the \"quicklisp\" dist),
  start an independent SBCL process and load FILE with the variable
  CL-USER:*QLMAPPER-OBJECT-NAME* bound to the system's name."
  (map-objects file
               :dist-name dist-name
               :function #'ql-dist:provided-systems))

