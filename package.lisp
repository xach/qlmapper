;;;; package.lisp

(defpackage #:qlmapper
  (:use #:cl)
  (:export #:*sbcl-program*
           #:map-releases
           #:map-systems)
  (:import-from #:sb-ext
                #:run-program
                #:native-namestring))

