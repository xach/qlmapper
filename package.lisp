;;;; package.lisp

(defpackage #:qlmapper
  (:use #:cl)
  (:export #:*sbcl-program*
           #:map-releases
           #:map-systems
           #:map-loaded-systems)
  (:import-from #:sb-ext
                #:run-program
                #:native-namestring))

