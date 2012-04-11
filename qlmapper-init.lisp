(in-package #:cl-user)

(defmacro with-qlmapper-output ((stream file) &body body)
  `(with-open-file (,stream ,file :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
     ,@body))

(export 'with-qlmapper-output)
