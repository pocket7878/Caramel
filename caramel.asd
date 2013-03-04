#|
  This file is a part of caramel project.
  Copyright (c) 2013 Masato Sogame (poketo7878@gmail.com)
|#

#|
  Author: Masato Sogame (poketo7878@gmail.com)
|#

(in-package :cl-user)
(defpackage caramel-asd
  (:use :cl :asdf))
(in-package :caramel-asd)

(defsystem caramel
  :version "0.1"
  :author "Masato Sogame"
  :license "LLGPL"
  :depends-on (:cxml
               :cxml-dom
               :closure-html
               :css-selectors
               :buildnode)
  :components ((:module "src"
                :components
                ((:file "caramel"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op caramel-test))))
