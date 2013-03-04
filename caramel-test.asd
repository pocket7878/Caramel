#|
  This file is a part of caramel project.
  Copyright (c) 2013 Masato Sogame (poketo7878@gmail.com)
|#

(in-package :cl-user)
(defpackage caramel-test-asd
  (:use :cl :asdf))
(in-package :caramel-test-asd)

(defsystem caramel-test
  :author "Masato Sogame"
  :license "LLGPL"
  :depends-on (:caramel
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "caramel"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
