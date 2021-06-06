(defsystem "cl-json-path"
  :version "0.1.0"
  :author "Zhuo Hong Wei"
  :license "MIT"
  :depends-on (:alexandria
               :cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-json-path/tests"))))

(defsystem "cl-json-path/tests"
  :author "Zhuo Hong Wei"
  :license "MIT"
  :depends-on ("cl-json-path"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-json-path"
  :perform (test-op (op c) (symbol-call :rove :run c)))
