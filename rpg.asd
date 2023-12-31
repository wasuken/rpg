(defsystem "rpg"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "rpg/tests"))))

(defsystem "rpg/tests"
  :author ""
  :license ""
  :depends-on ("rpg"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for rpg"
  :perform (test-op (op c) (symbol-call :rove :run c)))
