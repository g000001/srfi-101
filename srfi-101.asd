;;;; srfi-101.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :srfi-101
  :serial t
  :depends-on (:fiveam :srfi-1 :srfi-16 :rnrs-compat :srfi-11 :srfi-1)
  :components ((:file "package")
               (:file "srfi-101")))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-101))))
  (load-system :srfi-101)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-101.test :srfi-101))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

