;;;; srfi-101.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :srfi-101
  :version "20200328"
  :description "SRFI 101 for CL: Purely Functional Random-Access Pairs and Lists"
  :long-description "SRFI 101 for CL: Purely Functional Random-Access Pairs and Lists
https://srfi.schemers.org/srfi-101"
  :author "David Van Horn"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:fiveam :srfi-1 :srfi-16 :rnrs-compat :srfi-11 :srfi-1)
  :components ((:file "package")
               (:file "srfi-101")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-101))))
  (let ((name "https://github.com/g000001/srfi-101")
        (nickname :srfi-101))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-101))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-101#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-101)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
