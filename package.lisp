;;;; package.lisp

(cl:in-package :cl-user)


;(delete-package :srfi-101)
(defpackage :srfi-101
  (:use)
  (:export
   :quote :pair? :cons :car :cdr :caar :cadr :cddr :cdar :caaar :caadr :caddr
   :cadar :cdaar :cdadr :cdddr :cddar :caaaar :caaadr :caaddr :caadar :cadaar
   :cadadr :cadddr :caddar :cdaaar :cdaadr :cdaddr :cdadar :cddaar :cddadr
   :cddddr :cdddar :null? :list? :list :make-list :length :append :reverse
   :list-tail :list-ref :list-set :list-ref/update :map :for-each
   :random-access-list->linear-access-list
   :linear-access-list->random-access-list))


;; (delete-package :srfi-101.internal)
(defpackage :srfi-101.internal
  (:use :rnrs-compat :fiveam :srfi-11 :mbe :srfi-16)
  (:import-from :cl :funcall))


(defpackage :srfi-101.test
  (:use :rnrs-compat :fiveam)
  (:shadowing-import-from :srfi-101
   :quote :pair? :cons :car :cdr :caar :cadr :cddr :cdar :caaar :caadr :caddr
   :cadar :cdaar :cdadr :cdddr :cddar :caaaar :caaadr :caaddr :caadar :cadaar
   :cadadr :cadddr :caddar :cdaaar :cdaadr :cdaddr :cdadar :cddaar :cddadr
   :cddddr :cdddar :null? :list? :list :make-list :length :append :reverse
   :list-tail :list-ref :list-set :list-ref/update :map :for-each
   :random-access-list->linear-access-list
   :linear-access-list->random-access-list))


#|(mapcar #'kl:ensure-keyword 
        '(quote pair? cons car cdr 
   caar cadr cddr cdar
   caaar caadr caddr cadar
   cdaar cdadr cdddr cddar
   caaaar caaadr caaddr caadar
   cadaar cadadr cadddr caddar
   cdaaar cdaadr cdaddr cdadar
   cddaar cddadr cddddr cdddar
   null? list? list length 
   append reverse list-tail
   list-ref map for-each))|#
()

#|(mapcar (arc:compose #'kl:ensure-keyword 
                     #'second)
        '((quote quote)
          (pair? pair?) 
          (cons cons)
          (car car) 
          (cdr cdr)
          (caar caar) 
          (cadr cadr)
          (cddr cddr)
          (cdar cdar)
          (caaar caaar)
          (caadr caadr)
          (caddr caddr)
          (cadar cadar)
          (cdaar cdaar)
          (cdadr cdadr)
          (cdddr cdddr)
          (cddar cddar)
          (caaaar caaaar)
          (caaadr caaadr)
          (caaddr caaddr)
          (caadar caadar)
          (cadaar cadaar)
          (cadadr cadadr)
          (cadddr cadddr)
          (caddar caddar)
          (cdaaar cdaaar)
          (cdaadr cdaadr)
          (cdaddr cdaddr)
          (cdadar cdadar)
          (cddaar cddaar)
          (cddadr cddadr)
          (cddddr cddddr)
          (cdddar cdddar)
          (null? null?)
          (list? list?)
          (list list)
          (make-list make-list)
          (length length)
          (append append)
          (reverse reverse)
          (list-tail list-tail)
          (list-ref list-ref)
          (list-set list-set)
          (list-ref/update list-ref/update)
          (map map)
          (for-each for-each)
          (random-access-list->linear-access-list
           random-access-list->linear-access-list)
          (linear-access-list->random-access-list
           linear-access-list->random-access-list)))|#

