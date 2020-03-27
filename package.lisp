;;;; package.lisp

(cl:in-package common-lisp-user)


(defpackage "https://github.com/g000001/srfi-101"
  (:use)
  (:export
   quote pair? cons car cdr caar cadr cddr cdar caaar caadr caddr
   cadar cdaar cdadr cdddr cddar caaaar caaadr caaddr caadar cadaar
   cadadr cadddr caddar cdaaar cdaadr cdaddr cdadar cddaar cddadr
   cddddr cdddar null? list? list make-list length append reverse
   list-tail list-ref list-set list-ref/update map for-each
   random-access-list->linear-access-list
   linear-access-list->random-access-list))


(defpackage "https://github.com/g000001/srfi-101#internals"
  (:use 
   rnrs-compat
   fiveam
   mbe
   "https://github.com/g000001/srfi-11"
   "https://github.com/g000001/srfi-16")
  (:import-from cl funcall)
  (:shadowing-import-from 
   "https://github.com/g000001/srfi-101"
   quote pair? cons car cdr caar cadr cddr cdar caaar caadr caddr
   cadar cdaar cdadr cdddr cddar caaaar caaadr caaddr caadar cadaar
   cadadr cadddr caddar cdaaar cdaadr cdaddr cdadar cddaar cddadr
   cddddr cdddar null? list? list make-list length append reverse
   list-tail list-ref list-set list-ref/update map for-each
   random-access-list->linear-access-list
   linear-access-list->random-access-list))


;;; *EOF*
