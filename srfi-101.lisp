(cl:in-package :srfi-101.internal)

;; SRFI 101: Purely Functional Random-Access Pairs and Lists
;; Copyright (c) David Van Horn 2009.  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. REMEMBER, THERE IS NO SCHEME UNDERGROUND. IN NO EVENT
;; SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
;; THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; Ported to Common Lisp by Masaomi Chiba

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defstruct (kons (:predicate kons?)
                      (:constructor make-kons (size tree rest)))
    size tree rest))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defstruct (node (:predicate node?)
                      (:constructor make-node (val left right)))
    val left right))

(cl:declaim (cl:inline make-foldl make-foldr
                       srfi-101::foldl/1 srfi-101::foldr/1
                       car cdr null?))

;; Nat -> Nat
(define (sub1 n) (- n 1))
(define (add1 n) (+ n 1))
    
;; [Tree X] -> X
(define (tree-val t)
  (if (node? t) 
      (node-val t)
      t))


;; [X -> Y] [Tree X] -> [Tree Y]
(define (tree-map f t)
  (if (node? t)
      (make-node (funcall f (node-val t))
                 (tree-map f (node-left t))
                 (tree-map f (node-right t)))
      (funcall f t)))

;; [X -> Y] [Tree X] -> unspecified
(define (tree-for-each f t)
  (if (node? t)
      (begin (funcall f (node-val t))
             (tree-for-each f (node-left t))
             (tree-for-each f (node-right t)))
      (funcall f t)))

;; [X Y Z ... -> R] [List [Tree X] [Tree Y] [Tree Z] ...] -> [Tree R]
(define (tree-map/n f ts)
  (let recr ((ts ts))
       (if (and (pair? ts)
                (node? (car ts)))
           (make-node (apply f (map #'node-val ts))
                      (recr (map #'node-left ts))
                      (recr (map #'node-right ts)))
           (apply f ts))))
  
;; [X Y Z ... -> R] [List [Tree X] [Tree Y] [Tree Z] ...] -> unspecified
(define (tree-for-each/n f ts)
  (let recr ((ts ts))
      (if (and (pair? ts)
               (node? (car ts)))
          (begin (apply f (map #'node-val ts))
                 (recr (map #'node-left ts))
                 (recr (map #'node-right ts)))
          (apply f ts))))
  
;; Nat [Nat -> X] -> [Tree X]
;; like build-list, but for complete binary trees
(define (build-tree i f) ;; i = 2^j-1
  (let rec ((i i) (o 0))
    (if (= 1 i) 
        (funcall f o)
        (let ((i/2 (half i)))
          (make-node (funcall f o)
                     (rec i/2 (add1 o))
                     (rec i/2 (+ 1 o i/2)))))))
  
;; Consumes n = 2^i-1 and produces 2^(i-1)-1.
;; Nat -> Nat
(define (half n)
  ;; (bitwise-arithmetic-shift n -1)
  (cl:ash n -1))

;; Nat X -> [Tree X]
(define (tr..make-tree i x) ;; i = 2^j-1
  (let recr ((i i))
    (if (= 1 i) 
        x
        (let ((n (recr (half i))))
          (make-node x n n)))))

;; Nat [Tree X] Nat [X -> X] -> X [Tree X]
(define (tree-ref/update mid t i f)
  (cond ((zero? i)
         (if (node? t) 
             (values (node-val t)
                     (make-node (funcall f (node-val t))
                                (node-left t)
                                (node-right t)))
             (values t (funcall f t))))
        ((<= i mid)
         (let-values (((v* t*) (tree-ref/update (half (sub1 mid)) 
                                                        (node-left t) 
                                                        (sub1 i) 
                                                        f)))
           (values v* (make-node (node-val t) t* (node-right t)))))
        (else           
         (let-values (((v* t*) (tree-ref/update (half (sub1 mid)) 
                                                (node-right t) 
                                                (sub1 (- i mid)) 
                                                f)))
           (values v* (make-node (node-val t) (node-left t) t*))))))
  
;; Special-cased above to avoid logarathmic amount of cons'ing
;; and any multi-values overhead.  Operates in constant space.
;; [Tree X] Nat Nat -> X
;; invariant: (= mid (half (sub1 (tree-count t))))
(define (tree-ref/a t i mid) 
  (cond ((zero? i) (tree-val t))
        ((<= i mid) 
         (tree-ref/a (node-left t) 
                     (sub1 i) 
                     (half (sub1 mid))))
        (else 
         (tree-ref/a (node-right t) 
                     (sub1 (- i mid)) 
                     (half (sub1 mid))))))
  
;; Nat [Tree X] Nat -> X
;; invariant: (= size (tree-count t))
(define (tree-ref size t i)
  (if (zero? i)
      (tree-val t)
      (tree-ref/a t i (half (sub1 size)))))
  
;; Nat [Tree X] Nat [X -> X] -> [Tree X]
(define (tree-update size t i f)
  (let recr ((mid (half (sub1 size))) (t t) (i i))
    (cond ((zero? i)
           (if (node? t)
               (make-node (funcall f (node-val t))
                             (node-left t)
                             (node-right t))
               (funcall f t)))
          ((<= i mid)
           (make-node (node-val t) 
                      (recr (half (sub1 mid))
                            (node-left t) 
                            (sub1 i)) 
                      (node-right t)))
          (else
           (make-node (node-val t) 
                      (node-left t) 
                      (recr (half (sub1 mid))
                            (node-right t) 
                            (sub1 (- i mid))))))))

;; ------------------------
;; Random access lists
  
;; [RaListof X]
(cl:defconstant srfi-101::null (quote ()))

;; [Any -> Boolean]
(define srfi-101:pair? #'kons?)

;; [Any -> Boolean]
(define srfi-101:null? #'null?)

;; X [RaListof X] -> [RaListof X]  /\
;; X Y -> [RaPair X Y]
(define (srfi-101:cons x ls)
  (if (kons? ls)
      (let ((s (kons-size ls)))
        (if (and (kons? (kons-rest ls))
                 (= (kons-size (kons-rest ls))
                    s))
            (make-kons (+ 1 s s) 
                       (make-node x 
                                  (kons-tree ls)
                                  (kons-tree (kons-rest ls)))
                       (kons-rest (kons-rest ls)))
            (make-kons 1 x ls)))
      (make-kons 1 x ls)))

;; [RaPair X Y] -> X Y
(define srfi-101::car+cdr 
  (lambda (p)
    (cl:assert (kons? p))
    (if (node? (kons-tree p))
        (let ((s* (half (kons-size p))))
          (values (tree-val (kons-tree p))
                  (make-kons s* 
                             (node-left (kons-tree p))
                             (make-kons s*
                                        (node-right (kons-tree p))
                                        (kons-rest p)))))
        (values (kons-tree p) (kons-rest p)))))

;; [RaPair X Y] -> X
(define (srfi-101:car p)
  (call-with-values (lambda () (srfi-101::car+cdr p))
                    (lambda (car cdr) (cl:declare (cl:ignore cdr)) car)))

;; [RaPair X Y] -> Y
(define (srfi-101:cdr p)
  (call-with-values (lambda () (srfi-101::car+cdr p))
                    (lambda (car cdr) (cl:declare (cl:ignore car)) cdr)))

;; [RaListof X] Nat [X -> X] -> X [RaListof X]
(define (srfi-101:list-ref/update ls i f)
                                        ;(assert (< i (srfi-101:length ls)))
  (let recr ((xs ls) (j i))
    (if (< j (kons-size xs))
        (let-values (((v* t*) 
                      (tree-ref/update (half (sub1 (kons-size xs))) 
                                       (kons-tree xs) j f)))
          (values v* (make-kons (kons-size xs) 
                                t* 
                                (kons-rest xs))))
        (let-values (((v* r*) 
                      (recr (kons-rest xs) 
                            (- j (kons-size xs)))))
          (values v* (make-kons (kons-size xs) 
                                (kons-tree xs) 
                                r*))))))

;; [RaListof X] Nat [X -> X] -> [RaListof X]
(define (srfi-101::list-update ls i f)
  ;(assert (< i (srfi-101:length ls)))
  (let recr ((xs ls) (j i))
    (let ((s (kons-size xs)))
      (if (< j s) 
          (make-kons s (tree-update s (kons-tree xs) j f) (kons-rest xs))
          (make-kons s (kons-tree xs) (recr (kons-rest xs) (- j s)))))))

;; [RaListof X] Nat X -> (values X [RaListof X])
(define (srfi-101::list-ref/set ls i v)
  (srfi-101:list-ref/update ls i (lambda (_) (cl:declare (cl:ignore _)) v)))

;; X ... -> [RaListof X]
(define (srfi-101:list . xs)
  (srfi-1:fold-right #'srfi-101:cons srfi-101::null xs))

;; Nat X -> [RaListof X]
(define srfi-101:make-list
  (case-lambda
   ((k) (srfi-101::make-list k 0))
   ((k obj)
    (let loop ((n k) (a srfi-101::null))
         (cond ((zero? n) a)
               (else 
                (let ((t (largest-skew-binary n)))
                  (loop (- n t)
                        (make-kons t (tr..make-tree t obj) a)))))))))

;; A Skew is a Nat 2^k-1 with k > 0.

;; Skew -> Skew
(define (skew-succ t) (add1 #|(bitwise-arithmetic-shift t 1)|#
                       (cl:ash t 1)))

;; Computes the largest skew binary term t <= n.
;; Nat -> Skew
(define (largest-skew-binary n)
  (if (= 1 n) 
      1
      (let* ((t (largest-skew-binary (half n)))
             (s (skew-succ t)))
        (if (> s n) t s))))  

;; [Any -> Boolean]
;; Is x a PROPER list?
(define (srfi-101:list? x)
  (or (srfi-101:null? x)
      (and (kons? x)
           (srfi-101:list? (kons-rest x)))))

(define srfi-101:caar (lambda (ls) (srfi-101:car (srfi-101:car ls))))
(define srfi-101:cadr (lambda (ls) (srfi-101:car (srfi-101:cdr ls))))
(define srfi-101:cddr (lambda (ls) (srfi-101:cdr (srfi-101:cdr ls))))
(define srfi-101:cdar (lambda (ls) (srfi-101:cdr (srfi-101:car ls))))

(define srfi-101:caaar (lambda (ls) (srfi-101:car (srfi-101:car (srfi-101:car ls)))))
(define srfi-101:caadr (lambda (ls) (srfi-101:car (srfi-101:car (srfi-101:cdr ls)))))
(define srfi-101:caddr (lambda (ls) (srfi-101:car (srfi-101:cdr (srfi-101:cdr ls)))))
(define srfi-101:cadar (lambda (ls) (srfi-101:car (srfi-101:cdr (srfi-101:car ls)))))
(define srfi-101:cdaar (lambda (ls) (srfi-101:cdr (srfi-101:car (srfi-101:car ls)))))
(define srfi-101:cdadr (lambda (ls) (srfi-101:cdr (srfi-101:car (srfi-101:cdr ls)))))
(define srfi-101:cdddr (lambda (ls) (srfi-101:cdr (srfi-101:cdr (srfi-101:cdr ls)))))
(define srfi-101:cddar (lambda (ls) (srfi-101:cdr (srfi-101:cdr (srfi-101:car ls)))))

(define srfi-101:caaaar (lambda (ls) (srfi-101:car (srfi-101:car (srfi-101:car (srfi-101:car ls))))))
(define srfi-101:caaadr (lambda (ls) (srfi-101:car (srfi-101:car (srfi-101:car (srfi-101:cdr ls))))))
(define srfi-101:caaddr (lambda (ls) (srfi-101:car (srfi-101:car (srfi-101:cdr (srfi-101:cdr ls))))))
(define srfi-101:caadar (lambda (ls) (srfi-101:car (srfi-101:car (srfi-101:cdr (srfi-101:car ls))))))
(define srfi-101:cadaar (lambda (ls) (srfi-101:car (srfi-101:cdr (srfi-101:car (srfi-101:car ls))))))
(define srfi-101:cadadr (lambda (ls) (srfi-101:car (srfi-101:cdr (srfi-101:car (srfi-101:cdr ls))))))
(define srfi-101:cadddr (lambda (ls) (srfi-101:car (srfi-101:cdr (srfi-101:cdr (srfi-101:cdr ls))))))
(define srfi-101:caddar (lambda (ls) (srfi-101:car (srfi-101:cdr (srfi-101:cdr (srfi-101:car ls))))))
(define srfi-101:cdaaar (lambda (ls) (srfi-101:cdr (srfi-101:car (srfi-101:car (srfi-101:car ls))))))
(define srfi-101:cdaadr (lambda (ls) (srfi-101:cdr (srfi-101:car (srfi-101:car (srfi-101:cdr ls))))))
(define srfi-101:cdaddr (lambda (ls) (srfi-101:cdr (srfi-101:car (srfi-101:cdr (srfi-101:cdr ls))))))
(define srfi-101:cdadar (lambda (ls) (srfi-101:cdr (srfi-101:car (srfi-101:cdr (srfi-101:car ls))))))
(define srfi-101:cddaar (lambda (ls) (srfi-101:cdr (srfi-101:cdr (srfi-101:car (srfi-101:car ls))))))
(define srfi-101:cddadr (lambda (ls) (srfi-101:cdr (srfi-101:cdr (srfi-101:car (srfi-101:cdr ls))))))
(define srfi-101:cddddr (lambda (ls) (srfi-101:cdr (srfi-101:cdr (srfi-101:cdr (srfi-101:cdr ls))))))
(define srfi-101:cdddar (lambda (ls) (srfi-101:cdr (srfi-101:cdr (srfi-101:cdr (srfi-101:car ls))))))

;; [RaList X] -> Nat
(define (srfi-101:length ls)
  (cl:assert (srfi-101:list? ls))
  (let recr ((ls ls))
    (if (kons? ls)
        (+ (kons-size ls) (recr (kons-rest ls)))
        0)))

(define (make-foldl empty? first rest)
  (cl:declare (cl:optimize (cl:debug 1))
              (cl:function empty? first rest))
  (cl:labels ((f (cons empty ls)
                (if (funcall empty? ls) 
                    empty
                    (f cons
                      (funcall cons (funcall first ls) empty) 
                      (funcall rest ls)))))
    #'f))

(define (make-foldr empty? first rest)
  (cl:declare (cl:optimize (cl:debug 1))
              (cl:function empty? first rest))
  (cl:labels ((f (cons empty ls)
                (if (funcall empty? ls) 
                    empty
                    (funcall cons (funcall first ls)
                             (f cons empty (funcall rest ls))))))
    #'f))

;; [X Y -> Y] Y [RaListof X] -> Y
(define srfi-101::foldl/1
  (make-foldl #'srfi-101:null? #'srfi-101:car #'srfi-101:cdr))

(define srfi-101::foldr/1
  (make-foldr #'srfi-101:null? #'srfi-101:car #'srfi-101:cdr))

;; [RaListof X] ... -> [RaListof X]
(define (srfi-101:append . lss)
  (cond ((null? lss) srfi-101::null)
        (else (let recr ((lss lss))
                (cond ((null? (cdr lss)) (car lss))
                      (else (srfi-101::foldr/1 #'srfi-101:cons
                                               (recr (cdr lss))
                                               (car lss))))))))

;; [RaListof X] -> [RaListof X]
(define (srfi-101:reverse ls)
  (cl:declare (cl:optimize (cl:debug 1)))  
  (srfi-101::foldl/1 #'srfi-101:cons srfi-101::null ls)
  #|(let ((empty? #'srfi-101:null?)
        (first #'srfi-101:car)
        (rest #'srfi-101:cdr))
    (cl:labels ((f (cons empty ls)
                  (if (funcall empty? ls) 
                      empty
                      (f cons
                        (funcall cons (funcall first ls) empty) 
                        (funcall rest ls)))))
      (f #'srfi-101:cons srfi-101::null ls)))|#)

;; [RaListof X] Nat -> [RaListof X]
(define (srfi-101:list-tail ls i)
  (let loop ((xs ls) (j i))
    (cond ((zero? j) xs)
          (else (loop (srfi-101:cdr xs) (sub1 j))))))

;; [RaListof X] Nat -> X
;; Special-cased above to avoid logarathmic amount of cons'ing
;; and any multi-values overhead.  Operates in constant space.
(define (srfi-101:list-ref ls i)
  ;(assert (< i (srfi-101:length ls)))
  (let loop ((xs ls) (j i))
    (if (< j (kons-size xs))
        (tree-ref (kons-size xs) (kons-tree xs) j)
        (loop (kons-rest xs) (- j (kons-size xs))))))

;; [RaListof X] Nat X -> [RaListof X]
(define (srfi-101:list-set ls i v)
  (let-values (((_ l*) (srfi-101::list-ref/set ls i v)))
    _                                   ;dummy
    l*))

;; [X ... -> y] [RaListof X] ... -> [RaListof Y]
;; Takes advantage of the fact that map produces a list of equal size.
(define srfi-101:map
  (case-lambda 
   ((f ls)
    (let recr ((ls ls))
      (if (kons? ls)
          (make-kons (kons-size ls) 
                     (tree-map f (kons-tree ls)) 
                     (recr (kons-rest ls)))
          srfi-101::null)))
   ((f . lss)
    ;(check-nary-loop-args 'srfi-101:map (lambda (x) x) f lss)
    (let recr ((lss lss))
      (cond ((srfi-101:null? (car lss)) srfi-101::null)
            (else
             ;; IMPROVE ME: make one pass over lss.
             (make-kons (kons-size (car lss))
                        (tree-map/n f (map #'kons-tree lss))
                        (recr (map #'kons-rest lss)))))))))


;; [X ... -> Y] [RaListof X] ... -> unspecified
(define srfi-101:for-each
  (case-lambda 
   ((f ls)
    (cl:when (kons? ls)
      (tree-for-each f (kons-tree ls))
      (srfi-101:for-each f (kons-rest ls))))
   ((f . lss)
    ;(check-nary-loop-args 'srfi-101:map (lambda (x) x) f lss)
    (let recr ((lss lss))
      (cl:when (srfi-101:pair? (car lss))
        (tree-map/n f (map #'kons-tree lss))
        (recr (map #'kons-rest lss)))))))

;; [RaListof X] -> [Listof X]
(define (srfi-101:random-access-list->linear-access-list x)
  (srfi-101::foldr/1 #'cons '() x))

;; [Listof X] -> [RaListof X]
(define (srfi-101:linear-access-list->random-access-list x)
  (srfi-1:fold-right #'srfi-101:cons '() x))

;; This code based on code written by Abdulaziz Ghuloum
;; http://ikarus-scheme.org/pipermail/ikarus-users/2009-September/000595.html
(define get-cached
  (let ((h (cl:make-hash-table :test #'cl:eq)))
    (lambda (x)
      (cl:labels ((f (x)
                    (cond
                      ((pair? x) (srfi-101:cons (f (car x)) (f (cdr x))))
                      ((vector? x) (cl:map 'cl:vector #'f x))
                      (else x))))
         (cond
          ((not (or (pair? x) (vector? x))) x)
          ((cl:gethash x h))
          (else
           (let ((v (f x)))
             (cl:setf (cl:gethash x h) v)
             v)))))))


(define-syntax srfi-101:quote
  (syntax-rules ()
    ((srfi-101:quote datum) (get-cached 'datum)))) 


;;; *EOF*
