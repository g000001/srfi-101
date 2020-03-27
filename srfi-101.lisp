(cl:in-package "https://github.com/g000001/srfi-101#internals")

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
                       car cdr))

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
       (if (and (rnrs:pair? ts)
                (node? (rnrs:car ts)))
           (make-node (apply f (rnrs:map #'node-val ts))
                      (recr (rnrs:map #'node-left ts))
                      (recr (rnrs:map #'node-right ts)))
           (apply f ts))))
  
;; [X Y Z ... -> R] [List [Tree X] [Tree Y] [Tree Z] ...] -> unspecified
(define (tree-for-each/n f ts)
  (let recr ((ts ts))
      (if (and (rnrs:pair? ts)
               (node? (rnrs:car ts)))
          (begin (apply f (rnrs:map #'node-val ts))
                 (recr (rnrs:map #'node-left ts))
                 (recr (rnrs:map #'node-right ts)))
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
(cl:defconstant null (rnrs:quote ()))

;; [Any -> Boolean]
(define pair? #'kons?)

;; [Any -> Boolean]
(define null? #'rnrs:null?)

;; X [RaListof X] -> [RaListof X]  /\
;; X Y -> [RaPair X Y]
(define (cons x ls)
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
(define car+cdr 
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
(define (car p)
  (call-with-values (lambda () (car+cdr p))
                    (lambda (car cdr) (cl:declare (cl:ignore cdr)) car)))

;; [RaPair X Y] -> Y
(define (cdr p)
  (call-with-values (lambda () (car+cdr p))
                    (lambda (car cdr) (cl:declare (cl:ignore car)) cdr)))

;; [RaListof X] Nat [X -> X] -> X [RaListof X]
(define (list-ref/update ls i f)
                                        ;(assert (< i (length ls)))
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
(define (list-update ls i f)
  ;(assert (< i (length ls)))
  (let recr ((xs ls) (j i))
    (let ((s (kons-size xs)))
      (if (< j s) 
          (make-kons s (tree-update s (kons-tree xs) j f) (kons-rest xs))
          (make-kons s (kons-tree xs) (recr (kons-rest xs) (- j s)))))))

;; [RaListof X] Nat X -> (values X [RaListof X])
(define (list-ref/set ls i v)
  (list-ref/update ls i (lambda (_) (cl:declare (cl:ignore _)) v)))

;; X ... -> [RaListof X]
(define (list . xs)
  (srfi-1:fold-right #'cons null xs))

;; Nat X -> [RaListof X]
(define make-list
  (case-lambda
   ((k) (make-list k 0))
   ((k obj)
    (let loop ((n k) (a null))
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
(define (list? x)
  (or (null? x)
      (and (kons? x)
           (list? (kons-rest x)))))

(define caar (lambda (ls) (car (car ls))))
(define cadr (lambda (ls) (car (cdr ls))))
(define cddr (lambda (ls) (cdr (cdr ls))))
(define cdar (lambda (ls) (cdr (car ls))))

(define caaar (lambda (ls) (car (car (car ls)))))
(define caadr (lambda (ls) (car (car (cdr ls)))))
(define caddr (lambda (ls) (car (cdr (cdr ls)))))
(define cadar (lambda (ls) (car (cdr (car ls)))))
(define cdaar (lambda (ls) (cdr (car (car ls)))))
(define cdadr (lambda (ls) (cdr (car (cdr ls)))))
(define cdddr (lambda (ls) (cdr (cdr (cdr ls)))))
(define cddar (lambda (ls) (cdr (cdr (car ls)))))

(define caaaar (lambda (ls) (car (car (car (car ls))))))
(define caaadr (lambda (ls) (car (car (car (cdr ls))))))
(define caaddr (lambda (ls) (car (car (cdr (cdr ls))))))
(define caadar (lambda (ls) (car (car (cdr (car ls))))))
(define cadaar (lambda (ls) (car (cdr (car (car ls))))))
(define cadadr (lambda (ls) (car (cdr (car (cdr ls))))))
(define cadddr (lambda (ls) (car (cdr (cdr (cdr ls))))))
(define caddar (lambda (ls) (car (cdr (cdr (car ls))))))
(define cdaaar (lambda (ls) (cdr (car (car (car ls))))))
(define cdaadr (lambda (ls) (cdr (car (car (cdr ls))))))
(define cdaddr (lambda (ls) (cdr (car (cdr (cdr ls))))))
(define cdadar (lambda (ls) (cdr (car (cdr (car ls))))))
(define cddaar (lambda (ls) (cdr (cdr (car (car ls))))))
(define cddadr (lambda (ls) (cdr (cdr (car (cdr ls))))))
(define cddddr (lambda (ls) (cdr (cdr (cdr (cdr ls))))))
(define cdddar (lambda (ls) (cdr (cdr (cdr (car ls))))))

;; [RaList X] -> Nat
(define (length ls)
  (cl:assert (list? ls))
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
(define foldl/1
  (make-foldl #'null? #'car #'cdr))

(define foldr/1
  (make-foldr #'null? #'car #'cdr))

;; [RaListof X] ... -> [RaListof X]
(define (append . lss)
  (cond ((rnrs:null? lss) null)
        (else (let recr ((lss lss))
                (cond ((rnrs:null? (rnrs:cdr lss)) (rnrs:car lss))
                      (else (foldr/1 #'cons
                                               (recr (rnrs:cdr lss))
                                               (rnrs:car lss))))))))

;; [RaListof X] -> [RaListof X]
(define (reverse ls)
  (cl:declare (cl:optimize (cl:debug 1)))  
  (foldl/1 #'cons null ls)
  #|(let ((empty? #'null?)
        (first #'car)
        (rest #'cdr))
    (cl:labels ((f (cons empty ls)
                  (if (funcall empty? ls) 
                      empty
                      (f cons
                        (funcall cons (funcall first ls) empty) 
                        (funcall rest ls)))))
      (f #'cons null ls)))|#)

;; [RaListof X] Nat -> [RaListof X]
(define (list-tail ls i)
  (let loop ((xs ls) (j i))
    (cond ((zero? j) xs)
          (else (loop (cdr xs) (sub1 j))))))

;; [RaListof X] Nat -> X
;; Special-cased above to avoid logarathmic amount of cons'ing
;; and any multi-values overhead.  Operates in constant space.
(define (list-ref ls i)
  ;(assert (< i (length ls)))
  (let loop ((xs ls) (j i))
    (if (< j (kons-size xs))
        (tree-ref (kons-size xs) (kons-tree xs) j)
        (loop (kons-rest xs) (- j (kons-size xs))))))

;; [RaListof X] Nat X -> [RaListof X]
(define (list-set ls i v)
  (let-values (((_ l*) (list-ref/set ls i v)))
    _                                   ;dummy
    l*))

;; [X ... -> y] [RaListof X] ... -> [RaListof Y]
;; Takes advantage of the fact that map produces a list of equal size.
(define map
  (case-lambda 
   ((f ls)
    (let recr ((ls ls))
      (if (kons? ls)
          (make-kons (kons-size ls) 
                     (tree-map f (kons-tree ls)) 
                     (recr (kons-rest ls)))
          null)))
   ((f . lss)
    ;(check-nary-loop-args 'map (lambda (x) x) f lss)
    (let recr ((lss lss))
      (cond ((null? (rnrs:car lss)) null)
            (else
             ;; IMPROVE ME: make one pass over lss.
             (make-kons (kons-size (rnrs:car lss))
                        (tree-map/n f (rnrs:map #'kons-tree lss))
                        (recr (rnrs:map #'kons-rest lss)))))))))


;; [X ... -> Y] [RaListof X] ... -> unspecified
(define for-each
  (case-lambda 
   ((f ls)
    (cl:when (kons? ls)
      (tree-for-each f (kons-tree ls))
      (for-each f (kons-rest ls))))
   ((f . lss)
    ;(check-nary-loop-args 'map (lambda (x) x) f lss)
    (let recr ((lss lss))
      (cl:when (pair? (rnrs:car lss))
        (tree-map/n f (rnrs:map #'kons-tree lss))
        (recr (rnrs:map #'kons-rest lss)))))))

;; [RaListof X] -> [Listof X]
(define (random-access-list->linear-access-list x)
  (foldr/1 #'rnrs:cons '() x))

;; [Listof X] -> [RaListof X]
(define (linear-access-list->random-access-list x)
  (srfi-1:fold-right #'cons '() x))

;; This code based on code written by Abdulaziz Ghuloum
;; http://ikarus-scheme.org/pipermail/ikarus-users/2009-September/000595.html
(define get-cached
  (let ((h (cl:make-hash-table :test #'cl:eq)))
    (lambda (x)
      (cl:labels ((f (x)
                    (cond
                      ((rnrs:pair? x) (cons (f (rnrs:car x)) (f (rnrs:cdr x))))
                      ((vector? x) (cl:map 'cl:vector #'f x))
                      (else x))))
         (cond
          ((not (or (rnrs:pair? x) (vector? x))) x)
          ((cl:gethash x h))
          (else
           (let ((v (f x)))
             (cl:setf (cl:gethash x h) v)
             v)))))))


(define-syntax quote
  (syntax-rules ()
    ((quote datum) (get-cached 'datum)))) 


;;; *EOF*
