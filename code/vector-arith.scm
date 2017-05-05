;;; Vincent Anioke 


;;; Vector Arithmetic


(define (vector-element-wise element-procedure)
  (lambda vecs 
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

(define (ensure-vector-lengths-match vecs)
  (let ((first-vec-length (vector-length (car vecs))))
    (if (any (lambda (v)
	       (not (n:= (vector-length v)
			 first-vec-length)))
	     vecs)
	(error "Vector dimension mistmatch:" vecs))))


(define (ext-vector? object)
  (if (and (vector? object)
	   (every (lambda (v)
		 (or (symbolic? v) (number? v))) (vector->list object)))
      #t
      #f))
(register-predicate! ext-vector? 'ext-vector)

(define (ext-vector-or-num? object) ;;;to deal with scalar multiplication
  (if (or (ext-vector? object) (number? object) (symbolic? object)) #t #f))
(register-predicate! ext-vector-or-num? 'ext-vector-or-num)

(define (add-vect v1 v2)
  (define add-op (vector-element-wise +)) 
  (ensure-vector-lengths-match (list v1 v2))
  (add-op v1 v2))

(define (sub-vect v1 v2)
  (define sub-op (vector-element-wise -))
  (ensure-vector-lengths-match (list v1 v2))
  (sub-op v1 v2))

(define (dot-product v1 v2)
  (define mult-op (vector-element-wise *))
  (ensure-vector-lengths-match (list v1 v2))
  (define dot-vector (mult-op v1 v2))
  (define sum-over-vector (lambda (v)
			    (if (= 0 (vector-length v)) 0
				(+ (vector-first v) (sum-over-vector (vector-tail v 1))))))
  (sum-over-vector dot-vector))

(define (scalar-product v1 v2) ;;; first arg vector; second-arg scalar
  (define multiply-by-scalar (lambda (c) (* c v2)))
  (define apply-scalar-product (vector-element-wise multiply-by-scalar))
  (apply-scalar-product v1))
			

(define (mult-vect v1 v2)
  (cond ((and (ext-vector? v1) (ext-vector? v2)) (dot-product v1 v2))
	((and (ext-vector? v1) (or (symbolic? v2)(number? v2))) (scalar-product v1 v2))
	((and (or (symbolic? v1)(number? v1)) (ext-vector? v2)) (scalar-product v2 v1)))) ;;; no need for else since annotation mechanism ensures we're dealing with vectors and/or numberss/sybmolics.

(define (negate-vect v1)
  (define neg-op (vector-element-wise -))
  (neg-op v1)) ;;; alternative definition would be to create a same-size 0-length vector v and do (sub-vect v v1)


(define (vector-extender base-arithmetic)
  (make-arithmetic 'ext-vector ext-vector? (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (let ((base-predicate
           (arithmetic-domain-predicate base-arithmetic)))
      (lambda (operator base-operation)
	(let ((procedure 
	       (case operator
		 ((+) (lambda (a b)
			(add-vect a b)))
		 ((-) (lambda (a b)
			(sub-vect a b)))
		 ((*) (lambda (a b)
			(mult-vect a b)))
		 ((negate) (lambda (a)
			     (negate-vect a)))
		 ((magnitude) (lambda (a) (sqrt (dot-product a a))))
		 (else (lambda args
			 (error "Operator undefined in Vector Arithmetic" operator))))))
	  (and procedure
	       (make-operation operator
			       (all-args (operator-arity operator) 
					 ext-vector-or-num?)
			       procedure)))))))

(define combined-arithmetic
  (extend-arithmetic symbolic-extender
                     numeric-arithmetic))


(define vector-arithmetic
  (extend-arithmetic vector-extender combined-arithmetic))





