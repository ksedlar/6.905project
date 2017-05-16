;;; Matrices Arithmetic

(define (matrix-element-wise element-procedure)
	(lambda mats
		(ensure-matrix-dimensions-match mats)
		(apply vector-map element-procedure mats)))


(define (ensure-matrix-dimensions-match mats)
	;;; currently assuming uniform matrices
	(define row-size (vector-length (car mats)))
	(define column-size (vector-length (vector-first (car mats))))
	(if (any (lambda (m)
		(not (and (n:= (vector-length m) row-size)
			  (n:= (vector-length (vector-first m)) column-size))))
		mats)
	(error "Matrix dimension mismatch:" mats)))

(define (ensure-matrix-multiplication m1 m2)
	(if (not (n:= (vector-length (vector-first m1)) (vector-length m2)))
	(error "Matrix Multiplication Dimension Mismatch:" (list m1 m2))))

(define (rectangular? object)
	(define row-size (vector-length (vector-first object)))
	(if (every (lambda (v) (= row-size (vector-length v))) (vector->list object))
	#t
	#f))

(define (matrix? object)
	(if (and (vector? object) (rectangular? object)
		(every (lambda (v)
			(ext-vector? v)) (vector->list object)))
	#t
	#f))
(register-predicate! matrix? 'matrix) 

(define (matrix-or-scalar? object) ;;; to deal with matrix scalar multiplications
	(if (or (matrix? object) (number? object) (symbolic? object)) #t #f))
(register-predicate! matrix-or-scalar? 'matrix-or-scalar)

(define (add-mats m1 m2)
	(define add-op (matrix-element-wise +))
	(ensure-matrix-dimensions-match (list m1 m2))
	(add-op m1 m2))

(define (sub-mats m1 m2)
	(define sub-op (matrix-element-wise -))
	(ensure-matrix-dimensions-match (list m1 m2))
	(sub-op m1 m2))


;;; can edit transpose m later so that the transpose of a tranposed
;;; matrix is not a pair but a single vector of vectors
(define (transpose m)
	(define list_of_vectors (vector->list m))
	(define list_of_lists (map vector->list list_of_vectors))
	(define transposed_list (apply map list list_of_lists))
	(define trans_list_of_vectors (map list->vector transposed_list))
	(cons (list->vector trans_list_of_vectors) 'transposed))
	;;;(list->vector trans_list_of_vectors))


;;; consider functionality separation by creating list-of-list and vector-of-vector
;;; conversion procedures to use in transpose and mult-mats
(define (mult-mats m1 m2)
	(define alt_m2 (car (transpose m2)))
	(ensure-matrix-multiplication m1 m2)
	(define (mult-vecs v1)
		(lambda (arg)
			(* v1 arg)))
	(define (produce_single_list)
		(lambda (arg)
			(map (mult-vecs arg) (vector->list alt_m2))))
	(define list_of_lists (map (produce_single_list) (vector->list m1)))
	(define list_of_vectors (map list->vector list_of_lists))
	(list->vector list_of_vectors))


(define (mult-by-scalar mat scalar)
	(define multiply-by-scalar (lambda (v) (scalar-product v scalar)))
	(define apply-scalar-product (matrix-element-wise multiply-by-scalar))
	(apply-scalar-product mat))

(define (multiply-mats m1 m2)
	(cond ((and (matrix? m1) (matrix? m2)) (mult-mats m1 m2))
		((and (matrix? m1) (or (symbolic? m2) (number? m2))) (mult-by-scalar m1 m2))
		((and (or (symbolic? m1) (number? m1)) (matrix? m2)) (mult-by-scalar m2 m1))))

(define (negate-mat m1)
	(define neg-op (matrix-element-wise negate-vect))
	(neg-op m1))

(define (square-matrix? m1)
  (and matrix? (= (vector-length m1) (vector-length (vector-first m1)))))

;(define (det-2by2-mats m1)
;  (let ((a (vector-first (vector-first m1)))
;        (b (vector-first (vector-second m1)))
;        (c (vector-second (vector-first m1)))
;        (d (vector-second (vector-second m1))))
;    (- (* a d) (* b c))))

(define (vec-remove-nth v1 n)
  (let ((v3 (vector-head v1 n))
        (v4 (vector-tail v1 (+ n 1))))
    (define v5 (vector-grow v3 (- (vector-length v1) 1)))
    (subvector-move-left! v4 0 (vector-length v4) v5 (vector-length v3))
    v5))

(define (submats m1 row column)
  (vector-map (lambda (v1) (vec-remove-nth v1 column)) (vec-remove-nth m1 row)))

; Also flips to the right parity
(define (determinant-submats m1 row column)
  (define parity
    (if (even? (+ row column))
        1
        -1))
  (* parity (determinant-mats (submats m1 row column))))

(define (laplace-term m1 n)
;  (define coef
;    (if (even? n)
;        (vector-ref (vector-first m1) n)
;        (- (vector-ref (vector-first m1) n))))
;  (define submats
;    (vector-map (lambda (v1) (vec-remove-nth v1 n)) (vector-tail m1 1)))
;  (* coef (determinant-mats submats)))
  (* (vector-ref (vector-first m1) n) (determinant-submats m1 0 n)))

(define (large-mats-helper m1 n sum)
  (if (<= (vector-length m1) n)
      sum
      (large-mats-helper m1 (+ n 1) (+ sum (laplace-term m1 n)))))

(define (det-large-mats m1)
  (large-mats-helper m1 0 0))

(define (determinant-mats m1)
  (if (square-matrix? m1)
      (cond ((= (vector-length m1) 1) (vector-first (vector-first m1)))
;            ((= (vector-length m1) 2) (det-2by2-mats m1))
            (else (det-large-mats m1)))
      (error "Not a square matrix:" m1)))

(define (copy-mats m1)
  (vector-map vector-copy m1))

(define (cofactor-helper m1 m2 row column)
  (vector-set! (vector-ref m2 row)
               column
               (determinant-submats m1 row column))
  (cond ((and (< row (vector-length m1))
              (< (+ column 1) (vector-length (vector-first m1))))
         (cofactor-helper m1 m2 row (+ column 1)))
        ((and (< (+ row 1) (vector-length m1))
              (= (+ column 1) (vector-length (vector-first m1))))
         (cofactor-helper m1 m2 (+ row 1) 0))
        ((and (= (+ row 1) (vector-length m1))
              (= (+ column 1) (vector-length (vector-first m1))))
         m2)
        (else (error "cofactor helper out of bounds"))))
         
(define (cofactor m1)
  (cofactor-helper m1 (copy-mats m1) 0 0))

(define (inverse-large-mats m1)
  (* (/ 1 (determinant-mats m1)) (car (transpose (cofactor m1)))))

(define (inverse-mats m1)
  (let ((det (determinant-mats m1)))
    (if (= 0 det)
        (error "Determinant is 0:" m1)
        (if (= (vector-length m1) 1)
             (/ 1 det)
             (inverse-large-mats m1)))))
  

(define (matrix-extender base-arithmetic)
	(make-arithmetic 'matrix matrix? (list base-arithmetic)
	(lambda (name base-constant)
		base-constant)
	(let ((base-predicate
		(arithmetic-domain-predicate base-arithmetic)))
		(lambda (operator base-operation)
	(let ((procedure
		(case operator
		((+) (lambda (a b)
			(add-mats a b)))
		((-) (lambda (a b)
			(sub-mats a b)))
		((*) (lambda (a b)
			(multiply-mats a b)))
		((negate) (lambda (a)
			(negate-mat a)))
		(else (lambda args
			(error "Operator undefined in Matrix Arithmetic" operator))))))
		(and procedure
			(make-operation operator
				(all-args (operator-arity operator)
					matrix-or-scalar?)
				procedure)))))))


;;; combined-arithmetic is defined in vector-arith.scm
(define matrix-arithmetic
	(extend-arithmetic matrix-extender vector-arithmetic))



