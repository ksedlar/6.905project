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

(define (matrix? object)
	(if (and (vector? object)
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
	(define multiply-by-scalar (lambda (v) (* v scalar)))
	(define apply-scalar-product (matrix-element-wise multiply-by-scalar))
	(apply-scalar-product mat))

(define (multiply-mats m1 m2)
	(cond ((and (matrix? m1) (matrix? m2)) (mult-mats m1 m2))
		((and (matrix? m1) (or (symbolic? m2) (number? m2))) (mult-by-scalar m1 m2))
		((and (or (symbolic? m1) (number? m1)) (matrix? m2)) (mult-by-scalar m2 m1))))

(define (negate-mat m1)
	(define neg-op (matrix-element-wise -))
	(neg-op m1))

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



