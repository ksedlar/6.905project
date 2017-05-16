;;; Using Matrices to Solve the 25 Lights Puzzle
;;; Complete Problem Description in Project Report

;;; Summary: 25 lights puzzle involves 25 rooms arranged in a 5 by 5 grid.
;;; Switching the light in a room toggles on/off the light in that room and 
;;; adjacent rooms (above, below, left, right). The goal is, assuming all lights
;;; are initially off, to figure out which sequence of room light flip switches turn them all on.
;;; For efficiency, we require each room switch to be flipped at most once.
;;; The following code formalizes and solves the puzzle.
;;; A description and justification of the procedures that follow is included in the report.  

;;; useful in extracting the last row elements
;;; when we perform reduced row echelon form
(define (last-row-elements mat)
  (define size (vector-length (vector-first mat)))
  (define mat-list (vector->list mat))
  (define (obtain-last)
    (lambda (vec)
      (vector-ref vec (- size 1))))
  (map (obtain-last) mat-list))


;;; assume index is valid.
;;; given a list of matrices and an index, outputs a vector
;;; corresponding to the concatenation of vectors at the index of each matrix.
(define (vector-append-index matrices-list index)
  (define (index-ref matrix)
    (vector-ref matrix index))
  (apply vector-append (map index-ref matrices-list)))


;;; assembler procedure builds a single matrix from a combination of matrices
;;; single-list-assembler assumes that each matrix in matrices-list has the 
;;; same number of rows. Otherwise, doesn't really make sense to assemble.
(define (single-list-assembler matrices-list)
  (define size (vector-length (car matrices-list)))
  (define (assemble current_index)
    (if (= size current_index)
	(make-vector 0)
	(vector-append (vector (vector-append-index matrices-list current_index))
		       (assemble (+ 1 current_index)))))
  (assemble 0))

;;; whereas single-list-assembler worked with a single list of matrices,
;;; multi-list-assembler works with a list of sublists of matrices.
(define (multi-list-assembler matrices-list)
  (define intermediate-result (map single-list-assembler matrices-list))
  (apply vector-append intermediate-result))

;;; Useful submatrices. See report for guidance.
;;; To summarize, the following 3 submatrices represent
;;; recurring elements of the 25-by-25 matrix that formalizes the puzzle.
(define a (vector (vector 1 1 0 0 0)
		  (vector 1 1 1 0 0)
		  (vector 0 1 1 1 0)
		  (vector 0 0 1 1 1)
		  (vector 0 0 0 1 1)))

(define i (vector (vector 1 0 0 0 0)
		     (vector 0 1 0 0 0)
		     (vector 0 0 1 0 0)
		     (vector 0 0 0 1 0)
		     (vector 0 0 0 0 1)))

(define z (vector (vector 0 0 0 0 0)
		       (vector 0 0 0 0 0)
		       (vector 0 0 0 0 0)
		       (vector 0 0 0 0 0)
		       (vector 0 0 0 0 0)))


;;; we can now painlessly construct our crucial 25 by 25 matrix
(define big-lights-matrix (multi-list-assembler (list (list a i z z z)
						      (list i a i z z)
						      (list z i a i z)
						      (list z z i a i)
						      (list z z z i a)))) 

(define s-final-row (vector (vector 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

;;; corresponds to the wanted final state of the 25 rooms.
(define s-final (car (transpose s-final-row)))

;;; performing a reduced row echelon form operation on this matrix yields a solution.
(define augmented-matrix (single-list-assembler (list big-lights-matrix s-final)))

;;; given a list-of-lists matrix with a nonzero first element, ensures first row has a leading 1
;;; and other rows a leading 0. Specifically in mod 2.
(define (mod-reduction mat)
  (define row-size (length mat))
  (define (first-row-result)
    (define (mod2)
      (lambda (arg)
	(modulo arg 2)))
    (define intermediate (map (mod2) (car mat)))
    (if (= 0 (car intermediate))
	(map (lambda (arg) (modulo (+ 1 arg) 2)) intermediate)
	intermediate))
  (define (reduce-by-pivot-row)
    (lambda (row)
      (define unmodded-result 
	(- (list->vector row) (* (car row) (list->vector (first-row-result)))))
      (define unmodded-list (vector->list unmodded-result))
      (if (= 0 (car row))
	  row
	  (map (lambda (arg) (modulo arg 2)) unmodded-list))))
  (append (list (first-row-result)) (map (reduce-by-pivot-row) (sublist mat 1 (length mat)))))


;;; rrefmod2 takes our augmented matrix and performs reduced row echelon form modulo 2. The resulting matrix
;;; solves the 25 lights puzzle.
(define (rrefmod2 puzzle-matrix) 
  ;;; we move to the top any rows that don't start with 0
  (if (or (null? puzzle-matrix) (null? (car puzzle-matrix))) puzzle-matrix
  (let ((matrix-state (call-with-current-continuation (lambda (cc) 
    (let df ((sub-matrix puzzle-matrix)) (if (null? sub-matrix) (cc puzzle-matrix) (if (zero? (caar sub-matrix)) (let ((z (df (cdr sub-matrix))))
       (if (null? z) sub-matrix (cons (car z) (cons (car sub-matrix)(cdr z))))) sub-matrix)))))))
  ;;; using mod-reduction, we recursively ensure submatrices with non-zero
  ;;; first elements are appropriately reduced.
  (if (zero? (caar matrix-state)) (map (lambda (puzzle-matrix) (cons 0 puzzle-matrix)) (rrefmod2 (map cdr matrix-state))) (let* (
      (modded-matrix (mod-reduction matrix-state))
      (modded-submatrix (rrefmod2 (map cdr (cdr modded-matrix))))
  ;;; ensure a zero in row (cdar modded-matrix) is above
  ;;; each initial 1 of a row of modded-submatrix
      (d (let w ((sm (cdar modded-matrix))(i modded-submatrix))
        (if (null? i) sm (w (let sr ((puzzle-matrix sm) (sub-matrix (car i)))
          (if (null? puzzle-matrix) '() (if (zero? (car sub-matrix)) (cons (car puzzle-matrix) (sr (cdr puzzle-matrix)(cdr sub-matrix)))
        (map (lambda (p q)(modulo (- p (* (car puzzle-matrix) q)) 2)) puzzle-matrix sub-matrix)))) (cdr i))))))
   (cons (cons 1 d) (map (lambda (puzzle-matrix) (cons 0 puzzle-matrix)) modded-submatrix)))))))

;;; Solving the Puzzle
(define row-reduced-result (rrefmod2 (map vector->list (vector->list augmented-matrix))))

;;; by inspection, row-reduced-result is a matrix that contains 2 free variables
;;; (Recall that we have 25 variables, one per light). The free variables correspond
;;; to lights 24 and 25, and by assuming that these lights may remain switched off,
;;; the last row elements of row-reduced-result immediately tell us which lights should be switched on.

(define vector-row-reduced-result (list->vector (map list->vector row-reduced-result)))
(define solution-to-puzzle (last-row-elements vector-row-reduced-result))

;;; Running the code yields the following value for solution-to-puzzle:
;;; (0 1 1 0 1 0 1 1 1 0 0 0 1 1 1 1 1 0 1 1 1 1 0 0 0),
;;; telling us to switch on the lights in rooms 2,3,5,7,8,9,13,14,15,16,17,19,20,21 and 22.
;;; Indeed this represents a valid solution to the 25 lights puzzle. 













