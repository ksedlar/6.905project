; 6.905 Term Project
; Katie Sedlar, Vincent Anioke

(load "code/load.scm")
(load "code/matrices-arith.scm")

; We are implementing matrix arithmetic.

; For now, matrices are represented as a column of rows,
; e.g. (matrix (vector (vector 1 2 3) (vector 4 5 6)) is
; | 1 2 3 |
; | 4 5 6 |

(install-arithmetic! matrix-arithmetic)

; Test Cases

; tests on what qualify as matrices

(matrix? (vector (vector 1 2) (vector 2 4)))
; Should produce #t
;Value: #t

(matrix? (vector (vector 1)))
; Should produce #t
;Value: #t

(matrix? (vector (vector 1 2) (vector 2 3 4)))
; Should produce #f
;Value: #f 

(matrix? (vector 3 4))
; Should produce #f
;Value: #f

; tests on addition

(+ (vector (vector 1 2) (vector 3 4))
   (vector (vector 7 8) (vector 9 10)))
; Should produce (vector (vector 8 10) (vector 12 14))
;Value 34: #(#(8 10) #(12 14))  

(+ (vector (vector -1 4 7) (vector 6 15 -9))
   (vector (vector 50 8 42) (vector 19 0 20)))
; Should produce (vector (vector 49 12 49) (vector 25 15 11))
;Value 5: #(#(49 12 49) #(25 15 11)) 

(+ (vector (vector 1 2)) (vector (vector 3 4)))
; Should produce (vector (vector 4 6))
;Value 10: #(#(4 6))

(+ (vector (vector 1 2) (vector 3 4)) (vector (vector 1 2)))
; Should produce some sort of error with matrices not being the same dimension
;Matrix dimension mismatch: (#(... ...) #(...)) 

(+ (vector (vector 'a 'b) (vector 3 4)) (vector (vector 1 2) (vector 5 6)))
; Should produce (vector (vector (+ a 1) (+ b 2)) (vector 8 10))
;Value 12: #(#((+ a 1) (+ b 2)) #(8 10)) 

; tests on subtraction

(- (vector (vector 1 2 3) (vector 4 5 6) (vector 7 8 9))
   (vector (vector 9 8 7) (vector -1 0 6) (vector 3 2 1)))
; Should produce ((vector (vector -8 -6 -4) (vector 5 5 0) (vector 4 6 8)))
;Value 36: #(#(-8 -6 -4) #(5 5 0) #(4 6 8))

(- (vector (vector 1 1) (vector 1 1)) (vector (vector 1 1) (vector 1 1)))
; Should produce ((vector (vector 0 0) (vector 0 0)))
;Value 37: #(#(0 0) #(0 0))

(- (vector (vector 1 2) (vector 3 4)) (vector (vector 1 2)))
; Should produce some sort of error with matrices not being the same dimension
;Matrix dimension mismatch: (#(... ...) #(...))  

(- (vector (vector 1 2) (vector 3 4)))
; Should produce (vector (vector -1 -2) (vector -3 -4))
;Value 4: #(#(-1 -2) #(-3 -4))

(- (vector (vector 5 'b) (vector 3 'a)) (vector (vector 1 2) (vector 'c 'd)))
; Should produce (vector (vector 4 (- 'b 2)) (vector (- 3 'c) (- 'a 'd)))
;Value 13: #(#(4 (- b 2)) #((- 3 c) (- a d)))   

; tests on transpose

(transpose (vector (vector 1 2) (vector 3 4)))
; Should produce (transpose (vector (vector 1 3) (vector 2 4)))
; ;Value 2: (#(#(1 3) #(2 4)) . transposed) 

; tests on scalar multiplication

(* (vector (vector 1 2) (vector 3 4)) 2)
; Should produce (vector (vector 2 4) (vector 6 8))
;Value 3: #(#(2 4) #(6 8))

(* 2 (vector (vector 1 2) (vector 3 4)))
; Should produce (vector (vector 2 4) (vector 6 8))
;Value 3: #(#(2 4) #(6 8)) 

; tests on matrix-matrix multiplication

(* (vector (vector 1 2) (vector 3 4))      
	   (vector (vector 2 5) (vector 13 6)))
; Should produce (vector (vector 28 17) (vector 58 39))
;Value 9: #(#(28 17) #(58 39))  

(* (vector (vector 1 2) (vector 3 4))      
	   (vector (vector 2) (vector 1)))
; Should produce (vector (vector 4) (vector 10))
;Value 11: #(#(4) #(10))   

(* (vector (vector 1 2) (vector 3 4))      
	   (vector (vector 2) (vector 'a)))
; Should produce (vector (vector (+ 2 (* 2 'a))) (vector (+ 6 (* 4 'a))))
;Value 14: #(#((+ 2 (+ (* 2 a) 0))) #((+ 6 (+ (* 4 a) 0)))) 

; tests on subfunctions for determinant

(define v1 (vector 1 2 3 4))

(define m1 (vector (vector 1 2 3) (vector 4 5 6) (vector 7 8 9)))

(square-matrix? m1)
;Value: #t

(square-matrix? (vector v1))
;Value: #f  

(square-matrix? (vector (vector 1 2 3) (vector 4 5 6)))
;Value: #f 

(vec-remove-nth v1 2)
;Value 10: #(1 2 4)

(vec-remove-nth v1 0) 
;Value 11: #(2 3 4) 

(vec-remove-nth v1 3)
;Value 12: #(1 2 3)   

(submats m1 0 0)
;Value 3: #(#(5 6) #(8 9))  

(submats m1 2 2)
;Value 6: #(#(1 2) #(4 5))

(submats m1 1 2)
;Value 7: #(#(-1 -2) #(-7 -8))

(submats (submats m1 2 2) 1 1)
;Value 8: #(#(1)) 

(determinant-submats m1 2 2)
;Value: -3 

(laplace-term m1 0)
;Value: -3 

(laplace-term m1 1)
;Value: 12   

(laplace-term m1 2) 
;Value: -9  

(det-large-mats m1)
;Value: 0 

; tests on determinant

(determinant-mats (vector (vector 1)))
; Should produce 1
;Value: 1

(determinant-mats (vector (vector 1 2) (vector 3 4)))
; Should produce -2
;Value: -2 

(determinant-mats (vector (vector 'a 'b) (vector 3 4)))
; Should produce 4a - 3b
;Value 27: (+ (+ 0 (* a 4)) (* b -3))   

(determinant-mats (vector (vector 1 -1) (vector -1 1)))
; Should produce 0
;Value: 0

(determinant-mats (vector (vector 1 2 3) (vector 4 5 0) (vector 7 8 9)))
; Should produce -36  
;Value: -36 

(determinant-mats (vector (vector 1 2 3) (vector 4 5 6) (vector 7 8 9)))
; Should produce 0
;Value: 0 

(determinant-mats (vector (vector 1 1 0 1) (vector 1 1 1 0) 
(vector 1 0 1 1) (vector 1 1 0 0)))
; Should produce -1
;Value: -1 

(determinant-mats (vector (vector 1 2 3) (vector 4 5 6)))
; Should produce an error for not being a square matrix
;Not a square matrix: #(#(1 2 3) #(4 5 6))

; tests on subfunctions for inverse

(define m2 (copy-mats m1))
(vector-set! (vector-ref m2 0) 0 50)
; Should change the first element of m2 to 50, while
; the first element of m1 remains 1
m2
;Value 4: #(#(50 2 3) #(4 5 6) #(7 8 9)) 
m1
;Value 5: #(#(1 2 3) #(4 5 6) #(7 8 9)) 

(cofactor m1)
;Value 19: #(#(-3 6 -3) #(6 -12 6) #(-3 6 -3)) 

(cofactor m2)
;Value 20: #(#(-3 6 -3) #(6 429 -386) #(-3 -288 242)) 

; tests on matrix inverse

(inverse-mats (vector (vector 1 2) (vector 3 4)))
; Should produce (vector (vector -2 1) (vector 1.5 -0.5))
;Value 9: #(#(-2 1) #(3/2 -1/2)) 

(inverse-mats (vector (vector 1 -1) (vector -1 1)))
; Should say the matrix is not invertible          
;Determinant is 0: #(#(1 -1) #(-1 1))   

(inverse-mats (vector (vector 'a 2) (vector 3 'b)))
;Value 29: #(#((* (* 1 b) (/ 1 (+ (+ 0 (* a (* 1 b))) -6))) 
; (* -2 (/ 1 (+ (+ 0 (* a (* 1 b))) -6)))) 
; #((* -3 (/ 1 (+ (+ 0 (* a (* 1 b))) -6))) 
; (* (* 1 a) (/ 1 (+ (+ 0 (* a (* 1 b))) -6)))))

(inverse-mats (vector (vector 1 2 3) (vector 4 5 0) (vector 7 8 9)))
; Should produce (vector (vector -1.25 -.1666666 .416666666) 
; (vector 1 .333333 -0.333333) (vector .08333333 -.1666666 .08333333))
;Value 18: #(#(-5/4 -1/6 5/12) #(1 1/3 -1/3) #(1/12 -1/6 1/12)) 

(inverse-mats (vector (vector 1 2 3) (vector 4 5 6) (vector 7 8 9)))
; Should say the matrix is not invertible
;Determinant is 0: #(#(1 2 3) #(4 5 6) #(7 8 9)) 

(inverse-mats (vector (vector 1 1 0 1) (vector 1 1 1 0) (vector 1 0 1 1) (vector 1 1 0 0)))
; Should produce (vector (vector -1 -1 1 2) (vector 1 1 -1 -1) 
; (vector 0 1 0 -1) (vector 1 0 0 -1))  
;Value 14: #(#(-1 -1 1 2) #(1 1 -1 -1) #(0 1 0 -1) #(1 0 0 -1)) 

; tests on division

(define m3 (vector (vector 1 6 4) (vector 9 2 3) (vector 5 7 8)))
(define m4 (vector (vector 10 7 4) (vector 2 1 4) (vector 6 3 2)))

(/ m3 m4) 
; Should be #(#(11/4 11/20 -23/5) #(-5/4 1/4 7/2) #(9/4 29/20 -17/5))  
;Value 21: #(#(11/4 11/20 -23/5) #(-5/4 1/4 7/2) #(9/4 29/20 -17/5)) 

(/ m4 m3)
; Should be #(#(79/45 64/45 -41/45) #(-29/27 -8/27 31/27) #(19/27 22/27 -11/27)) 
;Value 24: #(#(79/45 64/45 -41/45) #(-29/27 -8/27 31/27) #(19/27 22/27 -11/27)) 

(/ m3 m1)
; Should produce an error
;Determinant is 0: #(#(1 2 3) #(4 5 6) #(7 8 9)) 

(/ m1 3)
; Should be #(#(3 6 9) #(12 15 18) #(21 24 27))  
;Value 25: #(#(3 6 9) #(12 15 18) #(21 24 27))

(define m5 (vector (vector 1 2) (vector 3 4)))
(define m6 (vector (vector 4 3) (vector 6 7)))

(/ m5 m6)
; Should be #(#(-1/2 1/2) #(-3/10 7/10))
;Value 26: #(#(-1/2 1/2) #(-3/10 7/10)) 

(/ m5 m4)
; Should produce an error
;Matrix Multiplication Dimension Mismatch: (#(... ...) #(... ... ...))  

(define m7 (vector (vector 'a 2) (vector 3 4)))

(/ m7 m6)
;Value 30: #(#((+ (* a 7/10) -6/5) (+ (* a -3/10) 4/5)) #(-3/10 7/10))

(/ m6 m7)
;Value 31: #(#((+ (* 4 (* 4 (/ 1 (+ (+ 0 (* a 4)) -6)))) 
; (+ (* 3 (* -3 (/ 1 (+ (+ 0 (* a 4)) -6)))) 0)) 
; (+ (* 4 (* -2 (/ 1 (+ (+ 0 (* a 4)) -6)))) 
; (+ (* 3 (* (* 1 a) (/ 1 (+ (+ 0 (* a 4)) -6)))) 0))) 
; #((+ (* 6 (* 4 (/ 1 (+ (+ 0 (* a 4)) -6)))) 
; (+ (* 7 (* -3 (/ 1 (+ (+ 0 (* a 4)) -6)))) 0)) 
; (+ (* 6 (* -2 (/ 1 (+ (+ 0 (* a 4)) -6)))) 
; (+ (* 7 (* (* 1 a) (/ 1 (+ (+ 0 (* a 4)) -6)))) 0))))