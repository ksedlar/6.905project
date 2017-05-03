; 6.905 Term Project
; Katie Sedlar, Vincent Anioke

(load "code/load.scm")

; We are implementing matrix arithmetic.

; For now, matrices are represented as a column of rows,
; e.g. (matrix (vector (vector 1 2 3) (vector 4 5 6)) is
; | 1 2 3 |
; | 4 5 6 |

; Test Cases
(+ (matrix (vector (vector 1 2) (vector 3 4)))
   (matrix (vector (vector 7 8) (vector 9 10))))
; Should produce (matrix (vector (vector 8 10) (vector 12 14)))

(+ (matrix (vector (vector -1 4 7) (vector 6 15 -9)))
   (matrix (vector (vector 50 8 42) (vector 19 0 20))))
; Should produce (matrix (vector (vector 49 12 49) (vector 25 15 11)))

(+ (matrix (vector (vector 1 2))) (matrix (vector (vector 3 4))))
; Should produce (matrix (vector 4 6))

(+ (matrix (vector (vector 1 2) (vector 3 4))) (matrix (vector (vector 1 2))))
; Should produce some sort of error with matrices not being the same dimension

(- (matrix (vector (vector 1 2 3) (vector 4 5 6) (vector 7 8 9)))
   (matrix (vector (vector 9 8 7) (vector -1 0 6) (vector 3 2 1))))
; Should produce (matrix (vector (vector -8 -6 -4) (vector 5 5 0) (vector 4 6 8)))

(- (matrix (vector (vector 1 1) (vector 1 1))) (matrix (vector (vector 1 1) (vector 1 1))))
; Should produce (matrix (vector (vector 0 0) (vector 0 0)))

(- (matrix (vector 1 2) (vector 3 4)) (matrix (vector (vector 1 2))))
; Should produce some sort of error with matrices not being the same dimension