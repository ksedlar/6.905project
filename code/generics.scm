(define (generic-dispatcher)
  (simple-generic-dispatcher))

;(define (generic-dispatcher)
;  (cached-generic-dispatcher implementation-type-name))
;(define (cached-generic-dispatcher get-key)
;  (make-cached-generic-dispatcher (simple-generic-dispatcher)
;                                  get-key))
;(define (make-cached-generic-dispatcher base-dispatcher get-key)
;  (let ((get-handler
;         (simple-list-memoizer eqv?
;                               hash-by-eqv
;                               (lambda (args) (map get-key args))
;                               (base-dispatcher 'get-handler))))
;    (lambda (message)
;      (case message
;        ((get-handler) get-handler)
;        (else (base-dispatcher message))))))