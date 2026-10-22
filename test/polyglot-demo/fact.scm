(define (kawa-fact n) (if (< n 2) 1 (* n (kawa-fact (- n 1)))))
