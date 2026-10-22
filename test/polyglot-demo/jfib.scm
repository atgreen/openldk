(define (jfib n) (if (< n 2) n (+ (jfib (- n 1)) (jfib (- n 2)))))
