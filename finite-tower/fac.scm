
(define fac
  (lambda (n)
    (if (< n 2) 1
	(fac-iter n 1))))

(define fac-iter
  (lambda (n m)
    (if (< n 2) m
	(fac-iter (- n 1) (* n m)))))


(define fib
  (lambda (n)
    (if (< n 3) 1
	(+ (fib (- n 1))
	   (fib (- n 2))))))









