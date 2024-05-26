


(define fac
  (lambda (n)
    (if (< n 2)
	1
	(fac-iter n 1))))


(define fac-iter
  (lambda (n m)
    (if (< n 2)
	m
	(fac-iter (- n 1) (* n m)))))


;; ------------------------

(define (fib n)
  (if
   (= n 1)
   1
   (if (= n 2)
       1
       (+ (fib (- n 1))
	  (fib (- n 2))))))


;; ------- cond ? -------------
(define (fib2 n)
  (cond
   ((= n 1) 1)
   ((= n 2) 1)
   (#t (+ (fib (- n 1))
	  (fib (- n 2))))))

(define (fib3 n)
  (cond
   ((= n 1) 1)
   ((= n 2) 1)
   (else (+ (fib (- n 1))
	    (fib (- n 2))))))


((lambda (a) a) (begin 1))

((lambda (a) ((lambda (b) (list a b )) (begin 2))) (begin 1))

(let ((a 1))(list a a))

(let* ((a 1)(b (+ a a))(c (+ b b))) (list a b c))


(define (even-odd)
  (letrec ((even? (lambda (n)
		    (format #t "even ~a => ~%" n)
		    (cond
		     ((= n 0) #t)
		     ((< n 0) #f)
		     (else (odd? (- n 1))))))
	   (odd? (lambda (n)
		   (format #t "odd ~a => ~%" n)
		   (cond
		    ((= n 1) #t)
		    ((< n 1) #f)
		    (else (even? (- n 1)))))))
    (list 2 (even? 2)
	  3 (even? 3)
	  5 (odd? 5)
	  7 (odd? 7)
	  9 (even? 9))))


(define (even-odd2)
  (let ((even?2 #f)
	(odd?2 #f))
    (set! even?2 (lambda (n)
		   (format #t "even2 ~a => ~%" n)
		   (cond
		    ((= n 0) #t)
		    ((< n 0) #f)
		    (else (odd?2 (- n 1))))))

    (set! odd?2 (lambda (n)
		  (format #t "odd2 ~a => ~%" n)
		  (cond
		   ((= n 1) #t)
		   ((< n 1) #f)
		   (else (even?2 (- n 1))))))
    (list 2 (even?2 2)
	  3 (even?2 3)
	  5 (odd?2 5)
	  7 (odd?2 7)
	  9 (even?2 9))))


(define twice (lambda (n) (+ n n )))

(twice 5)

;; unfortunately (map twice '(1 2 3 4)) does not give '(2 4 6 8)
;; 



  






