
;; ---------------------------------
;; can we instrument when we evaluate an expression ?
;; how many times does fib call + when do (fib 20)
;; need access to + operator used in fib
;; instrumenting fib by changing the actual + operator itself
;;
;; also limited by our ability to wield multiple args - but hey , 
;; we will get there  , change  a two arg +
;;
;; can we instrument a normal procedure in a normal scheme program ? 
;; can we instrument + 
;;

;; ----------------------------------------------------
;; proof we cannot alter inbuilt + in scheme 
(define (fib n)
  (if
   (= n 1)
   1
   (if (= n 2)
       1
       (+ (fib (- n 1))
	  (fib (- n 2))))))

(define old+ +)
(define counter 0)
(set! +
  (lambda (x y)
    (display "adding ") (display x) (display " + ") (display y) (newline)
    (set! counter (+ 1 counter))
    (old+ x y)))

(begin
  (set! counter 0)
  (fib 20)
  (display "fib 20 has ")
  (display counter)
  (display " ADDITIONS")
  )
;; ------------------------------------------------
;; $1 = 6765
;; $2 = 0
;; ------------------------------------------------



;; ------------------------------------------------
;; can we alter a user defined procedure
;; proof we can instrument user defined procedure
(define counter 0)
(define myadd
    (lambda (x y)
      (set! counter (+ 1 counter))
      (+ x y)))

(define (myfib n)
  (if
   (= n 1)
   1
   (if (= n 2)
       1
       (myadd (myfib (- n 1))
	      (myfib (- n 2))))))

(begin
  (set! counter 0)
  (myfib 20)
  (display "myfib 20 has ")
  (display counter)
  (display " ADDITIONS")
  (newline)
  )
;;-------------------------------------------------------
;; myfib 20 has 6764 ADDITIONS 
;; $1 = #t
;; ------------------------------------------------------


#|

is there any difference between being lexically inside a function
and that function calling other add operations ?

(= 1 1)

(= "alpha" 3) will raise an error instead of returning false
is #f the correct behaviour here ?



|#





    



	      



  






