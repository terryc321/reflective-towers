
#|

ultra vanilla scheme debugger - think R 4 R S 

ADVANTAGE TO OWN LANGUAGE
( 1 )  (= 'a 'b)
in normal scheme if A is not a number , then fails , rather would it not be better to return false #f
?

1 prefer ultra basic scheme procedures ??

simple meta circular interpreter designed for debugging
using as much plain vanilla scheme forgoing imports modules and anything implementation specific
recursive meta circular
e r k
123 constant so does not need to be evaluated
repl is recursive also
debugger is also like a repl can enter things , evaluate , continue , abort evaluation ,
primitive read  - read / parses / etc.. anything but primitive 


environment needs to be chained so when procedure entered , build up new environment,
when procedure finished ,
environment returned by env := cdr env

ENV : [ current function env ] -> [ other functions in callstack ] -> ... -> [ global environment ]

when current function finished
ENV := CDR ENV

simply drops current function environment
ENV : [ other functions in callstack ] -> ... -> [ global environment ]

ENV is a LIST of { LIST of  { LIST of SYMBOL :: LIST of VALUES } ... } 

ENV as ( (a 3 b 4 c 5) (d 6 f 7 ) )
but limits how much data can put in value

FAVOUR applroach below ... allows meta programming 
ENV as ( ( (a 3 'red) (b 4 'green) (c 5 'red) )  ( (d 1 'blue) (e 2 'green )) )




PROCEDuRES
procedures can be primitive - inbuilt to underlying system , whatever, however that works .
procedures can be compound - ie user defined by user code.

what is output of display
   (display "done for side effect")

any output ?
no output ?


|#

(define this-file-name "debug.scm")


(define global-env #f)

(define empty-env '(()) )

(define primitive-env #f)

;; continuation call-stack 
;; move up and down in the call chain , simply have to remember where been


(define env-lookup
  (lambda (expr env cont)
    (cond
     ((null? env) "not found")
     (#t (let ((var-values (assoc expr (car env))))
	   (cond
	    (var-values (cont (car (cdr var-values))))
	    (#t (env-lookup expr (cdr env) cont))))))))



;; sets expression in environment if not found , returns #f ,
;; otherwise returns pair (symbol value) to distinguish from if value was just #f
;; returned #f could not tell if worked or failed
(define env-set!
  (lambda (env sym val)
    (cond
     ((null? env) #f)
     (#t (let ((var-values (assoc sym (car env))))
	   (cond
	    (var-values (set-cdr! var-values (list val))
			(list sym val))
	    (#t (env-set! (cdr env) sym val))))))))
    


;; try to splice new value into head of list ?
(define env-define
  (lambda (env sym val)
    (cond
     ((env-set! env sym val) val)
     (#t ;;set! failed
      (set-car! env (cons (list sym val) (car env)))
      val))))




;; -------------- ----------------
(define show-env
  (lambda (env)
    (display env)))

    ;; (letrec ((foo (lambda (local-env)
    ;; 		    (cond
    ;; 		     ((null? local-env)		      
    ;; 		      (display "expression ")
    ;; 		      (display expr)
    ;; 		      (display " not found in environment.")
    ;; 		      (newline)
    ;; 		      ;; cannot continue down cont because otherwise computation would continue
    ;; 		      ;; as nothing had happended
    ;; 		      (repl))
    ;; 		     (#t 
    ;; 		      (let ((find (member expr local-env)))
    ;; 			(if find
    ;; 			    (cont find)
    ;; 			    (foo (cdr env)))))))))
    ;;   (foo env))))
    



(define global-reset (lambda () #t))

;; a big block of code for evaluate is not really easy to alter inside itself
;; if we hand off to
;; how can we over-ride the "defaults"  ?

;; prefix ev - to mean evaluate own EVAL ...
;; TO DO - rewrite conditional? to ev-if?
;; TO DO - rewrite sequence? to ev-begin?
(define evaluate
  (lambda (expr env cont)
    (cond
     ((eq? expr '!!) (evaluate-reset expr env cont))
     ((eq? expr '!) (evaluate-quit expr env cont))
     ((number? expr) (evaluate-number expr env cont))
     ((ev-bool? expr) (evaluate-boolean expr env cont))
     ((quoted? expr) (evaluate-quoted expr env cont))
     ((symbol? expr) (evaluate-variable expr env cont))
     ((ev-string? expr) (evaluate-string expr env cont))
     ((definition? expr) (evaluate-define expr env cont))
     ((abstraction? expr) (evaluate-lambda expr env cont))
     ((assignment? expr) (evaluate-set! expr env cont))
     ((conditional? expr) (evaluate-if expr env cont))
     ((sequence? expr) (evaluate-sequence expr env cont))     
     ((application? expr) (evaluate-application expr env cont))
     (#t (display "ERROR : do not know how to evaluate this below ")
	 (newline)
	 (display "> ")
	 (write expr)
	 (newline)))))





(define ev-bool?
  (lambda (expr)
    (or (eq? expr #f)
	(eq? expr #t))))

(define evaluate-boolean
  (lambda (expr env cont)
    (cont expr)))


(define assignment?
  (lambda (expr)
    (and (pair? expr) (eq? (car expr) 'set!))))


(define conditional?
  (lambda (expr)
    (and (pair? expr) (eq? (car expr) 'if))))


(define ev-string?
  (lambda (expr) (string? expr)))

(define evaluate-string
  (lambda (expr env cont)
    (cont expr)))


(define sequence?
  (lambda (expr)
    (and (pair? expr) (eq? (car expr) 'begin))))


;; if it looks like a application then it is an application
(define abstraction?
  (lambda (expr)
    (and (pair? expr) (eq? (car expr) 'lambda))))


;; wrap environment in a lambda so it does not spew into console when printed
;; just a closure = expr + env 
(define evaluate-lambda
  (lambda (expr env cont)
    (cont (list 'compound expr (lambda () env)))))


;; do some lookup ? -- simply hand off to lookup and let that sort it out
;; if something goes wrong - may never return...
(define evaluate-variable
  (lambda (expr env cont)
    (env-lookup expr env cont)))


(define evaluate-reset
  (lambda (expr env cont)
    (newline)
    (display "RESET ! Resetting .")
    (global-reset)
    (newline)))


;; to quit - simply do not pass anything to continuation cont
;; thereby breaking the call chain
(define evaluate-quit
  (lambda (expr env cont)
    (newline)
    (display "QUITTING ! Returning to default scheme implementation .")
    (newline)))



(define evaluate-set!
  (lambda (expr env cont)

		  (newline)
		  (display " evaluate-set!   ")
		  (newline)
    
    (let ((sym (car (cdr expr)))
	  (uneval (car (cdr (cdr expr)))))

      (newline)
      (display " sym =   ")
      (display sym)
      (newline)
      
      (display "uneval =   ")
      (display uneval)
      (newline)
            
      (evaluate uneval env 
		(lambda (val)
		  
		  (newline)
		  (display "   trying to set!   ")
		  (display sym)
		  (display " <- ")
		  (display val)
		  (newline)

		  (let ((outcome (env-set! env sym val)))

		    ;; (newline)
		    ;; (display "  env-set! outcome :  ")
		    ;; (display outcome)
		    ;; (display " ?? pair ? ")
		    ;; (display (pair outcome))
		    ;; (newline)
		    
		    (cond
		     ((pair? outcome) (cont val))
		     (#t
		      (newline)
		      (display "ERROR : no variable ")
		      (display sym)
		      (display " in environment to set.")
		      (newline)))))))))




(define evaluate-sequence
  (lambda (expr env cont)
    (evaluate-implicit-sequence (cdr expr) env cont)))

(define evaluate-implicit-sequence
  (lambda (expr env cont)
    (cond
     ((null? (cdr expr)) (evaluate (car expr) env cont))
     (#t (evaluate (car expr) env (lambda (val)
				    (evaluate-implicit-sequence (cdr expr) env cont)))))))


;; (if a b)
;; (if a b c)


(define evaluate-if
  (lambda (expr env cont)
    (let ((ev-cond (car (cdr expr)))
	  (ev-true (car (cdr (cdr expr)))))
      (evaluate ev-cond env 
		(lambda (val)
		  (if val
		      (evaluate ev-true env cont)
		      (if (null? (cdr (cdr (cdr expr))))
			  (cont #f)
			  (let ((ev-false (car (cdr (cdr (cdr expr))))))
			    (evaluate ev-false env cont)))))))))


(define evaluate-define
  (lambda (expr env cont)
    (let ((sym (car (cdr expr)))
	  (uneval (car (cdr (cdr expr)))))
    (evaluate uneval env 
	      (lambda (val)
		(env-define env sym val)
		(cont val))))))


(define evaluate-application
  (lambda (expr env cont)
    (let ((un-args (appl-operands expr)))
      (evaluate-arguments un-args env '()
			  (lambda (e-args)
			    (evaluate (appl-operator expr) env
				      (lambda (op)
					(cond
					 ((primitive? op) (cont (apply (car (cdr op)) e-args)))
					 ((compound? op)
					  (evaluate-compound op e-args env cont))))))))))


;; make a new environment list
;; which is cons'd onto original environment
;;
;; may need another list - wrapping this 
(define (env-tie symbols values)  
  (map (lambda (x y) (list x y)) symbols values))



;; (define ev-call/cc
;;   (lambda (expr env cont)
;;     (let ((lam (car (cdr expr))))
;;       (evaluate lam env (lambda (val)
;; 			  (


;; cfn - compound function
;; un-args un-evaluated arguments ?
;; e-args evaluated args ?
;; env
;; cont
(define evaluate-compound
  (lambda (cfn e-args env cont)
    
    ;; (newline)
    ;; (display "cfn = ")
    ;; (display cfn)
    ;; (newline)
    
    (let* ((lam (car (cdr cfn)))
	   (lam-args (car (cdr lam)))
	   (lam-body (cdr (cdr lam)))
	   (closure-env-delayed (car (cdr (cdr cfn))))
	   )

      ;; (newline)
      ;; (display "lam = ")
      ;; (display cfn)
      ;; (newline)
      ;; 
      ;; (newline)
      ;; (display "lam.args = ")
      ;; (display lam-args)
      ;; (newline)
      ;; (display "e-args = ")
      ;; (display e-args)
      ;; (newline)
      ;; 
      ;; (newline)
      ;; (display "lam.body = ")
      ;; (display lam-body)
      ;; (newline)

      ;; for toplevel procedures , no need for closure since everything is in the global-env
      ;; notice we call closure-env-delayed as there is an anon lambda no args holding
      ;; closure environment safe from printing out .
      (let ((new-env (cons (env-tie lam-args e-args) (closure-env-delayed))))
	(evaluate-implicit-sequence lam-body
				    new-env
				    (lambda (result)
				      (cont result)))))))
    
    



(define evaluate-arguments
  (lambda (args env zs cont)
    (cond
     ((null? args) (cont (reverse zs)))
     (#t (evaluate (car args) env
		   (lambda (val)
		     (evaluate-arguments (cdr args) env (cons val zs) cont)))))))


(define appl-operator
  (lambda (expr) (car expr)))

(define appl-operands
  (lambda (expr) (cdr expr)))

;; if it looks like a application then it is an application
(define application?
  (lambda (expr)
    (pair? expr)))



;; if it looks like a application then it is an application
(define definition?
  (lambda (expr)
    (and (pair? expr) (eq? (car expr) 'define))))


;; is this expression a quoted expression
;; scheme reader will turn 'a into (quote a )
(define quoted?
  (lambda (expr)
    (and (pair? expr)
	 (eq? (car expr) 'quote))))

(define evaluate-number
  (lambda (expr env cont)  
    (cont expr)))

(define evaluate-quoted
  (lambda (expr env cont)
    (cont (car (cdr expr)))))



;; ?? 
(define primitive-read read)

;; primitive expression - such as inbuilt from underlying scheme system
(define primitive? (lambda (x) (and (pair? x) (eq? (car x) 'primitive))))

;; compound exprsession 
(define compound? (lambda (x) (and (pair? x) (eq? (car x) 'compound))))


;; (define (read-eval-print-loop in-prompt out-prompt)
;;   (display in-prompt)
;;   (newline)
;;   (display out-prompt)
;;   )
(define (recur-repl counter)

  ;; ----- dangerous to display ENVIRONMENT because likely to have circular lists ---
  ;; (newline)
  ;; (display "ENV : ")  
  ;; (show-env global-env)
  ;; (newline)

  (newline)
  
  (display "in [")
  (display counter)
  (display "] : ")
  
  (let ((expr (read)))
    ;; (display expr)
    ;; (newline)
    (evaluate expr
	      global-env
	      (lambda (result)
		(newline)
		(display "out [")
		(display counter)
		(display "] : ")
		(display result)
		(newline)
		(recur-repl (+ counter 1))))))




;; debug levels
;; repl is predefined in some scheme environemnts
(define (rep)
  (load this-file-name)
  (newline) (display "reloaded ") (display this-file-name) (newline)
  (recur-repl 0))


;;
(set! primitive-env
  `((
     
     (car (primitive ,car))
     (cdr (primitive ,cdr))
     (cons (primitive ,cons))
     (list (primitive ,list))
     (pair? (primitive ,pair?))
     (null? (primitive ,null?))
     
     (> (primitive ,>))
     (< (primitive ,<))
     (>= (primitive ,>=))
     (<= (primitive ,<=))
     (= (primitive ,=))

     (+ (primitive ,+))
     (- (primitive ,-))
     (* (primitive ,*))
     (/ (primitive ,/))
     (reverse (primitive ,reverse))
     (append (primitive ,append))
     
     (eq? (primitive ,eq?))

     (display (primitive ,display))
     (write (primitive ,write))
     (read (primitive ,read))
     
     (boolean? (primitive ,boolean?))
     (not (primitive ,not))
     
     )))


(set! global-env primitive-env)




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








