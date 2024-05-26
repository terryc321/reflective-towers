(import scheme)
(import (chicken syntax))
(import expand-full)
(import (chicken pretty-print))
(import (chicken format))
(import srfi-69) ;; hash tables

;; save original eval if ever need it again  
(define original-eval eval)

(define first car)
(define second cadr)


(define (lookup-binding exp env cont fk)
  (cond
   ((null? env) (fk (list "expression not found in environment" exp env cont fk)))
   (#t (let* ((ribs (car env))
	      (pr (assoc exp ribs)))
	 (cond
	  (pr (cont pr))
	  (#t (lookup-binding exp (cdr env) cont fk)))))))


(define (bawk s)
  (display s)
  (newline)
  (display "exiting .........")
  (newline))


(define make-environment
  (lambda (x)
    (lambda () x)))


;; advantage of currying ?
;; does remembering everything cause huge space leak ??
(define (evaluate exp env cont fk)
  (let ((fk2 (lambda (err) (fk (cons (list 'evaluated exp (make-environment env) cont)
				 err)))))
    (cond
     ((boolean? exp) (cont exp))
     ((number? exp) (cont exp))
     ((string? exp) (cont exp))
     ((vector? exp) (cont exp))
     ;; 
     ((symbol? exp) (ev-symbol exp env cont fk2))
     ;; (lambda (..) ...)
     ((ev-lambda? exp) (ev-lambda exp env cont fk2))
     ;; (set! x y)
     ((ev-assign? exp) (ev-assign exp env cont fk2))
     ;; (define f ...)
     ;; (define (f x) ...)
     ((ev-define? exp) (ev-define exp env cont fk2))   
     ((ev-begin? exp) (ev-begin exp env cont fk2))   
     ((ev-quote? exp) (ev-quote exp env cont fk2))   
     ((ev-if? exp) (ev-if exp env cont fk2))
     ;; (cond (test expr1 expr2) (test2 expr3 expr4 ..)..)
     ((ev-cond? exp) (ev-cond exp env cont fk2))
     ;; let  parallel let
     ((ev-let? exp) (ev-let exp env cont fk2))
     ;; let* sequential let
     ((ev-let*? exp) (ev-let* exp env cont fk2))
     ;; letrec 
     ((ev-letrec? exp) (ev-letrec exp env cont fk2))
     ;; 
     ;; load 
     ((ev-load? exp) (ev-load exp env cont fk2))
     ((ev-quit? exp) 'quit)
     ((ev-import? exp) (ev-import exp env cont fk2))     
     (else
      (ev-application exp env cont fk2)))))


;;----------------- letrec -------------------------
;;
;; turn (letrec ((foo (lambda ...))) ... body ... (foo))
;; into (let ((foo #f)) (set! foo (lambda ..)) ... body ... (foo) )
;;
(define (ev-letrec? exp)
  (and (pair? exp) (eq? (car exp) 'letrec)))

(define (ev-letrec exp env cont fk)
  (let* ((var-bods (car (cdr exp)))
	 (body (cdr (cdr exp)))
	 (vars (map car var-bods))
	 (bods (map cdr var-bods))
	 (falses (map (lambda (x) `(,x #f)) vars))
	 (sets (map (lambda (x) (cons 'set! x)) var-bods)) ;; (set! foo (lambda ...))
	 (macro-result `(let ,falses ,@sets ,@body)))
    ;; (newline)
    ;; (format #t "LETREC vars  -> ~a ~%" vars)    
    ;; (format #t "LETREC in  -> ~a ~%" exp)
    ;; (format #t "LETREC out -> ~a ~%" macro-result)
    (evaluate macro-result     
	      env
	      cont
	      fk)))



;; ---------------- let* ---------------------------
;; macro
;; convert (let* ((a ...)(b...)(c...)) ...)
;;  into  ((lambda (a) ...) (begin a..))
(define (ev-let*? exp)
  (and (pair? exp) (eq? (car exp) 'let*)))

(define (recur-let* vars bods body)
  (cond
   ((null? (cdr vars))
    (let ((var (car vars))
	  (bod (car bods)))
      `((lambda (,var) ,@body) ,bod)))
   (#t
    (let ((var (car vars))
	  (bod (car bods))
	  (other-vars (cdr vars))
	  (other-bods (cdr bods)))
      `((lambda (,var) ,(recur-let* other-vars other-bods body)) ,bod)))))


(define (ev-let* exp env cont fk)
  (let* ((var-bods (car (cdr exp)))
	 (body (cdr (cdr exp)))
	 (vars (map car var-bods))
	 (bods (map cdr var-bods))
	 (begin-bods (map (lambda (b) (cons 'begin b)) bods))
	 (macro-result (recur-let* vars begin-bods body)))
    ;; (newline)
    ;; (format #t "LET* in  -> ~a ~%" exp)
    ;; (format #t "LET* out -> ~a ~%" macro-result)
    (evaluate macro-result     
	      env
	      cont
	      fk)))





;; ---------------- let ---------------------------
;; macro
;; convert (let ((a ...)(b...)(c...)) ...)
;;  into  ((lambda (a b c) ...body...) (begin a..) (begin b..) (begin c..))
(define (ev-let? exp)
  (and (pair? exp) (eq? (car exp) 'let)))

(define (ev-let exp env cont fk)
  (let* ((var-bods (car (cdr exp)))
	 (body (cdr (cdr exp)))
	 (vars (map car var-bods))
	 (bods (map cdr var-bods))
	 (begin-bods (map (lambda (b) (cons 'begin b)) bods))
	 (macro-result `((lambda ,vars ,@body) ,@begin-bods)))
    (evaluate macro-result     
	      env
	      cont
	      fk)))



;; ----------- symbol lookup ------------------------
(define (ev-symbol exp env cont fk)
  (lookup-binding exp env (lambda (v) (cont (second v)))
		  (lambda (err)
		    (display "ev-symbol ERROR :[ ")
		    (display exp)		    
		    (display " ] : symbol not found in environment")
		    (newline)
		    (fk err))))




;; ---------------- cond ---------------------------
;; convert (cond ...) into (if ...)
;; 
(define (ev-cond? exp)
  (and (pair? exp) (eq? (car exp) 'cond)))


;; ;; allowed else keyword 
;; (cond
;;  (test1 ...)
;;  (test2 ...)
;;  (else ...))
(define (ev-cond exp env cont fk)
  (ev-cond-cases (cdr exp) env cont fk))

(define (ev-cond-cases cases env cont fk)
  (cond
   ((null? cases) (cont (make-undefined)))
   (#t (let* ((test-body (car cases))
	      (test (car test-body))
	      (body (cdr test-body)))
	 (cond
	  ((and (null? (cdr cases)) (eq? test 'else))
	   (ev-sequence body (make-undefined) env cont fk))
	  (#t
	   (evaluate test env
		     (lambda (v)
		       (if v
			   (ev-sequence body (make-undefined) env cont fk)
			   (ev-cond-cases (cdr cases) env cont fk)))
		     fk)))))))







;;; ----- theres no import in the upper interpreter yet --------
;; not sure what import is supposed to do
;; 
(define (ev-import? exp)
  (and (pair? exp) (eq? (car exp) 'import)))

(define (ev-import exp env cont fk)
  (let ((args (cdr exp)))
    (cont #t)))

;; -------------load from a file -------------------------------
(define (ev-load? exp)
  (and (pair? exp) (eq? (car exp) 'load)))

;; (load "file..")
(define (ev-load-seq port env cont fk)
  (let ((in (read port)))
    (cond
     ((eof-object? in) (cont (make-undefined)))
     (#t
      (newline)
      (display in)
      (newline)
      (evaluate in
		   env
		   (lambda (v)
		     (newline)
		     (display ">> ")
		     (display v)
		     (newline)		     
		     (ev-load-seq port env cont fk))
		   fk)))))

     

(define (ev-load exp env cont fk)
  (let* ((args (cdr exp))
	 (arg1 (car args)))
    (evaluate arg1 env
	      (lambda (filename)
		(let ((port (open-input-file filename)))
		  (ev-load-seq port env (lambda (result)
					  (close-input-port port)
					  (cont result))
			       fk)))
	      fk)))


(define (tag x y)
  (cons x y))

(define (untag x)
  (cdr x))

(define (make-closure x y)
  (tag 'closure
       (list (list 'lam x)
	     (list 'env (lambda () y)))))


;; primitive procedures do not use environment
(define (make-primitive-procedure x)
  (tag 'primitive-procedure (list 'procedure x)))

(define (primitive-procedure x)
  (second (untag x)))


;; primitive-procedure
;; compound procedure
(define (primitive-procedure? x)
  (and (pair? x) (eq? (car x) 'primitive-procedure)))

(define (compound-procedure? x)
  (and (pair? x) (eq? (car x) 'closure)))

(define (compound-procedure-lambda x)
  (second (assoc 'lam (untag x))))

(define (compound-procedure-args x)
  (second (compound-procedure-lambda x)))

(define (compound-procedure-body x)
  (cdr (cdr (compound-procedure-lambda x))))



(define (ev-args args acc env cont fk)
  (cond
   ((null? args) (cont (reverse acc)))
   (#t (evaluate (car args) env
		 (lambda (val)
		   (ev-args (cdr args) (cons val acc) env cont fk))
		 fk))))

(define (ev-primitive-apply efun eargs env cont fk)
  (let ((fn (primitive-procedure efun)))
    (cont (apply fn eargs))))


#|
wrap environment prevent splurging environment out to stdout as probably circular list
once a single - one closure is created  - environment then becomes tricky to print
if not careful end in an infinite loop

('closure (lambda (x) x) (lambda () env))
|#
(define (ev-compound-apply efun eargs env cont fk)
  ;; (display "ev-compound-apply ")
  ;; (newline)
  ;; (display "efun -> ")
  ;; (display efun)
  ;; (newline)
  ;; (display "args -> ")
  ;; (display eargs)
  ;; (newline)
  (let* ((vars (compound-procedure-args efun))
	 (body (compound-procedure-body efun))
	 (fresh-alist (map (lambda (x y) (list x y)) vars eargs))
	 (new-env (cons fresh-alist env)))    
    ;; (display "vars -> ")
    ;; (display vars)
    ;; (newline)
    ;; (display "body -> ")
    ;; (display body)
    ;; (newline)
    (ev-sequence body (make-undefined) new-env cont fk)))


(define (ev-application exp env cont fk)
  (let* ((args (cdr exp))
	 (fun (car exp)))
    (ev-args args '()
		   env
		   (lambda (eargs)
		     (evaluate fun env (lambda (efun)
					 (cond
					  ((primitive-procedure? efun)
					   (ev-primitive-apply efun eargs env cont fk))
					  ((compound-procedure? efun)
					   (ev-compound-apply efun eargs env cont fk))))
			       fk))
		   fk)))



(define (ev-lambda? exp)
  (and (pair? exp) (eq? (car exp) 'lambda)))

;; (if x y z?)
(define (ev-lambda exp env cont fk)
  (cont (make-closure exp env)))

(define (ev-quit? exp)
  (and (pair? exp) (eq? (car exp) 'quit)))


(define (ev-begin? exp)
  (and (pair? exp) (eq? (car exp) 'begin)))

(define (make-undefined)
  (tag 'undefined (lambda () 'undefined)))

(define (undefined? x)
  (and (pair? x) (eq? (car x) 'undefined)))


;; (begin . . . )
(define (ev-begin exp env cont fk)
  (let ((args (cdr exp)))
    (cond
     ((null? args) (cont (make-undefined)))
     (else
      (let ((arg1 (car args)))
	(evaluate arg1 env
		  (lambda (v)
		    (ev-sequence (cdr args) v env cont fk))
		  fk))))))

;; evaluates sequence remembering last result  
(define (ev-sequence args result env cont fk)
  (cond
   ((null? args) (cont result))
   (#t
    (let ((arg1 (car args)))
      (evaluate arg1 env
		(lambda (v)
		  (ev-sequence (cdr args) v env cont fk))
		fk)))))


(define (ev-if? exp)
  (and (pair? exp) (eq? (car exp) 'if)))

;; (if x y z?)
(define (ev-if exp env cont fk)
  (let* ((args (cdr exp))
	 (con (car args))
	 (true-part (car (cdr args)))
	 (false-part #f))
    (evaluate con env
	      (lambda (ec)
		(if ec
		    (evaluate true-part env cont fk)
		    (cond
		     ((pair? (cdr (cdr (cdr exp))))
		      (let ((false-part (car (cdr (cdr (cdr exp))))))
			(evaluate false-part env cont fk)))
		     (#t (cont #f)))))
	      fk)))


(define (do-define var val env renv cont fk)
  (lookup-binding var env
	  (lambda (binding)
	    (set-cdr! binding (list val))
	    (cont val))
	  (lambda (err)
	    (cond
	     ;; tack onto front list of env
	     ((pair? (car env))
	      (let ((alist (car env)))
		(set-car! env (cons (list var val) alist))
		(cont val)))
	     (else 	      
	      (fk (list "define - unable to create binding for " var " bad environment ")))))))


(define (ev-define? exp)
  (and (pair? exp) (eq? (car exp) 'define)))

;; (define a b)
(define (ev-define exp env cont fk)
  (let ((args (cdr exp)))
    (cond
     ((pair? (car args)) (ev-define2 exp env cont fk))
     (#t (ev-define3 exp env cont fk)))))



;; (define (f x y) ... )
(define (ev-define2 exp env cont fk)
  (let* ((args (cdr exp))
	 (body (cdr args))
	 (varlist (car args))
	 (name (car varlist))
	 (vars (cdr varlist)))
    (evaluate `(define ,name (lambda ,vars ,@body ))
	      env
	      cont
	      fk)))
	 
;; (define f (lambda (x y z) ...))
(define (ev-define3 exp env cont fk)
  (let* ((args (cdr exp))
	 (var (car args))
	 (val (cadr args)))
    (evaluate val env
	      (lambda (val2)
		(do-define var val2 env env cont fk))
	      fk)))



(define (ev-assign? exp)
  (and (pair? exp) (eq? (car exp) 'set!)))

(define (ev-assign exp env cont fk)
  (let* ((args (cdr exp))
	 (var (car args))
	 (val (cadr args)))
    (evaluate val env
	      (lambda (val2)
		(do-assign var val2 env env cont fk)
		(display "assigning ")
		(display var)
		(display " <- ")
		(display val2)
		(newline)
		(cont val2))
	      fk)))


(define (do-assign var val env renv cont fk)
  (lookup-binding var env
	  (lambda (binding)
	    (set-cdr! binding (list val))
	    (cont val))
	  (lambda (err)
	    (fk (cons
		 (list "assignment no binding for " var " found in environment")
		 err)))))


(define (ev-quote? exp)
  (and (pair? exp) (eq? (car exp) 'quote)))

(define (ev-quote exp env cont fk)
  (let* ((args (cdr exp))
	 (var (car args)))
    (cont var)))

;; interpreter repl 
(define (r-e-p-l n level prompt env k fk)
  (newline)
  (display prompt)
  (display " ")
  (display n)
  (display " / ")
  (display level)
  (display " : ")
  (let ((exp (read)))
    (display exp)
    (newline)
    (evaluate exp env (lambda (val)
			(display " ")
			(display n)
			(display " / ")
			(display level)
			(display " ")			
		    (display ">> ")
		    (display val)
		    (r-e-p-l (+ n 1) level prompt env k fk))
	      (lambda (fail-val)
		(display ">>?? Fail ")
		(display " : ")
		(display fail-val)
		;; give debugger way to escape back into repl where error
		;; threw us into debugger and pretend we did not see the error...
		(d-e-b-u-g 1 level "DEBUG" env (lambda (res)
						 (r-e-p-l n level prompt env k fk))
			   k fk)))))



;; debug repl 
(define (d-e-b-u-g n level prompt env repl-k k fk)
  (newline)
  (display prompt)
  (display " ")
  (display n)
  (display " / ")
  (display level)
  (display " : ")
  (let ((exp (read)))
    (display exp)
    (newline)
    ;; pre-empty
    (cond
     ((equal? exp '(unquote q))
      (format #t "leaving debugger ~%")
      (repl-k #t))
     (#t ;; carry on in debugger 
      (evaluate exp env (lambda (val)
			  (display " ")
			  (display n)
			  (display " / ")
			  (display level)
			  (display " ")			
			  (display ">> ")
			  (display val)
			  (d-e-b-u-g (+ n 1) level prompt env repl-k k fk))
		(lambda (fail-val)
		  (display ">>?? Fail ")
		  (display " : ")
		  (display fail-val)
		  (d-e-b-u-g 1 (+ 1 level) prompt env repl-k k fk)))))))





;; interpreter.scm should reload interpreter.scm
(define (test)
  (load "interpreter.scm") 
  (let ([n 1]
	[level 1]
	[prompt "ready"]
	[cont (lambda (x) x)]
	[fail (lambda (x) (list 'fail x))])
    (r-e-p-l n
	     level
	     prompt
	     (the-global-environment)
	     cont
	     fail)))




;; all the stuff from the host environment
;; if something is a procedure - from host system for example - make it a primitive-procedure
;; if not - just a value and can be used as that
(define (the-global-environment)
  (list
   (map (lambda (x)
	  (let ((sym (car x))
		(val (cadr x)))
	    (cond
	     ((procedure? val) (list sym (make-primitive-procedure val)))
	     (#t (list sym val)))))
	(list
	 (list 'number? number?)
	 (list 'integer? integer?)
	 ;; (list 'float? float?) ;; what is a float?
	 (list 'rational? rational?)
	 (list 'string? string?)
	 
	 (list 'vector? vector?)
	 (list 'vector-ref vector-ref)
	 (list 'vector-set! vector-set!)

	 (list 'list? pair?)
	 (list 'list-ref list-ref)
	 (list 'set-car! set-car!)
	 (list 'set-cdr! set-cdr!)

	 (list 'append append)
	 
	 (list '< <)
	 (list '> >)
	 (list '= =)
	 (list '>= >=)
	 (list '<= <=)
	 (list '+ +)
	 (list '* *)
	 (list '- -)
	 (list '/ /)	 
	 (list 'assoc assoc)
	 (list 'member member)	 
	 (list 'null? null?)
	 (list 'cons cons)
	 (list 'car car)
	 (list 'cdr cdr)
	 (list 'caar caar)
	 (list 'cadr cadr)
	 (list 'cddr cddr)
	 
	 (list 'list list)
	 (list 'reverse reverse)	 	 
	 (list 'eq? eq?)
	 (list 'equal? equal?)
	 ;;(list 'map map) ;; cannot use higher order procedures
	 (list 'format format)
	 (list 'display display)
	 (list 'newline newline)
	 ))))




   












   
   
   
   
   
  



