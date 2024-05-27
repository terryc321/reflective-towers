
;; each environment is an assoc-list
;; reason use lists is to build up 
(define (lookup-value exp env cont fk)
  (cond
   ((null? env) (fk (list "expression not found in environment" exp env cont fk)))
   (#t (let* ((ribs (car env))
	      (pr (assoc exp ribs)))
	 (cond
	  (pr (cont (second pr)))
	  (#t (lookup-value exp (cdr env) cont fk)))))))


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


;; advantage of currying ?
;; does remembering everything cause huge space leak ??
(define (evaluate exp env cont fk)
  (let ((fk2 (lambda (err) (cons (list 'evaluated exp env cont)
				 err))))
    (cond
     ((boolean? exp) (cont exp))
     ((number? exp) (cont exp))
     ((string? exp) (cont exp))
     ((vector? exp) (cont exp))
     ((symbol? exp) (lookup-value exp env cont fk2))
     ((ev-lambda? exp) (ev-lambda exp env cont fk2))
     ((ev-assign? exp) (ev-assign exp env cont fk2))
     ((ev-define? exp) (ev-define exp env cont fk2))   
     ((ev-begin? exp) (ev-begin exp env cont fk2))   
     ((ev-quote? exp) (ev-quote exp env cont fk2))   
     ((ev-if? exp) (ev-if exp env cont fk2))
     ((ev-load? exp) (ev-load exp env cont fk2))
     ((ev-quit? exp) 'quit)
     ((ev-import? exp) (ev-import exp env cont fk2))
     (else
      (ev-application exp env cont fk2)))))



;;; ----- theres no import in the upper interpreter yet --------
(define (ev-import? exp)
  (and (pair? exp) (eq? (car exp) 'import)))

(define (ev-import exp env cont fk)
  (let ((args (cdr exp)))
    (cont #t)))


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
		(d-e-b-u-g 1 level "Core-DEBUG" env k fk)))))



;; debug repl 
(define (d-e-b-u-g n level prompt env k fk)
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
		    (d-e-b-u-g (+ n 1) level prompt env k fk))
	      (lambda (fail-val)
		(display ">>?? Fail ")
		(display " : ")
		(display fail-val)
		(d-e-b-u-g 1 (+ 1 level) prompt env k fk)))))


;; core - should reload core !
(define (test)
  ;; reloads this file every time ...
  (load "core.scm") 
  (let ([n 1]
	[level 1]
	[prompt "core-ready"]
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
	 (list 'list list)
	 (list 'reverse reverse)	 	 
	 ))))

   












   
   
   
   
   
  



