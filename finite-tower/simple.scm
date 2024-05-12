

;; ----------------------------------------------------
;; Code copied verbatim from this paper
;; "A Simple Reflective Interpreter"
;; Stanley Jefferson
;; Daniel P Friedman

;; IMSA '92 International Workshop on Reflection and
;; Meta-Level Architecture
;; Tokyo , November 4-7 , 1992
;; ----------------------------------------------------

;; a simple reflective interpreter

;; in some scheme systems it may be necessary to
;; first load a seperate file that initializes
;; every global variable in this file to some
;; arbitrary value using DEFINE.  It may also be
;; necessary to include the following definition
;; in that file:

;; (set! void
;;   (let ((g (cons '* '*)))
;;     (lambda () g)))

;; let not defined yet
(set! sys/void (lambda () (cons '* '*)))


(set! evaluate
  (lambda (e r k)
    ((cond
     ((constant? e) evaluate-constant)
     ((variable? e) evaluate-variable)
     ((if? e)       evaluate-if)
     ((assignment? e)  evaluate-assignment)
     ((abstraction? e) evaluate-abstraction)
     (#t evaluate-combination))
     e r k)))


(set! evaluate-constant
  (lambda (e r k)
    (k (constant-part e))))

(set! evaluate-variable
  (lambda (e r k)
    (get-pair e r
	      (lambda (success-pair)
		(k (cdr success-pair)))
	      (lambda ()
		(wrong "symbol not bound: " e)))))

(set! wrong
  (lambda (message object)
    (display "Error: ")
    (display message)
    (display object)
    (newline)))

(set! evaluate-if
  (lambda (e r k)
    (evaluate (test-part e) r
	      (lambda (v)
		(if v
		    (evaluate (then-part e) r k)
		    (evaluate (else-part e) r k))))))

(set! evaluate-assignment
  (lambda (e r k)
    (evaluate (value-part e) r
	      (lambda (v)
		(get-pair (id-part e) r
			  (lambda (success-pair)
			    (set-cdr! success-pair v)
			    (k (sys/void)))
			  (lambda ()
			    (set-cdr! global-env
				      (cons (cons (id-part e) v)
					    (cdr global-env)))
			    (k (sys/void))))))))


(set! evaluate-abstraction
  (lambda (e r k)
    (k (make-compound
	(formals-part e) (body-part e) r))))

(set! evaluate-combination
  (lambda (e r k)
    
    (display "evaluate-combination Line93 :")
    (newline)
    (display "e = ")
    (display e)
    (newline)
    (display "operands-part e = ")
    (display (operands-part e))
    (newline)
    (display "operator-part e = ")
    (display (operator-part e))
    (newline)
					 
    (evaluate (operator-part e) r
	      (lambda (proc)
		(if (reifier? proc)
		    ;; if proc is a reifier
		    ((reifier-to-compound proc)
		     (operands-part e) r k)
		    ;; proc not reifier
		    (evaluate-operands (operands-part e) r
				       (lambda (args)
					 (display "evaluate-combination Line 114 : ")
					 (newline)
					 (display "args = ")
					 (display args)
					 (newline)
					 (display "proc = ")
					 (display args)
					 (newline)
					 
					 (apply-procedure proc args k)
					 )))))))





(set! evaluate-operands
  (lambda (operands r k)
    (if (null? operands)
	(k '())
	(evaluate (car operands) r
		  (lambda (v)
		    (evaluate-operands (cdr operands) r
				       (lambda (w)
					 (k (cons v w)))))))))


(set! evaluate-sequence
  (lambda (body r k)
    (if (null? (cdr body))
	(evaluate (car body) r k)
	(evaluate (car body) r
		  (lambda (v)
		    (evaluate-sequence (cdr body) r k))))))


(set! evaluate-sequence
  (lambda (body r k)
    (if (null? (cdr body))
	(evaluate (car body) r k)
	(evaluate (car body) r
		  (lambda (v)
		    (evaluate-sequence (cdr body) r k))))))

(set! apply-procedure
  (lambda (proc args k)
    (display "apply-procedure Line 169 : ")
    (display "proc = ") (display proc) (newline)
    
    (if (compound? proc)
	(evaluate-sequence
	 (procedure-body proc)
	 (extend
	  (procedure-environment proc)
	  (procedure-parameters proc)
	  args)
	 k)
	(k (apply-primitive
	    (procedure-name proc) args)))))



;; ------------ end of page 56  ------------------

(set! apply-primitive
  (lambda (name args)
    (cond
     ((eq? name 'car) (car (1st args)))
     ((eq? name 'cdr) (cdr (1st args)))
     ((eq? name 'cons) (cons (1st args) (2nd args)))
     ((eq? name 'set-car!) (set-car! (1st args) (2nd args)))
     ((eq? name 'set-cdr!) (set-cdr! (1st args) (2nd args)))
     ((eq? name 'assq) (assq (1st args) (2nd args)))
     ((eq? name 'memq) (memq (1st args) (2nd args)))
     ((eq? name 'null?) (null? (1st args)))
     ((eq? name '=) (= (1st args) (2nd args)))
     ((eq? name 'eq?) (eq? (1st args) (2nd args)))
     ((eq? name 'newline) (newline))
     ((eq? name 'write) (write (1st args)))
     ((eq? name 'display) (display (1st args)))
     ((eq? name 'read) (if (null? args) (read) (read (1st args))))
     ((eq? name '+) (+ (1st args) (2nd args)))
     ((eq? name '-) (- (1st args) (2nd args)))
     ((eq? name '*) (* (1st args) (2nd args)))
     ((eq? name 'symbol?) (symbol? (1st args)))
     ((eq? name 'list)
      (display "list => ") (display args) (newline)
      args)
     ((eq? name 'pair?) (pair? (1st args)))
     ((eq? name 'eof-object?) (eof-object? (1st args)))
     ((eq? name 'close-input-port) (close-input-port (1st args)))
     ((eq? name 'open-input-file) (open-input-file (1st args)))
     ((eq? name 'sys/void) (sys/void))
     (#t "Shouldn't Happen!"))))




;;;
;;; Environments.							    
;;;

(set! extend
  (lambda (r ids vals)
    (if (null? ids)
	r
	(extend
	 (cons (cons (car ids)
		     (cdr ids))
	       r)
	 (cdr ids)
	 (cdr vals)))))

(set! get-pair
  (lambda (id r success failure)
    (find-pair id r
	       success
	       (lambda ()
		 (find-pair
		  id global-env success failure)))))


(set! find-pair
  (lambda (elt alist success failure)
    ((lambda (assq-result)
       (if assq-result
	   (success assq-result)
	   (failure)))
     (assq elt alist))))


(set! empty-env '())

;;;
;;; list utilities
;;;


(set! 1st (lambda (l) (car l)))
(set! 2nd (lambda (l) (car (cdr l))))
(set! 3rd (lambda (l) (car (cdr (cdr l)))))
(set! 4th (lambda (l) (car (cdr (cdr (cdr l))))))


(set! test-tag
  (lambda (tag)
    (lambda (e)
      (if (pair? e) (eq? (car e) tag) #f))))


;;;
;;; Procedures.
;;;

(set! make-compound
  (lambda (formals body r)
    (list 'compound formals body r)))

(set! compound? (test-tag 'compound))

(set! make-primitive
  (lambda (op)
    (list 'primitive op)))


(set! primitive? (test-tag 'primitive))


(set! primitive-identifiers
  (lambda ()
    '(car cdr cons set-car! set-cdr! assq memq
	  null? = eq? newline write display read
	  + - * symbol? list pair? eof-object?
	  close-input-port open-input-file sys/void)))


(set! make-reifier
  (lambda (formals body r)
    (list 'reifier formals body r)))


(set! reifier-to-compound
  (lambda (reifier)
    (cons 'compound (cdr reifier))))


;; -------- end of page 57 -------------


(set! compound-to-reifier
  (lambda (compound)
    (cons 'reifier (cdr compound))))

(set! reifier? (test-tag 'reifier))

(set! procedure-parameters 2nd)
(set! procedure-body 3rd)
(set! procedure-environment 4th)
(set! procedure-name 2nd)

;;;
;;; Syntax.
;;;

(set! variable? symbol?)
(set! if? (test-tag 'if))
(set! assignment? (test-tag 'set!))
(set! abstraction? (test-tag 'lambda))
(set! quote? (test-tag 'quote))

(set! constant?
  (lambda (e)
    (if (pair? e) (quote? e)
	(if (symbol? e) #f #t))))


(set! constant-part
  (lambda (e) (if (quote? e) (2nd e) e)))

(set! test-part 2nd)
(set! then-part 3rd)
(set! else-part 4th)

(set! id-part 2nd)
(set! value-part 3rd)

(set! formals-part 2nd)
(set! body-part (lambda (e) (cdr (cdr e))))

(set! operator-part 1st)
(set! operands-part cdr)

;;;
;;; Read-Eval-Print Loop and Loadfile
;;;

(set! openloop
  (lambda (read-prompt write-prompt)
    (display read-prompt)
    (evaluate (read) empty-env
      (lambda (v)
	(display write-prompt)
	(if (eq? v (sys/void))
	    "Nothing will be displayed"
	    (write v))
	(newline)
	(openloop read-prompt write-prompt)))))


(set! loadfile
  (lambda (file)
    ((lambda (port)
       ((lambda (loop)
	  (set! loop
	    (lambda (v)
	      (if (eof-object? v)
		  (close-input-port port)
		  (evaluate v empty-env
			    (lambda (ignore)
			      (loop (read port)))))))
	  (loop (read port)))
	'*))
     (open-input-file file))))



;;;
;;; The booting process
;;;

(set! mapper
  (lambda (f l)
    (if (null? l)
	'()
	(cons (f (car l)) (mapper f (cdr l))))))


(set! initialize-global-env
  (lambda ()
    (set! global-env
      (extend
       empty-env
       (primitive-identifiers)
       (mapper make-primitive
	       (primitive-identifiers))))))

(set! boot-flat
  (lambda ()
    (initialize-global-env)
    (openloop "0< " "0> ")))

(set! boot-tower
  (lambda ()
    (initialize-global-env)
    (loadfile this-file-name)
    (set-cdr! global-env
	      (cons (cons 'global-env global-env)
		    (cdr global-env)))
    (openloop "0> " "0: ")))


(set! this-file-name "simple.scm")


;; typical run 
#|
(begin
   (load "defines.scm")
   (load "simple.scm")
   (boot-tower))

|#


							

						    

						
						
						
				

			




















