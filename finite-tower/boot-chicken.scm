

;; Boot script for chicken scheme

;; load everything and the kitchen sink and more ......

(import scheme)
(import expand-full)
(import simple-exceptions)
(import (chicken repl))
(import (chicken string))
(import (chicken pretty-print))
(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day17")
;; (get-current-directory)
(import procedural-macros)
(import regex)
(import simple-md5)
(import simple-loops)
;; hash-table-ref  hash key thunk
;; hash-table-set! hash key val
;; sudo chicken-install srfi-178
(import srfi-178)
;; srfi-178 provides bit-vectors
;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))
(import sequences)
(import srfi-1)
(import matchable)
(define pp pretty-print)
(import srfi-69) ;; hash tables


;; ------------ macros ---------------------------------------
;; dolist
(define-macro (dolist varlist . body)
  (let ((var (car varlist))
	(ls (cadr varlist))
	(fn (gensym "fn")))	
    `(begin
       (letrec
	   ((,fn (lambda (xs)
		   (cond
		    ((null? xs) #f)
		    (#t (let ((,var (car xs)))
			  ,@body
			  (,fn (cdr xs))))))))
	 (,fn ,ls)))))


;; dofor
;; cannot handle decreasing steps ?
(define-macro (for v . body)
  (let ((var (car v))
	(init (cadr v))
	(lim (caddr v))
	(step (cadddr v))	      
	(foo (gensym "foo"))
	(v-i (gensym "i"))
	(v-step (gensym "step"))
	(v-lim (gensym "lim")))
    `(begin
       (letrec ;; want to capture var
	   ((,foo (lambda (,var ,v-step ,v-lim)
		    (cond
		     ((> ,var ,v-lim) #f)
		     (#t
		      ,@body
		      (,foo (+ ,var ,v-step) ,v-step ,v-lim))))))
	 (,foo ,init ,step ,lim)))))

;;(pp (expand* '(for (i 1 10 1) (format #t "i = ~A ~%" i))))
;; (for (i 1 10 1) (format #t "i = ~A ~%" i))
;; (for (i 10 1 -1) (format #t "i = ~A ~%" i))

;; ---------------------------------------------------------------------------



(load "defines.scm")

;; each level in tower uses set! as define + set ?
;; we will find out 
(load "simple.scm")

;; start the tower i suppose
(newline)
(display "please run (boot-tower) to start tower interpreter")
(newline)



