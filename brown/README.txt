
interpreter.scm


(define f ...)
(define (f ...) ...) is another way write (define f (lambda ...))

MAP - in general , higher order functions 
(map f xs)
(map f xs ys)
(map f xs ys ...)
higher order functions such as map - cannot just be pulled from host environment
because function f could be a primitive , or a compound procedure like 
 something defined in the interpreted language

----- TO DO -----
TO DO --- argument slurping (lambda ( f . x ) ...)
TO DO --- argument slurping (lambda  args  ...)


INTERPRETED language
---------------------
  HOST language

how does chicken handle errors ?
how does racket handle errors ?





