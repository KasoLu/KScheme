#lang racket

(require "helper.rkt")
(provide flatten-program)

(define flatten-program
  (lambda (program)
    (match program
      [`(letrec ([,label* (lambda () ,tail*)] ...) ,tail)
        (let 
          ([flat-tail* (map flatten-tail tail*)]
           [flat-lambda (lambda (l t a) `(,@a ,l ,@t))])
         `(code 
            ,@(flatten-tail tail) 
            ,@(foldl flat-lambda '() label* flat-tail*)))])))

(define flatten-tail
  (lambda (tail)
    (match tail
      [`(begin ,effect* ... ,tail)
       `(,@effect* ,@(flatten-tail tail))]
      [`(,triv)
       `((jump ,triv))])))

; ----- test ----- ;
;(test flatten-tail
;      '(begin (set! rax 10) (r15)) '(r15))
;(test flatten-program
;      '(letrec ([main$0 (lambda () (begin (set! rax 10) (main$0)))]
;                [main$1 (lambda () (begin (set! rbx 20) (main$1)))])
;         (r15)))

