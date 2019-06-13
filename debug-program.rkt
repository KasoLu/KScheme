#lang racket

(require "helper.rkt")
(provide debug-program)

(define debug-program
  (lambda (program)
    (match program
      [`(letrec ([,label* (lambda () ,tail*)] ...) ,tail)
        (let 
          ([debug-lambda (lambda (l t) `(,l (lambda () ,t)))]
           [debug-tail* (map debug-tail tail*)])
         `(letrec 
            ,(map debug-lambda label* debug-tail*)
            ,(debug-tail tail)))])))

(define debug-tail
  (lambda (tail)
    (match tail
      [`(begin ,effect* ... ,tail)
       `(begin ,@(map debug-effect effect*) ,(debug-tail tail))]
      [`(,triv)
       `(,(debug-triv triv))])))

(define debug-effect
  (lambda (effect)
    (match effect
      [`(set! ,var (,binop ,triv-1 ,triv-2))
       `(set-box! ,(debug-var var) 
                   (int64-truncate (,binop ,(debug-triv triv-1) ,(debug-triv triv-2))))]
      [`(set! ,var ,triv)
       `(set-box! ,(debug-var var) ,(debug-triv triv))])))

(define debug-triv
  (lambda (triv)
    (match triv
      [(? var?)
      `(unbox ,(debug-var triv))]
      [(? int?)
       (begin triv)]
      [(? label?)
       (begin triv)])))

(define debug-var
  (lambda (var)
    (match var
      [(? reg?)
       (begin var)]
      [(? frame-var?)
      `(vector-ref stack (+ (/ (unbox rbp) 8) ,(frame-var->index var)))])))

; ----- test ----- ;
;(test debug-program
;      '(letrec
;         ([f$1 (lambda ()
;                 (begin
;                   (set! fv0 rax)
;                   (set! rax (+ rax rax))
;                   (set! rax (+ rax fv0))
;                   (r15)))])
;         (begin
;           (set! rax 17)
;           (f$1))))

