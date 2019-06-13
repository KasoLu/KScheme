#lang racket

(require "helper.rkt")
(provide expose-frame-var)

(define expose-frame-var
  (lambda (program)
    (expose-program program)))

(define expose-program
  (lambda (program)
    (match program
      [`(letrec ([,label* (lambda () ,tail*)] ...) ,tail)
        (let 
          ([exp-lambda (lambda (l t) `(,l (lambda () ,t)))]
           [exp-tail* (map expose-tail tail*)])
         `(letrec ,(map exp-lambda label* exp-tail*) ,(expose-tail tail)))])))

(define expose-tail
  (lambda (tail)
    (match tail
      [`(begin ,effect* ... ,tail)
       `(begin ,@(map expose-effect effect*) ,(expose-tail tail))]
      [`(,triv)
       `(,(expose-triv triv))])))

(define expose-effect
  (lambda (effect)
    (match effect
      [`(set! ,var (,binop ,triv-1 ,triv-2))
       `(set! ,(expose-var var) (,binop ,(expose-triv triv-1) ,(expose-triv triv-2)))]
      [`(set! ,var ,triv)
       `(set! ,(expose-var var) ,(expose-triv triv))])))

(define expose-triv
  (lambda (triv)
    (match triv
      [(? var?)
       (expose-var triv)]
      [(? int?)
       (begin triv)]
      [(? label?)
       (begin triv)])))

(define expose-var
  (lambda (var)
    (match var
      [(? reg?)
       (begin var)]
      [(? frame-var?)
       (disp-opnd 'rbp (* 8 (frame-var->index var)))])))

; ----- test ----- ;
;(test expose-var
;      'rax 'fv1)
;(test expose-triv
;      '10 'main$0)
;(test expose-effect
;      '(set! fv1 10) '(set! fv2 (+ fv2 10)))
;(test expose-tail
;      '(begin (set! fv1 10) (fv2)) '(fv0))
;(test expose-program
;      '(letrec ([main$0 (lambda () (fv0))]) (fv1)))
;(test expose-frame-var
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

