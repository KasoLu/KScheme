#lang racket

(require "helper.rkt")
(provide debug-program)

(define debug-program
  (lambda (program)
    ($program program)))

(define $program
  (lambda (program)
    (match program
      [`(letrec ([,l* (lambda () ,b*)] ...) ,b)
        (let ([dbg-lmd (lambda (l b) `(,l (lambda () ,b)))])
         `(letrec 
            ,(map dbg-lmd l* (map $body b*))
            ,($body b)))])))

(define $body
  (lambda (body)
    (match body
      [`(locate ([,u* . ,l*] ...) ,t)
        (let ([dbg-bind* (lambda (u l) `(,u ,l))])
          `(let ,(map dbg-bind* u* (map $var l*)) ,($tail t)))])))

(define $tail
  (lambda (tail)
    (match tail
      [`(if ,p ,t-t ,t-f)
       `(if ,($pred p) ,($tail t-t) ,($tail t-f))]
      [`(begin ,e* ... ,t)
       `(begin ,@(map $effect e*) ,($tail t))]
      [`(,triv)
       `(,($triv triv))])))

(define $pred
  (lambda (pred)
    (match pred
      [`(true)
       `(true)]
      [`(false)
       `(false)]
      [`(if ,p-1 ,p-2 ,p-3)
       `(if ,($pred p-1) ,($pred p-2) ,($pred p-3))]
      [`(begin ,e* ... ,p)
       `(begin ,@(map $effect e*) ,($pred p))]
      [`(,r ,t-1 ,t-2)
       `(,r ,($triv t-1) ,($triv t-2))])))

(define $effect
  (lambda (effect)
    (match effect
      [`(nop)
       `(nop)]
      [`(set! ,var (,binop ,triv-1 ,triv-2))
       `(set-box! ,($var var) 
                   (int64-truncate (,binop ,($triv triv-1) ,($triv triv-2))))]
      [`(set! ,var ,triv)
       `(set-box! ,($var var) ,($triv triv))]
      [`(if ,p ,e-1 ,e-2)
       `(if ,($pred p) ,($effect e-1) ,($effect e-2))]
      [`(begin ,e* ... ,e)
       `(begin ,@(map $effect e*) ,($effect e))])))

(define $triv
  (lambda (triv)
    (match triv
      [(? var?)
      `(unbox ,($var triv))]
      [(? int?)
       (begin triv)]
      [(? label?)
       (begin triv)])))

(define $var
  (lambda (var)
    (match var
      [(? uvar?)
       (begin var)]
      [(? reg?)
       (begin var)]
      [(? fvar?)
      `(vector-ref stack (+ (/ (unbox rbp) 8) ,(fvar->index var)))])))

; ----- test ----- ;
;(test $program 
;      '(letrec () 
;         (locate ()
;                 (begin 
;                   (set! rax 5)
;                   (r15)))))
;(test $program
;      '(letrec ((double$0 (lambda () (locate () (begin (set! rax (+ rax rax)) (r15)))))) (locate () (begin (set! rax 10) (double$0)))))

