#lang racket

(require "helper.rkt")
(provide finalize-locations)

(define finalize-locations
  (lambda (program)
    ($program program)))

(define $program
  (lambda (program)
    (match program
      [`(letrec ([,l* (lambda () ,b*)] ...) ,b)
        (let ([$lmd (lambda (l b) `(,l (lambda () ,{$body b {env:make}})))])
          `(letrec ,{map $lmd l* b*} ,{$body b {env:make}}))])))

(define $body
  (lambda (body env)
    (match body
      [`(locate ([,u* ,l*] ...) ,t)
        ($tail t (env:extend env (map cons u* l*)))])))

(define $tail
  (lambda (tail env)
    (match tail
      [`(if ,p ,t-1 ,t-2)
       `(if ,{$pred p env} ,{$tail t-1 env} ,{$tail t-2 env})]
      [`(begin ,e* ... ,t)
       `(begin ,@{map {curryr $effect env} e*} ,{$tail t env})]
      [`(,t)
       `(,{$triv t env})])))

(define $pred
  (lambda (pred env)
    (match pred
      [`(true)
       `(true)]
      [`(false)
       `(false)]
      [`(if ,p-1 ,p-2 ,p-3)
       `(if ,{$pred p-1 env} ,{$pred p-2 env} ,{$pred p-3 env})]
      [`(begin ,e* ... ,p)
       `(begin ,@{map {curryr $effect env} e*} ,{$pred p env})]
      [`(,o ,t-1 ,t-2)
       `(,o ,{$triv t-1 env} ,{$triv t-2 env})])))

(define $effect
  (lambda (effect env)
    (match effect
      [`(nop)
       `(nop)]
      [`(set! ,v (,o ,t-1 ,t-2))
       `(set! ,{$var v env} (,o ,{$triv t-1 env} ,{$triv t-2 env}))]
      [`(set! ,v ,t)
       `(set! ,{$var v env} ,{$triv t env})]
      [`(if ,p ,e-1 ,e-2)
       `(if ,{$pred p env} ,{$effect e-1 env} ,{$effect e-2 env})]
      [`(begin ,e* ... ,e)
       `(begin ,@{map {curryr $effect env} e*} ,{$effect e env})])))

(define $triv
  (lambda (triv env)
    (match triv
      [(? var?)
       ($var triv env)]
      [\else
       (begin triv)])))

(define $var
  (lambda (var env)
    (match var
      [(? uvar?)
       (env:uvar->loc env var)]
      [\else
       (begin var)])))

; ----- helper ----- ;
(define env:make
  (lambda ()
    (hash-env:make)))

(define env:extend
  (lambda (env [pairs '()])
    (hash-env:extend env pairs)))

(define env:uvar->loc
  (lambda (env uvar)
    (hash-env:search env uvar)))

; ----- test ----- ;
(define env-wrap
  (lambda (func . pairs)
    (lambda (x)
      (func x (env:extend (env:make) pairs)))))

;(test (env-wrap $var (cons 'main.0 'rbx))
;      'main.0 'rax 'fv0)
;(test (env-wrap $triv (cons 'main.0 'rbx))
;      'main.0 'rax 'fv0 '10 'main$0)
;(test (env-wrap $effect (cons 'main.0 'rbx))
;      '(nop) '(set! main.0 10) '(set! main.0 (+ main.0 10)) '(if (true) (set! main.0 10) (set! main.0 20))
;      '(begin (set! main.0 10) (set! main.0 20)))
;(test (env-wrap $pred (cons 'main.0 'rbx))
;      '(true) '(false) '(> main.0 10) '(if (true) (< 10 main.0) (> main.0 20))
;      '(begin (set! main.0 10) (< main.0 30)))
;(test (env-wrap $tail (cons 'main.0 'rbx))
;      '(main.0) '(if (true) (main.0) (main.0))
;      '(begin (set! main.0 10) (main.0)))
;(test (env-wrap $body)
;      '(locate () (rax)) '(locate ([main.0 rax]) (main.0)) 
;      '(locate ([main.0 rax] [main.1 rbx]) (begin (set! main.0 (+ main.0 main.1)) (main.1))))
;(test $program
;      '(letrec () (locate ([main.0 rax]) (main.0)))
;      '(letrec ([main$0 (lambda () (locate ([main.0 rax]) (main.0)))]) (locate ([main.0 rbx]) (main.0))))

