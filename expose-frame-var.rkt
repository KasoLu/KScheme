#lang racket

(require "helper.rkt")
(provide expose-frame-var)

(define expose-frame-var
  (lambda (program)
    ($program program)))

(define $program
  (lambda (program)
    (match program
      [`(letrec ([,l* (lambda () ,t*)] ...) ,t)
        (let ([exp-lmd (lambda (l t) `(,l (lambda () ,t)))]
              [exp-t* (map $tail t*)])
          `(letrec ,{map exp-lmd l* exp-t*} ,{$tail t}))])))

(define $tail
  (lambda (tail)
    (match tail
      [`(begin ,e* ... ,t)
       `(begin ,@{map $effect e*} ,{$tail t})]
      [`(if ,p ,t-1 ,t-2)
       `(if ,{$pred p} ,{$tail t-1} ,{$tail t-2})]
      [`(,t)
       `(,{$triv t})])))

(define $pred
  (lambda (pred)
    (match pred
      [`(true)
       `(true)]
      [`(false)
       `(false)]
      [`(if ,p-1 ,p-2 ,p-3)
       `(if ,{$pred p-1} ,{$pred p-2} ,{$pred p-3})]
      [`(begin ,e* ... ,p)
       `(begin ,@{map $effect e*} ,{$pred p})]
      [`(,o ,t-1 ,t-2)
       `(,o ,{$triv t-1} ,{$triv t-2})])))

(define $effect
  (lambda (effect)
    (match effect
      [`(nop)
       `(nop)]
      [`(set! ,l (,o ,t-1 ,t-2))
       `(set! ,{$loc l} (,o ,{$triv t-1} ,{$triv t-2}))]
      [`(set! ,l ,t)
       `(set! ,{$loc l} ,{$triv t})]
      [`(if ,p ,e-1 ,e-2)
       `(if ,{$pred p} ,{$effect e-1} ,{$effect e-2})]
      [`(begin ,e* ... ,e)
       `(begin ,@{map $effect e*} ,{$effect e})])))

(define $triv
  (lambda (triv)
    (match triv
      [(? loc?)
       ($loc triv)]
      [\else
       (begin triv)])))

(define $loc
  (lambda (loc)
    (match loc
      [(? fvar?)
       (disp-opnd 'rbp (* 8 (fvar->index loc)))]
      [\else
       (begin loc)])))

; ----- test ----- ;
;(test $loc
;      'rax 'fv0 'rbx 'fv1)
;(test $triv
;      'rax 'fv2 '10 'main$0)
;(test $effect
;      '(nop) '(set! fv3 fv4) '(set! fv5 (+ fv5 fv6))
;      '(if (true) (set! fv7 fv8) (set! fv9 fv10))
;      '(begin (set! rax 10) (set! rbx 20)))
;(test $pred
;      '(true) '(false) '(> fv0 fv1) '(if (true) (false) (true))
;      '(begin (set! rax 10) (true)))
;(test $tail
;      '(fv0) '(if (true) (fv1) (fv2))
;      '(begin (set! rax 10) (fv3)))
;(test $program
;      '(letrec ([main$0 (lambda () (fv0))]) (fv1)))

