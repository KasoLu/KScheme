#lang racket

(require "helper.rkt")
(provide expose-basic-blocks)

(define expose-basic-blocks
  (lambda (program)
    (env:init!)
    ($program! program)))

(define $program!
  (lambda (program)
    (match program
      [`(letrec ([,l* (lambda () ,t*)] ...) ,t)
        (for-each (lambda (l t) (env:extend! l ($tail! t))) l* t*)
        (let ([exp-t ($tail! t)])
          `(letrec ,{env->bind*} ,exp-t))])))

(define $tail!
  (lambda (tail)
    (match tail
      [`(begin ,e* ... ,t)
        ($effect! `(begin ,@e*) ($tail! t))]
      [`(if ,p ,t-1 ,t-2)
        (let 
          ([t-2-c ($tail! t-2)]
           [t-1-c ($tail! t-1)])
          ($pred! p t-1-c t-2-c))]
      [`(,t)
        (begin tail)])))

(define $pred!
  (lambda (pred true-cont false-cont)
    (match pred
      [`(true)
        (begin true-cont)]
      [`(false)
        (begin false-cont)]
      [`(if ,p-1 ,p-2 ,p-3)
        (let
          ([p-3-c ($pred! p-3 true-cont false-cont)]
           [p-2-c ($pred! p-2 true-cont false-cont)])
          ($pred! p-1 p-2-c p-3-c))]
      [`(begin ,e* ... ,p)
        ($effect! `(begin ,@e*) ($pred! p true-cont false-cont))]
      [`(,o ,t-1 ,t-2)
       `(,{env:label! `(if ,pred (,{env:label! true-cont}) (,{env:label! false-cont}))})])))

(define $effect!
  (lambda (effect cont)
    (match effect
      [`(nop)
        (begin cont)]
      [`(set! ,_ ,_)
       `(,{env:label! `(begin ,effect ,cont)})]
      [`(if ,p ,e-1 ,e-2)
        (let 
          ([e-2-c ($effect! e-2 cont)]
           [e-1-c ($effect! e-1 cont)])
          ($pred! p e-1-c e-2-c))]
      [`(begin ,e* ...)
        (foldr $effect! cont e*)])))

; ----- helper ----- ;
(define branch-env (void))

(define env:init!
  (lambda ()
    (set! branch-env (list-env:make))))

(define env:label!
  (lambda (body)
    (let ([lb (unique-label!)])
      (env:extend! lb body)
      (begin lb))))

(define env:extend!
  (lambda (label body)
    (let ([bd (cons label body)])
      (set! branch-env (list-env:extend branch-env (list bd))))))

(define env->bind*
  (lambda ()
    (list-env:map branch-env #:reverse #f
      (lambda (l b) `(,l (lambda () ,b))))))

(define unique-label!
  (let ([unique 100])
    (lambda ()
      (set! unique (add1 unique))
      (symbol-format "l$~a" unique))))

; ----- test ----- ;
(define test-wrap
  (lambda (func)
    (lambda (x) 
      (env:init!)
      (let ([val (func x)])
        (begin (pretty-display branch-env) val)))))

;(test (test-wrap (lambda (x) ($effect! x '(r15))))
;      '(begin
;         (nop)
;         (set! rax r8)
;         (set! r9 (* r9 2))
;         (if (= rax 0) (set! r9 (+ r9 1)) (nop))
;         (set! r8 (sra r8 1))
;         (set! rbx 10)))
;(test (test-wrap (lambda (x) ($pred! x '(rax) '(rbx))))
;      '(begin
;         (set! rax 10)
;         (if 
;           (if (= r8 10) (true) (> r9 10))
;           (if (= rbx 20) (false) (< r9 10))
;           (begin
;             (set! rbx 20)
;             (= rax 30)))))
;(test (test-wrap $tail!)
;      '(begin
;         (set! rax 10)
;         (set! rax (+ rax 20))
;         (if (= rax 10)
;           (rax)
;           (rbx)))
;      '(rax))
;(test expose-basic-blocks
;      '(letrec 
;         ([f$1 (lambda ()
;                 (if (if (= r8 1) (true) (> r9 1000))
;                   (begin (set! rax r9) (r15))
;                   (begin
;                     (set! r9 (* r9 2))
;                     (set! rax r8)
;                     (set! rax (logand rax 1))
;                     (if (= rax 0)
;                       (set! r9 (+ r9 1))
;                       (nop))
;                     (set! r8 (sra r8 1))
;                     (f$1))))])
;         (begin (set! r8 3) (set! r9 10) (f$1))))

