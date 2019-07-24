#lang racket

(require "helper.rkt")
(provide inline-basic-blocks)

(define inline-basic-blocks
  (lambda (program)
    (env:init!)
    ($program program)))

(define $program
  (lambda (program)
    (match program
      [`(letrec ([,l* (lambda () ,t*)] ...) ,t)
        (for-each env:extend! l* t*)
        (refs-build! t)
        (let ([t-il ($tail t #f)])
          `(letrec ,(env->bind*) ,t-il))])))

(define $tail
  (lambda (tail need-call)
    (match tail
      [`(if ,r ,t-c ,f-c)
       `(if ,r ,($tail t-c #t) ,($tail f-c #t))]
      [`(begin ,e* ... ,t)
        (for-each $effect e*)
       `(begin ,@e* ,($tail t #f))]
      [`(,t)
        (if (not (label? t))
          (begin tail)
          (let ([t-il (inline t)])
            (cond
              [(not need-call)
               (begin t-il)]
              [(label-jump? t-il)
               (begin t-il)]
              [\else 
               (begin (env:modify! t t-il) tail)])))])))

(define $effect
  (lambda (effect)
    (match effect
      [`(set! ,_ ,(? label? t))
        (let ([mrb (env:search t)])
          (set-Mrb-refs! mrb -1)
          (env:modify! t ($tail (Mrb-body mrb) #f)))]
      [\else
        (void)])))

; ----- helper ----- ;
(struct Mrb ([refs #:mutable] [body #:mutable]) #:transparent)

(define branch-env (void))
(define env:init!
  (lambda ()
    (set! branch-env (list-env:make))))

(define env:extend!
  (lambda (label body)
    (let ([mrb (Mrb 0 body)])
      (set! branch-env (list-env:extend branch-env (list (cons label mrb)))))))

(define env:search
  (lambda (label)
    (list-env:search branch-env label)))

(define env:modify!
  (lambda (label body)
    (let ([mrb (env:search label)])
      (set-Mrb-refs! mrb -1)
      (set-Mrb-body! mrb body))))

(define env->bind*
  (lambda ()
    (list-env:fold branch-env
      (lambda (label mrb res*)
        (if (deal-ref? mrb)
          (cons `(,label (lambda () ,(Mrb-body mrb))) res*)
          (begin res*))))))

(define refs-build!
  (lambda (stmt)
    (match stmt
      [`(begin ,e* ... ,t)
        (for-each refs-build! e*)
        (refs-build! t)]
      [`(if ,r ,t-c ,f-c)
        (refs-build! t-c)
        (refs-build! f-c)]
      [`(set! ,_ ,t)
        (refs-build! `(,t))]
      [`(,t)
        (when (label? t)
          (let ([mrb (env:search t)])
            (set-Mrb-refs! mrb (add1 (Mrb-refs mrb)))
            (when (mono-ref? mrb)
              (refs-build! (Mrb-body mrb)))))])))

(define inline
  (lambda (label)
    (let ([mrb (env:search label)])
      (match mrb
        [(Mrb refs body)
         (cond
           [(mono-ref? mrb)
            (set-Mrb-refs! mrb 0)
            ($tail body #f)]
           [(poly-ref? mrb)
            (set-Mrb-refs! mrb -1)
            (set-Mrb-body! mrb ($tail body #f))
           `(,label)]
           [(deal-ref? mrb)
           `(,label)])]))))

(define mono-ref?
  (lambda (mrb)
    (= (Mrb-refs mrb) 1)))

(define poly-ref?
  (lambda (mrb)
    (> (Mrb-refs mrb) 1)))

(define deal-ref?
  (lambda (mrb)
    (< (Mrb-refs mrb) 0)))

(define label-jump?
  (lambda (tail)
    (match tail
      [`(,(? label?)) #t]
      [\else #f])))

; ----- test ----- ;
;(test inline-basic-blocks
;      '(letrec ((l$111 (lambda () (begin (set! r8 3) (l$110))))
;                (l$110 (lambda () (begin (set! r9 10) (f$1))))
;                (f$1 (lambda () (l$109)))
;                (l$109 (lambda () (if (= r8 1) (l$107) (l$108))))
;                (l$108 (lambda () (if (> r9 1000) (l$107) (l$106))))
;                (l$107 (lambda () (begin (set! rax r9) (r15))))
;                (l$106 (lambda () (begin (set! r9 (* r9 2)) (l$105))))
;                (l$105 (lambda () (begin (set! rax r8) (l$104))))
;                (l$104 (lambda () (begin (set! rax (logand rax 1)) (l$103))))
;                (l$103 (lambda () (if (= rax 0) (l$102) (l$101))))
;                (l$102 (lambda () (begin (set! r9 (+ r9 1)) (l$101))))
;                (l$101 (lambda () (begin (set! r8 (sra r8 1)) (f$1)))))
;         (l$111)))

