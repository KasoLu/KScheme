#lang racket

(require "helper.rkt")
(provide flatten-program)

(define flatten-program
  (lambda (program)
    ($program program)))

(define $program
  (lambda (program)
    (match program
      [`(letrec ([,l* (lambda () ,t*)] ...) ,t)
        (let ([env (env:make l*)])
          (let loop ([e env] [t* t*] [res* '()])
            (if (null? t*)
             `(code ,@($tail t env) ,@res*)
              (loop (env:next e) (cdr t*) `(,@res* ,(env:label e) ,@($tail (car t*) (env:next e)))))))])))

(define $tail
  (lambda (tail env)
    (match tail
      [`(begin ,e* ... ,t)
       `(,@e* ,@($tail t env))]
      [`(if ,r (,t-l) (,f-l))
        (cond
          [(not (env:curr-label? env t-l))
          `((if ,r (jump ,t-l)))]
          [(not (env:curr-label? env f-l))
          `((if (not ,r) (jump ,f-l)))])]
      [`(,t)
        (cond
          [(not (label? t))
          `((jump ,t))]
          [(not (env:curr-label? env t))
          `((jump ,t))]
          [\else
          `()])])))

; ----- helper ----- ;
(define env:make
  (lambda (label*) label*))

(define env:curr-label?
  (lambda (env label)
    (match env
      [(? null?)  (begin #f)]
      [(cons l _) (eq? label l)])))

(define env:label
  (lambda (env)
    (match env
      [(? null?)  (error 'env:label "env is empty")]
      [(cons l _) (begin l)])))

(define env:next
  (lambda (env)
    (match env
      [(? null?) `()]
      [(cons _ p) (begin p)])))

; ----- test ----- ;
;(test flatten-program
;      '(letrec ((f$1 (lambda () (if (= r8 1) (l$107) (l$108))))
;                (l$108 (lambda () (if (> r9 1000) (l$107) (l$106))))
;                (l$107 (lambda () (begin (set! rax r9) (r15))))
;                (l$106
;                  (lambda ()
;                    (begin
;                      (set! r9 (* r9 2))
;                      (begin
;                        (set! rax r8)
;                        (begin
;                          (set! rax (logand rax 1))
;                          (if (= rax 0) (l$102) (l$101)))))))
;                (l$102 (lambda () (begin (set! r9 (+ r9 1)) (l$101))))
;                (l$101 (lambda () (begin (set! r8 (sra r8 1)) (f$1)))))
;         (begin (set! r8 3) (begin (set! r9 10) (f$1)))))

