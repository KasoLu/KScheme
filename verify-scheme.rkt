#lang racket

(require "helper.rkt")
(provide verify-scheme)

(define verify-scheme
  (lambda (program)
    (label-env-init!)
    (verify-program! program)
    (begin program)))

(define verify-program!
  (lambda (program)
    (match program
      [`(letrec ([,label* (lambda () ,tail*)] ...) ,tail)
        (for-each verify-label-dup! label*)
        (for-each verify-tail (cons tail tail*))]
      [else
        (error 'verify-program "'~a' isn't a program" program)])))

(define verify-tail
  (lambda (tail)
    (match tail
      [`(begin ,effect* ... ,tail)
        (for-each verify-effect effect*)
        (verify-tail tail)]
      [`(,triv)
        (verify-triv triv)
        (when (int? triv)
          (error 'verify-tail "'~a' must not be a int" triv))]
      [else
        (error 'verify-tail "'~a' isn't a tail" tail)])))

(define verify-effect
  (let ([k? (lambda (k) (and (int? k) (<= 0 k 63)))])
    (lambda (effect)
      (match effect
        [`(set! ,var (,binop ,triv-1 ,triv-2))
          (verify-var var)
          (verify-binop binop)
          (verify-triv triv-1)
          (verify-triv triv-2)
          (unless (eq? var triv-1)
            (error 'verify-effect "'~a' != '~a'" var triv-1))
          (when (label? triv-2)
            (error 'verify-effect "'~a' cannot serve as operand" triv-2))
          (when (eq? binop '*)
            (unless (reg? var)
              (error 'verify-effect "'~a' must be a register" var)))
          (when (eq? binop 'sra)
            (unless (k? triv-2)
              (error 'verify-effect "'~a' not in [0, 63]" triv-2)))
          (when (int32~64? triv-2)
            (error 'verify-effect "'~a' must be a int32" triv-2))]
        [`(set! ,var ,triv)
          (verify-var var)
          (verify-triv triv)
          (when (or (label? triv) (int32~64? triv))
            (unless (reg? var)
              (error 'verify-effect "'~a' must be a reg" var)))
          (when (and (frame-var? var) (frame-var? triv))
            (error 'verify-effect "'~a' and '~a' cannot be samed with frame-var" var triv))]
        [else
          (error 'verify-effect "'~a' isn't a effect" effect)]))))

(define verify-triv
  (lambda (triv)
    (match triv
      [(? var?)
       (verify-var triv)]
      [(? int?)
       (void)]
      [(? label?)
       (verify-label-ref triv)]
      [else
       (error 'verify-triv "'~a' isn't triv" triv)])))

(define verify-var
  (lambda (var)
    (unless (var? var)
      (error 'verify-var "'~a' isn't a var" var))))

(define verify-binop
  (lambda (binop)
    (unless (binop? binop)
      (error 'verify-binop "'~a' isn't a binop" binop))))

(define verify-label-dup!
  (lambda (label)
    (match (label->index label)
      [(? number? idx)
       (if (hash-env:search label-env idx)
         (error 'verify-label-dup! "'~a' is duplicates" label)
         (hash-env:modify! label-env idx label))]
      [else
       (error 'verify-label-dup! "'~a' isn't a label" label)])))

(define verify-label-ref
  (lambda (label)
    (match (label->index label)
      [(? number? idx)
       (let ([found (hash-env:search label-env idx)])
         (unless (and found (eq? label found))
           (error 'verify-label-ref! "'~a' isn't existed" label)))]
      [else
       (error 'verify-label-ref! "'~a' isn't a label" label)])))

; ----- data ----- ;
(define label-env
  (void))

(define label-env-init!
  (lambda ()
    (set! label-env (hash-env:make))))

; ----- test ----- ;
;(test verify-var 
;      1 'x8 'fv01 'rax 'fv11)
;(test (lambda (x) (label-env-init!) (verify-label-dup! 'main$11) (verify-triv x))
;      '1.0 'main0 'main$01 'main$11)
;(test (lambda (x) (label-env-init!) (verify-label-dup! 'main$0) (verify-effect x))
;      '(rax! rax 10) '(set! 10 10) '(set! rax abc) '(set! fv0 main$0)
;      `(set! fv0 ,(expt 2 32)) '(set! rax 10)
;      '(rax! rax (+ rax 10)) '(set! 10 (+ rax 10)) '(set! rax (/ rax 10)) 
;      '(set! rax (+ abc 10)) '(set! rax (+ rax abc)) '(set! rax (+ rbx 10))
;      '(set! fv0 (* 10 10)) '(set! rax (sra rax 64)) `(set! rax (+ rax ,(expt 2 32)))
;      '(set! rax (+ rax 10)) '(set! fv0 (- fv0 10)))
;(test verify-tail
;      '(set! (r15)) '(begin (rax) (r15)) '(begin (set! rax 10) (set! rbx 20))
;      '(begin (set! rax 10) (r15))
;      '(abc) '(r15))
;(test (lambda (x) (label-env-init!) (verify-program! x))
;      '1 '() '(begin () (r15)) '(letrec ([main$01 (lambda () (r15))]) (r15))
;      '(letrec ([main$0 (begin () (r15))]) (r15)) '(letrec ([main$0 (lambda (rax) (r15))]) (r15))
;      '(letrec ([main$0 (lambda () (abc))]) (r15)) 
;      '(letrec ([main$0 (lambda () (r15))] [func$0 (lambda () (r15))]) (r15))
;      '(letrec ([main$0 (lambda () (r15))]) (abc))
;      '(letrec ([main$0 (lambda () (r15))]) (main$1))
;      '(letrec ([main$0 (lambda () (r15))]) (main$0)))
;(test verify-scheme
;      '1 '() '(begin () (r15)) '(letrec ([main$01 (lambda () (r15))]) (r15))
;      '(letrec ([main$0 (begin () (r15))]) (r15)) '(letrec ([main$0 (lambda (rax) (r15))]) (r15))
;      '(letrec ([main$0 (lambda () (abc))]) (r15)) 
;      '(letrec ([main$0 (lambda () (r15))] [func$0 (lambda () (r15))]) (r15))
;      '(letrec ([main$0 (lambda () (r15))]) (abc))
;      '(letrec ([main$0 (lambda () (r15))]) (main$1))
;      '(letrec ([main$0 (lambda () (r15))]) (main$0)))
