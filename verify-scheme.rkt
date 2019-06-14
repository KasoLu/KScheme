#lang racket

(require "helper.rkt")
(provide verify-scheme)

(define verify-scheme
  (lambda (program)
    ($program! program (env:make))
    (begin program)))

(define $program!
  (lambda (program env)
    (match program
      [`(letrec ([,l* (lambda () ,b*)] ...) ,b)
        (for-each (lambda (l) (check-label-dup! l env)) l*)
        (for-each (lambda (b) ($body b (env:extend env))) (cons b b*))]
      [\else
        (check-error 'program program)])))

(define $body
  (lambda (body env)
    (match body
      [`(locate ([,u* ,l*] ...) ,t)
        (let ([v-l* (map (curryr $loc env) l*)])
          (for-each (curryr check-uvar-dup! env) u* v-l*)
          ($tail t env))]
      [\else
        (check-error 'body body)])))

(define $tail
  (lambda (tail env)
    (match tail
      [`(begin ,e* ... ,t)
        (for-each (curryr $effect env) e*)
        ($tail t env)]
      [`(if ,p ,t-1 ,t-2)
        ($pred p env)
        ($tail t-1 env)
        ($tail t-2 env)]
      [`(,t)
        ($triv t env)
        (when (int? t)
          (error '$tail "'~a' must not be a int" t))]
      [\else
        (check-error 'tail tail)])))

(define $pred
  (lambda (pred env)
    (match pred
      [`(true)
        (void)]
      [`(false)
        (void)]
      [`(begin ,e* ... ,p)
        (for-each (curryr $effect env) e*)
        ($pred p env)]
      [`(if ,p-1 ,p-2 ,p-3)
        ($pred p-1 env)
        ($pred p-2 env)
        ($pred p-3 env)]
      [`(,o ,t-1 ,t-2)
        ($relop o env)
        ($triv t-1 env)
        ($triv t-2 env)
        (when (or (label? t-1) (label? t-2))
          (error '$pred "'~a'/'~a' cannot be a label" t-1 t-2))]
      [\else
        (check-error 'pred pred)])))

(define $effect
  (let ([k? (lambda (k) (and (int? k) (<= 0 k 63)))])
    (lambda (effect env)
      (match effect
        [`(nop)
          (void)]
        [`(set! ,v (,o ,t-1 ,t-2))
          ($var v env)
          ($binop o env)
          ($triv t-1 env)
          ($triv t-2 env)
          (let ([v~ (if (uvar? v) (env:uvar->loc env v) v)])
            (when (not (eq? v~ t-1))
              (error '$effect "'~a' != '~a'" v t-1))
            (when (label? t-2)
              (error '$effect "'~a' cannot serve as operand" t-2))
            (when (and (eq? o '*) (not (reg? v~))) 
              (error '$effect "'~a' must be a register" v))
            (when (and (eq? o 'sra) (not (k? t-2))) 
              (error '$effect "'~a' not in [0, 63]" t-2))
            (when (int32~64? t-2)
              (error '$effect "'~a' must be a int32" t-2)))]
        [`(set! ,v ,t)
          ($var v env)
          ($triv t env)
          (let ([v~ (if (uvar? v) (env:uvar->loc env v) v)])
            (when (and ((any? label? int32~64?) t) (not (reg? v~)))
              (error '$effect "'~a' must be a reg" v))
            (when (and (fvar? v~) (fvar? t))
              (error '$effect "'~a' and '~a' cannot be both fvar" v t)))]
        [`(if ,p ,e-1 ,e-2)
          ($pred p env)
          ($effect e-1 env)
          ($effect e-2 env)]
        [`(begin ,e* ... ,e)
          (for-each (curryr $effect env) (cons e e*))]
        [\else
          (check-error 'effect effect)]))))

(define $triv
  (lambda (triv env)
    (match triv
      [(? var?) 
       ($var triv env)]
      [(? int?)
       (void)]
      [(? label?)
       (check-label-ref triv env)]
      [\else
       (check-error 'triv triv)])))

(define $var
  (lambda (var env)
    (match var
      [(? uvar?)
       (check-uvar-ref var env)]
      [(? loc?)
       (void)]
      [\else
       (check-error 'var var)])))

(define $loc
  (lambda (loc env)
    (check 'loc loc? loc)))

(define $binop
  (lambda (binop env)
    (check 'binop binop? binop)))

(define $relop
  (lambda (relop env)
    (check 'relop relop? relop)))

; ----- helper ----- ;
(define check-label-dup!
  (lambda (label env)
    (check-dup! 'label env label->index label label)))

(define check-label-ref
  (lambda (label env)
    (check-ref 'label env label->index label 
      (lambda (found)
        (match found
          [(? (curry eq? label))
           (begin label)]
          [\else
           (begin #f)])))))

(define check-uvar-dup!
  (lambda (uvar loc env)
    (check-dup! 'uvar env uvar->index uvar (cons uvar loc))))

(define check-uvar-ref
  (lambda (uvar env)
    (check-ref 'uvar env uvar->index uvar 
      (lambda (found)
        (match found
          [(cons (? (curry eq? uvar)) loc)
           (begin loc)]
          [\else
           (begin #f)])))))

(define check-dup!
  (lambda (tag env x->idx x v)
    (match (x->idx x)
      [(and (? number?) (app (curry idx->key tag) key)) 
       (if (hash-env:search env key)
         (error 'check-dup! "'~a' is duplicates" x)
         (hash-env:modify! env key v))]
      [\else
       (error 'check-dup! "'~a' isn't a ~a" x tag)])))

(define check-ref
  (lambda (tag env x->idx x func)
    (match (x->idx x)
      [(and (? number?) (app (curry idx->key tag) key))
       (let ([res (func (hash-env:search env key))])
         (if (not res)
           (error 'check-ref "'~a' isn't existed" x)
           (begin res)))]
      [\else
       (error 'check-ref "'~a' isn't a ~a" x tag)])))

(define idx->key
  (lambda (tag idx)
    (symbol-format "~a$~a" tag idx)))

(define env:make
  (lambda ()
    (hash-env:make)))

(define env:extend
  (lambda (env [pairs '()])
    (hash-env:extend env pairs)))

(define env:uvar->loc
  (lambda (env uvar)
    (match (hash-env:search env (idx->key (uvar->index uvar)))
      [(cons uvar loc)
       (begin loc)])))

(define check
  (lambda (tag pred val)
    (when (not (pred val)) 
      (check-error tag val))))

(define check-error
  (lambda (tag val)
    (error (symbol-format "$~a" tag) "'~a' isn't a ~a" val tag)))

; ----- test ----- ;
(define env-wrap
  (lambda (func . pairs)
    (lambda (x)
      (func x (env:extend (env:make) pairs)))))

;(test (env-wrap $effect)
;      '() '(nob) '(nop rax 10) '(set! main.01 10) '(set! rdd 10) '(set! fv01 10)
;      `(set! rax ,(expt 2 63)) '(set! rax main$01) '(set! rax (/ rax 10)) '(fi (true) (nop) (nop))
;      '(begins (nop) (nop))
;      '(nop) '(set! rax 10) '(set! rax (+ rax 10)) '(if (true) (nop) (nop)) 
;      '(begin (nop)) '(begin (nop) (nop)))
;(test (env-wrap $pred)
;      '() '(trne) '(falss) '(+ 10 10) '(> abc rax) '(> rax abc) '(fi (true) (true) (true))
;      '(if (set) (true) (true)) '(if (true) (set) (true)) '(if (true) (true) (set)) 
;      '(begins (nop) (nop)) '(begin (true) (true)) '(begin (nop) (nop))
;      '(true) '(false) '(> rax rbx) '(if (true) (true) (true)) 
;      '(begin (true)) '(begin (nop) (true)))
;(test (env-wrap $tail)
;      '() '(abc) '(fi (true) (rax) (rax)) 
;      '(if (nop) (rax) (rax)) '(if (true) () (rax)) '(if (true) (rax) ())
;      '(begins (nop) (rax)) '(begin (abc) (rax)) '(begin (nop) (abc))
;      '(rax) '(if (true) (rax) (fv0)) '(begin (nop) (rax)))
;(test (env-wrap $body)
;      '() '(set! () (rax)) '(locate ([rbx rax]) (rbx)) '(locate ([main.0 10]) (rax))
;      '(locate ([main.0 rax] [main.1 rbx]) 10) '(locate () (main.0))
;      '(locate ([main.0 rax]) (main.0)) '(locate ([main.0 rax] [main.1 rbx]) (main.1)))
;(test (env-wrap $program!)
;      '() '(set! () (locate () (rax))) 
;      '(letrec ([main$01 (lambda () (locate () (rax)))]) (locate () (rax)))
;      '(letrec ([main$0 (set! () (locate () (rax)))]) (locate () (rax)))
;      '(letrec ([main$0 (lambda () (set! rax 10))]) (locate () (rax)))
;      '(letrec ([main$0 (lambda () (locate () (rax)))] (set! rax 10)))
;      '(letrec ([main$0 (lambda () (locate ([main.0 rax] [main.0 rbx]) (main.0)))])
;         (locate () (rax)))
;      '(letrec () (locate () (rax)))
;      '(letrec ([main$0 (lambda () (locate () (rax)))]) (locate () (rax)))
;      '(letrec ([main$0 (lambda () (locate ([main.0 rax]) (main.0)))]) 
;         (locate ([main.0 rbx]) (main.0))))

