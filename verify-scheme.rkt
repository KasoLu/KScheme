#lang racket

(require "helper.rkt")
(provide verify-scheme)

(define verify-scheme
  (lambda (program)
    (verify-program program)
    (begin program)))

(define verify-program
  (lambda (program)
    (match program
      [`(begin ,stmt ,stmt* ...)
        (for-each verify-stmt (cons stmt stmt*))]
      [else
        (error 'verify-program "'~a' isn't a program" program)])))

(define verify-stmt
  (lambda (stmt)
    (match stmt
      [`(set! ,(? verify-var var-to) (,(? verify-binop binop) ,(? verify-var var-from) ,val))
        (unless (or (int32? val) (var? val))
          (error 'verify-stmt "'~a' isn't a int32 or var" val))
        (unless (eq? var-to var-from)
          (error 'verify-stmt "'~a' != '~a'" var-to var-from))]
      [`(set! ,(? verify-var var) ,val)
        (unless (or (int64? val) (var? val))
          (error 'verify-stmt "'~a' isn't a int64 or var" val))]
      [else 
        (error 'verify-stmt "'~a' isn't a stmt" stmt)])))

(define verify-var
  (lambda (var)
    (unless (reg? var)
      (error 'verify-var "'~a' isn't a var" var))))

(define verify-binop
  (lambda (binop)
    (unless (binop? binop)
      (error 'verify-binop "'~a' isn't a binop" binop))))

; ----- test ----- ;
;(test verify-binop #:catch #t
;      '() '+ '- '* '/)

;(test verify-var #:catch #t
;      '() '10 'abc 'rax 'b8 'r8)

;(test verify-stmt #:catch #t
;      '() '(abc! rax 10) '(set! 10 10) `(set! rax ,(expt 2 64))
;      '(set! rax b8) '(set! rax (/ rax 10)) '(set! rax (+ 10 10))
;      `(set! rax (+ rax ,(expt 2 32))) `(set! rax (+ rax x8))
;      '(set! rax (+ rbx rax))
;      '(set! rax 10) '(set! rax rbx) '(set! rax (+ rax 10)) '(set! rax (+ rax rbx)))

;(test verify-program #:catch #t
;      '() '(set! (set! rax 10)) '(begin) '(begin (set! rax 10)))
