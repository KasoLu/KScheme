#lang racket

(require "helper.rkt")
(require "verify-scheme.rkt")
(require "generate-x86-64.rkt")
(require "test.rkt")

; ----- pass ----- ;
(define run-program
  (lambda (program)
    (match program
      [`(begin ,stmt* ...)
       `(begin ,@(map run-stmt stmt*) ((unbox r15)))])))

(define run-stmt
  (lambda (stmt)
    (match stmt
      [`(set! ,var-to (,binop ,var-from ,val))
       `(set-box! ,(run-var var-to) (,binop ,(run-val var-from) ,(run-val val)))]
      [`(set! ,var ,val)
       `(set-box! ,(run-var var) ,(run-val val))])))

(define run-val
  (lambda (val)
    (match val
      [(? int?)
       (begin val)]
      [(? var?)
      `(unbox ,(run-var val))])))

(define run-var
  (lambda (var) var))

; ----- run ----- ;
(define debug-passes
  (list
    verify-scheme
    run-program))

(define build-passes
  (list
    verify-scheme
    generate-x86-64))

(driver debug-passes build-passes tests)
