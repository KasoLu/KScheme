#lang racket

(require "helper.rkt")
(provide generate-x86-64)

(define generate-x86-64
  (lambda (program)
    (display boilerplate-start)
    (program->x86-64 program)
    (display boilerplate-end)))

(define program->x86-64
  (lambda (program)
    (match program
      [`(begin ,stmt* ...)
        (for-each (compose display stmt->x86-64) stmt*)])))

(define stmt->x86-64
  (lambda (stmt)
    (match stmt
      [`(set! ,var (,binop ,_ ,val))
        (format "  ~a ~a, ~a\n" (binop->x86-64 binop) (val->x86-64 val) (var->x86-64 var))]
      [`(set! ,var ,val)
        (format "  movq ~a, ~a\n" (val->x86-64 val) (var->x86-64 var))]
      )))

(define var->x86-64
  (lambda (var)
    (format "%~a" var)))

(define binop->x86-64
  (lambda (binop)
    (match binop
      ['+ "addq"]
      ['- "subq"]
      ['* "imulq"])))

(define int->x86-64
  (lambda (int)
    (format "$~a" int)))

(define val->x86-64
  (lambda (val)
    (match val
      [(? var?)
       (var->x86-64 val)]
      [(? int?)
       (int->x86-64 val)])))

(define boilerplate-start "  .globl _scheme_entry\n_scheme_entry:\n")
(define boilerplate-end   "  ret\n")

; ----- test ----- ;
;(test generate-x86-64
;      '(begin (set! rax 8) (set! rcx 3) (set! rax (- rax rcx))))
